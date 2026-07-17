##################################################
### Nesting phenology and inter-nesting period ###
###### Shifts with climate warming analysis ######
##################################################

dat<-read.csv(file="embryoGrowth_out/FINAL_all_sex_ratio_emergence_success_models.csv")

# ==== Step 1. Extract nest-level summaries ====
library(tidyverse)
library(viridis)
library(ggpubr)

# Parse year from Series
nests <- dat %>%
  mutate(
    year = as.integer(str_extract(Series, "(?<=Y)\\d{4}"))
  )

# ==== Step 2. Extract baseline nesting distribution ====
# One row per yday (same across all years — verify this assumption)
nesting_curve <- nests %>%
  filter(year == 1980) %>%          # or any representative year
  select(yday, smooth_prop) %>%
  arrange(yday)

# Key parameters for shifting
baseline_mu    <- weighted.mean(nesting_curve$yday, nesting_curve$smooth_prop)
baseline_sigma <- sqrt(weighted.mean(
  (nesting_curve$yday - baseline_mu)^2, nesting_curve$smooth_prop
))

cat("Baseline peak (mu):", round(baseline_mu), "doy\n")
cat("Baseline width (sigma):", round(baseline_sigma), "days\n")

# ==== Step 3. Nesting weight generator ====
make_shifted_weights <- function(yday_vec, mu, sigma, shift = 0, scale = 1) {
  # shift: negative = earlier, positive = later (days)
  # scale: <1 = narrower, >1 = wider
  w <- dnorm(yday_vec, mean = mu + shift, sd = sigma * scale)
  tibble(yday = yday_vec, weight = w / sum(w))
}

# ==== Step 4. Scenario sweep ====
scenarios <- expand.grid(
  shift = seq(-360, 360, by = 1),
  scale = c(0.75, 1.0, 1.25, 1.5)
)

# For each scenario, reweight outcomes per year
N_nests     <- 334
N_hatchlings <- 100

run_scenario <- function(shift_val, scale_val) {
  weights <- make_shifted_weights(
    1:365, baseline_mu, baseline_sigma,
    shift = shift_val, scale = scale_val
  )
  
  nests %>%
    left_join(weights, by = "yday") %>%
    mutate(
      # Recompute expected nests from shifted distribution
      expected_nests_new = round(weight * N_nests),
      hatchlings_new     = expected_nests_new * N_hatchlings,
      alive_new          = round(hatchlings_new * Emergence_success / 100),
      males_new          = round(alive_new * Sex_ratio),
      females_new        = alive_new - males_new
    ) %>%
    group_by(year) %>%
    summarise(
      # Population-level rates weighted by nest counts
      pop_sex_ratio     = sum(males_new,   na.rm = TRUE) /
        sum(alive_new,   na.rm = TRUE),
      pop_emerg_success = sum(alive_new,   na.rm = TRUE) /
        sum(hatchlings_new, na.rm = TRUE) * 100,
      total_alive       = sum(alive_new,   na.rm = TRUE),
      total_males       = sum(males_new,   na.rm = TRUE),
      total_females     = sum(females_new, na.rm = TRUE),
      .groups = "drop"
    )
}

results <- scenarios %>%
  rowwise() %>%
  mutate(data = list(run_scenario(shift, scale))) %>%
  unnest(data)

# ==== Step 5 — Score viability and summarise ====
# Adjust thresholds to your study population biology
SR_MALE_MIN  <- 0.05   # population non-viable below this male fraction
SR_MALE_MAX  <- 0.10   # feminization threshold of concern
ES_MIN       <- 0.50   # emergence success floor

results_scored <- results %>%
  mutate(
    sr_too_feminized = pop_sex_ratio < SR_MALE_MIN,
    sr_feminized     = pop_sex_ratio < SR_MALE_MAX,
    es_failed        = pop_emerg_success < ES_MIN,
    viable           = !sr_too_feminized & !es_failed
  ) %>%
  group_by(shift, scale) %>%
  summarise(
    pct_viable          = mean(viable),
    pct_feminized       = mean(sr_feminized),      # below 10% males
    pct_sr_collapsed    = mean(sr_too_feminized),  # below 1% males
    pct_es_failed       = mean(es_failed),
    mean_sr             = mean(pop_sex_ratio),
    mean_es             = mean(pop_emerg_success),
    # Future window only
    pct_viable_future   = mean(viable[year %in% 2025:2104]),
    pct_feminized_future = mean(sr_feminized[year %in% 2025:2104]),
    .groups = "drop"
  )

# ==== Step 6 — Visualise ====
# --- Heatmap: overall viability ---
ggplot(results_scored, aes(x = shift, y = factor(scale), fill = pct_viable)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(pct_viable, accuracy = 1)),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent,
                       name = "% viable years") +
  labs(x = "Phenology shift (days)", y = "Width scale",
       title = "Population viability across 1980–2104") +
  theme_minimal(base_size = 13)

# --- Sex ratio trajectories for key shift scenarios ---
results %>%
  filter(scale == 1.0, shift %in% c(0, -20, -40, -60)) %>%
  ggplot(aes(x = year, y = pop_sex_ratio, color = factor(shift))) +
  geom_line(alpha = 0.3, linewidth = 0.4) +
  geom_smooth(se = FALSE, linewidth = 1.1, method = "loess", span = 0.15) +
  geom_hline(yintercept = c(SR_MALE_MIN, SR_MALE_MAX),
             linetype = "dashed", color = "grey40") +
  scale_color_viridis_d(name = "Shift (days)") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Population male fraction",
       title = "Sex ratio trajectory by phenology shift") +
  theme_minimal(base_size = 13)

# --- Near / mid / late century breakdown for best scenarios ---
results_scored %>%
  filter(scale == 1.0) %>%
  pivot_longer(starts_with("pct_viable_"), names_to = "period",
               names_prefix = "pct_viable_", values_to = "pct") %>%
  mutate(period = factor(period, levels = c("near", "mid", "late"))) %>%
  ggplot(aes(x = shift, y = pct, color = period)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d(name = "Period") +
  labs(x = "Phenology shift (days)", y = "% viable years",
       title = "Viability by century period (scale = 1.0)") +
  theme_minimal(base_size = 13)

# ==== Step 7. Re-analyze just the future ====
results_future <- results %>%
  filter(year >= 2030, year <= 2100)

historical_means <- results %>%
  filter(year >= 2005, year < 2025) %>%
  group_by(shift, scale) %>%
  # Use the no-shift, scale=1 scenario as "historical"
  filter(shift == 0, scale == 1.0) %>%
  summarise(
    hist_mean_sr = mean(pop_sex_ratio,     na.rm = TRUE),
    hist_mean_es = mean(pop_emerg_success, na.rm = TRUE),
    .groups = "drop"
  )

hist_sr <- historical_means$hist_mean_sr
hist_es <- historical_means$hist_mean_es

# Subset to a readable number of shift lines
shifts_to_plot <- seq(-360, 360, by = 20)
results_future2 <- results_future[results_future$scale == 1,]

# Hatch success
hs_future<-results_future2 %>%
  filter(shift %in% shifts_to_plot) %>%
  ggplot(aes(x = year, y = pop_sex_ratio,
             color = shift, group = factor(shift))) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 0.8) +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 0.7) +
  geom_hline(yintercept = hist_sr, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  scale_color_gradient2(
    low = viridis(3, option = "D")[1],
    mid = viridis(3, option = "D")[2],
    high = viridis(3, option = "D")[3],
    midpoint = 0,
    name = "Shift (days)"
  ) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #facet_wrap(~ paste0("Scale: ", scale), ncol = 2) +
  labs(
    x = "Year", y = paste0("Population sex ratio \n (Proportion male)"),
    title = "(C)"
    #title = "Projected population sex ratio under phenological shifts",
    #subtitle = "Dashed line = 5% male production threshold"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm")) + font("xlab", face = "bold") + font("ylab", face = "bold")
hs_future

# Emergence success
es_future<-results_future2 %>%
  filter(shift %in% shifts_to_plot) %>%
  ggplot(aes(x = year, y = pop_emerg_success,
             color = shift, group = factor(shift))) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 0.8) +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "red", linewidth = 0.7) +
  geom_hline(yintercept = hist_es, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  scale_color_gradient2(
    low = viridis(3, option = "D")[1],
    mid = viridis(3, option = "D")[2],
    high = viridis(3, option = "D")[3],
    midpoint = 0,
    name = "Shift (days)"
  ) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #facet_wrap(~ paste0("Scale: ", scale), ncol = 2) +
  labs(
    x = "Year", y = "Population emergence success \n (Percent emerged)",
    title = "(D)"
    #title = "Projected population sex ratio under phenological shifts",
    #subtitle = "Dashed line = 5% male production threshold"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm")) + font("xlab", face = "bold") + font("ylab", face = "bold")
es_future

# Combine and output
fin_plot<-ggarrange(hs_future, es_future, ncol = 2, common.legend = T) 
ggsave(filename = "Plots/FINAL_phenology_shifts.png", height = 12, width = 26, fin_plot, units = "cm")

sup_hs<-ggplot(results_future2, aes(year, pop_sex_ratio, color = shift)) +
       labs(x = "Year", y = "Population sex ratio (% male)", title = "(C)") +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = hist_sr, lty = 2) +
  scale_color_gradient2(
    low = viridis(3, option = "D")[1],
    mid = viridis(3, option = "D")[2],
    high = viridis(3, option = "D")[3],
    midpoint = 0,
    name = "Shift (days)") +
  theme_classic() + font("xlab", face = "bold") + font("ylab", face = "bold")

sup_es<-ggplot(results_future2, aes(year, pop_emerg_success, color = shift)) +
  labs(x = "Year", y = "Population emergence success (% viable)", title = "(D)") +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = hist_es, lty = 2) +
  scale_color_gradient2(
    low = viridis(3, option = "D")[1],
    mid = viridis(3, option = "D")[2],
    high = viridis(3, option = "D")[3],
    midpoint = 0,
    name = "Shift (days)") +
  theme_classic() + font("xlab", face = "bold") + font("ylab", face = "bold")
  
sup_fig<-ggarrange(sup_hs, sup_es, ncol = 2, common.legend = T)
ggsave(filename = "Plots/FINAL_phenology_shifts_SUP.png", height = 12, width = 26, sup_fig, units = "cm")

Y2040<-results_future2[results_future2$pop_sex_ratio >= hist_sr & results_future2$year == 2040,]
Y2060<-results_future2[results_future2$pop_sex_ratio >= hist_sr & results_future2$year == 2060,]
Y2080<-results_future2[results_future2$pop_sex_ratio >= hist_sr & results_future2$year == 2080,]
Y2100<-results_future2[results_future2$pop_sex_ratio >= hist_sr & results_future2$year == 2100,]


results_future %>%
  group_by(shift, scale) %>%
  summarise(
    mean_sr = mean(pop_sex_ratio),
    mean_es = mean(pop_emerg_success),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = shift, y = factor(scale), fill = mean_sr)) +
  geom_raster() +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "white", linewidth = 0.6) +
  scale_fill_gradient2(
    low      = "#d6604d",
    mid      = "white",
    high     = "#2166ac",
    midpoint = 0.05,
    labels   = scales::percent_format(accuracy = 1),
    name     = "Mean male\nfraction"
  ) +
  labs(
    x = "Phenology shift (days)", y = "Distribution width (scale)",
    title = "Mean population male fraction 2030–2100",
    subtitle = "Colour midpoint at 5% male production threshold"
  ) +
  theme_minimal(base_size = 12)

results_future %>%
  group_by(shift, scale) %>%
  summarise(mean_es = mean(pop_emerg_success), .groups = "drop") %>%
  ggplot(aes(x = shift, y = factor(scale), fill = mean_es)) +
  geom_raster() +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "white", linewidth = 0.6) +
  scale_fill_viridis_c(
    option = "mako", direction = -1,
    labels = scales::percent_format(scale = 1, accuracy = 1),
    name   = "Mean emergence\nsuccess (%)"
  ) +
  labs(
    x = "Phenology shift (days)", y = "Distribution width (scale)",
    title = "Mean emergence success 2030–2100"
  ) +
  theme_minimal(base_size = 12)

results_future %>%
  filter(year >= 2080) %>%
  group_by(shift, scale) %>%
  summarise(
    mean_sr = mean(pop_sex_ratio),
    lo_sr   = quantile(pop_sex_ratio, 0.1),
    hi_sr   = quantile(pop_sex_ratio, 0.9),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = shift, y = mean_sr,
             color = factor(scale), fill = factor(scale))) +
  geom_ribbon(aes(ymin = lo_sr, ymax = hi_sr), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 0.7) +
  geom_hline(yintercept = hist_sr, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  scale_color_viridis_d(name = "Width scale") +
  scale_fill_viridis_d(name  = "Width scale") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Phenology shift (days)", y = "Mean male fraction",
    title = "End-of-century sex ratio (2080–2100) by phenological shift",
    subtitle = "Ribbon = 10th–90th percentile across years. Dashed = 5% threshold"
  ) +
  theme_minimal(base_size = 12)

# === Step 8. Check shifts from the literature ====
# Days per year — pull these from your literature
# Positive = later, negative = earlier
pheno_rates <- tribble(
  ~species,          ~rate,   ~source,
  "Green turtle",    -0.6,    "Females nesting earlier",
  "Loggerhead",      -0.3,    "e.g. Mazaris et al.",
  "Leatherback",     -1.2,    "e.g. Neeman et al.",
  "No shift",         0.0,    "Baseline"
)

# Anchor: shift = 0 at 2030 (i.e. current distribution)
future_years <- 2030:2100

pheno_trajectories <- pheno_rates %>%
  rowwise() %>%
  mutate(
    data = list(
      tibble(
        year      = future_years,
        cum_shift = round(rate * (future_years - 2030))  # cumulative days shifted
      )
    )
  ) %>%
  unnest(data)

# For each year, extract the SR/ES from the matching scenario
# This requires interpolating from your results — join on nearest shift value
results_pheno <- pheno_trajectories %>%
  mutate(shift_rounded = cum_shift) %>%   # already integer shifts in results
  left_join(
    results_future %>% filter(scale == 1.0),  # fix scale = 1.0 for now
    by = c("shift_rounded" = "shift", "year")
  )

ggplot(results_pheno,
       aes(x = year, y = pop_sex_ratio,
           color = species, group = species)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = hist_sr, linetype = "dashed",
             color = "grey30", linewidth = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dotted",
             color = "red", linewidth = 0.6) +
  annotate("text", x = 2031, y = hist_sr + 0.003,
           label = "Historical mean SR", hjust = 0,
           size = 3.2, color = "grey30") +
  annotate("text", x = 2031, y = 0.05 + 0.003,
           label = "5% male threshold", hjust = 0,
           size = 3.2, color = "red") +
  scale_color_viridis_d(name = "Scenario") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year", y = "Population male fraction",
    title = "Projected sex ratio under literature-derived phenological shift rates",
    subtitle = "Scale = 1.0 (current nesting distribution width)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggplot(results_pheno,
       aes(x = year, y = pop_emerg_success,
           color = species, group = species)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = hist_es, linetype = "dashed",
             color = "grey30", linewidth = 0.7) +
  annotate("text", x = 2031, y = hist_es + 0.5,
           label = "Historical mean ES", hjust = 0,
           size = 3.2, color = "grey30") +
  scale_color_viridis_d(name = "Scenario") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1,
                                                    suffix = "%")) +
  labs(
    x = "Year", y = "Emergence success (%)",
    title = "Projected emergence success under literature-derived shift rates",
    subtitle = "Scale = 1.0 (current nesting distribution width)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# ==== Day-by-day comparison ====
library(dplyr)
library(lubridate)

dat_10yr <- dat %>%
  mutate(
    year = lubridate::year(Date),
    period_start = 1980 + 10 * floor((year - 1980) / 10),
    period_end = period_start + 9,
    period = paste0(period_start, "-", period_end)
  ) %>%
  filter(period_end <= 2099) %>%
  group_by(period, period_start, yday) %>%
  summarise(
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )  %>%
  mutate(
    plot_date = as.Date(yday - 1, origin = "2000-01-01")
  )

fig5A<-ggline(dat_10yr, x = "plot_date", y = "Sex_ratio", col = "period", plot_type = "l", size = 1.5,
              xlab = "Nesting date", ylab = paste0("Predicted mean sex ratio \n (Proportion male)"),
              title = "(A)", legend.title = "Decade") +
  scale_color_viridis_d(option = "C") +
  scale_x_date(date_labels = "%b") + font("xlab", face = "bold") + font("ylab", face = "bold")

fig5B<-ggline(dat_10yr, x = "plot_date", y = "Emergence_success", col = "period", plot_type = "l", size = 1.5,
              xlab = "Nesting date", ylab = paste0("Predicted mean \n emergence success \n (Percent emerged)"),
              title = "(B)", legend.title = "Decade") +
  scale_color_viridis_d(option = "C") +
  scale_x_date(date_labels = "%b") + font("xlab", face = "bold") + font("ylab", face = "bold")

AB<-ggarrange(fig5A, fig5B, common.legend = T, ncol = 2)
CD<-ggarrange(hs_future, es_future, common.legend = T, ncol = 2, legend = "top")
ABCD<-ggarrange(AB, CD, ncol = 1, nrow = 2)
ggsave(filename = "Plots/FINAL_Fig5.png", ABCD, width = 22, height = 22, units = "cm")

# ==== Look at days shifted to maintain current levels ====
sr_list<-list()
es_list<-list()
for(q in 2030:2100){
  df <- results_future2[results_future2$year == q,]
  df2 <- df[df$pop_sex_ratio >= hist_sr,]
  df4 <- as.data.frame(cbind(df2$shift, df2$year))
  colnames(df4)<-c("Shift", "Year")
  
  df3 <- df[df$pop_emerg_success >= hist_es,]
  df5 <- as.data.frame(cbind(df3$shift, df3$year))
  colnames(df5)<-c("Shift", "Year")
  
  y=q-2029
  sr_list[[y]]<-df4
  es_list[[y]]<-df5
}

sr_shift <- do.call(rbind, sr_list)
sr_shift$Shift<-as.integer(sr_shift$Shift)
fig5E<-ggline(sr_shift, x = "Shift", y = "Year", col = "Year", plot_type = "p",
       legend = "none", xlab = "Number of days offset from current",
       ylab = "", title = "(E)") + geom_vline(xintercept = 0, lty = 2) +
  font("xlab", face = "bold") + font("ylab", face = "bold") +
  scale_colour_viridis_c(option = "E")

es_shift <- do.call(rbind, es_list)
es_shift$Shift<-as.integer(es_shift$Shift)
fig5F<-ggline(es_shift, x = "Shift", y = "Year", col = "Year", plot_type = "p",
       legend = "none", xlab = "Number of days offset from current",
       ylab = "", title = "(F)") + geom_vline(xintercept = 0, lty = 2) +
  font("xlab", face = "bold") + font("ylab", face = "bold") +
  scale_colour_viridis_c(option = "E")
EF<-ggarrange(fig5E, fig5F, ncol = 2)

ABCDEF<-ggarrange(AB, CD, EF, ncol = 1, nrow = 3)
ggsave(filename = "Plots/FINAL_Fig5.png", ABCDEF, width = 22, height = 28, units = "cm")

with(sr_shift[sr_shift$Shift >= 0,],
aggregate(Shift ~ Year, FUN = "min"))
with(sr_shift[sr_shift$Shift <= 0,],
     aggregate(Shift ~ Year, FUN = "max"))

with(es_shift[es_shift$Shift >= 0,],
     aggregate(Shift ~ Year, FUN = "min"))
with(es_shift[es_shift$Shift <= 0,],
     aggregate(Shift ~ Year, FUN = "max"))
