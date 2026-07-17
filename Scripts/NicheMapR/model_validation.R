#########################################
### RMSE & R^2 for model vs empirical ###
#########################################

library(ggpubr)
library(tidyverse)
source("scripts/to_posixct.R")

# ==== Empirical observations ====
loggers<-read.csv(file="Temperature_logger_data/Compiled_temperature_traces.csv")
loggers$datetime <- to_posixct(loggers$datetime)

# ==== Synthetic nests (metabolic heat-adjusted NicheMapR outputs) ====
load(file = "embryoGrowth_in/synthetic_nests.RData")
synth_list <- list()
for(q in 1:length(synthetic_nests)){
  df <- synthetic_nests[[q]]
  df$nestID <- names(synthetic_nests)[q]
  df <- df %>%
    mutate(
      year = as.integer(str_extract(nestID, "(?<=Y)\\d{4}")),
      doy  = as.integer(str_extract(nestID, "(?<=_)\\d+")),
      
      start_date = ymd(paste0(year, "-01-01")) + days(doy - 1),
      
      datetime = start_date + minutes(Time)
    )
  df2 <- df %>%
    select(datetime, Temperature, nestID)
  synth_list[[q]]<-df2
}

synth <- do.call(rbind, synth_list)

# ==== For each empirical nest, extract the model equivalent ====
emp_list<-list()
for(q in 1:length(levels(factor(loggers$S_N)))){
  nest <- levels(factor(loggers$S_N))[q]
  df <- loggers[loggers$S_N == nest,]
  df$date <- as.Date(df$datetime)
  df_summary <- df %>%
    group_by(date) %>%
    summarise(
      across(where(is.numeric), mean, na.rm = TRUE),
      .groups = "drop"
    )
  df_summary$Nest_ID <- df$S_N[1]
  df_summary$Season <- df$Season[1]
  
  year<-gsub("-.*","",df_summary$date)[1]
  yday<-yday(df_summary$date)[1]
  extract<-paste0("Y",year,"_",yday)
  model_sub <- synth[synth$nestID == extract,]
  model_sub$datetime <- as.Date(model_sub$datetime)
  
  df_merge <- merge(df_summary, model_sub, by.x = "date", by.y = "datetime")
  colnames(df_merge)<-c("Date", "Obs_Temp", "Nest_ID", "Season", "Mod_Temp", "Synth_ID")  
  
  emp_list[[q]]<-df_merge
}

comp_df <- do.call(rbind, emp_list)

# ==== Nest vs model validation ====

#----------------------------------------------------------
# Nest-level model performance statistics
#----------------------------------------------------------

nest_stats <- comp_df %>%
  group_by(Nest_ID, Season, Synth_ID) %>%
  group_modify(~{
    
    mod <- lm(Obs_Temp ~ Mod_Temp, data = .x)
    
    tibble(
      n = nrow(.x),
      
      start_date = min(.x$Date, na.rm = TRUE),
      end_date   = max(.x$Date, na.rm = TRUE),
      
      mean_obs = mean(.x$Obs_Temp, na.rm = TRUE),
      mean_mod = mean(.x$Mod_Temp, na.rm = TRUE),
      
      # Mean Error (Bias)
      bias = mean(.x$Mod_Temp - .x$Obs_Temp, na.rm = TRUE),
      
      # Mean Absolute Error
      mae = mean(abs(.x$Mod_Temp - .x$Obs_Temp), na.rm = TRUE),
      
      # Root Mean Squared Error
      rmse = sqrt(
        mean((.x$Mod_Temp - .x$Obs_Temp)^2, na.rm = TRUE)
      ),
      
      # Regression statistics
      r2 = summary(mod)$r.squared,
      slope = coef(mod)[2],
      intercept = coef(mod)[1]
    )
  }) %>%
  ungroup()

#----------------------------------------------------------
# Overall model performance across all nests
#----------------------------------------------------------

overall_mod <- lm(Obs_Temp ~ Mod_Temp, data = comp_df)

overall_stats <- tibble(
  n = nrow(comp_df),
  
  mean_obs = mean(comp_df$Obs_Temp, na.rm = TRUE),
  mean_mod = mean(comp_df$Mod_Temp, na.rm = TRUE),
  
  bias = mean(comp_df$Mod_Temp - comp_df$Obs_Temp, na.rm = TRUE),
  
  mae = mean(abs(comp_df$Mod_Temp - comp_df$Obs_Temp), na.rm = TRUE),
  
  rmse = sqrt(
    mean((comp_df$Mod_Temp - comp_df$Obs_Temp)^2, na.rm = TRUE)
  ),
  
  r2 = summary(overall_mod)$r.squared,
  slope = coef(overall_mod)[2],
  intercept = coef(overall_mod)[1]
)

print(overall_stats)

#----------------------------------------------------------
# Summary across nests
#----------------------------------------------------------

nest_summary <- nest_stats %>%
  summarise(
    n_nests = n(),
    
    median_r2 = median(r2, na.rm = TRUE),
    mean_r2   = mean(r2, na.rm = TRUE),
    
    median_rmse = median(rmse, na.rm = TRUE),
    mean_rmse   = mean(rmse, na.rm = TRUE),
    
    median_bias = median(bias, na.rm = TRUE),
    mean_bias   = mean(bias, na.rm = TRUE),
    
    median_mae = median(mae, na.rm = TRUE),
    mean_mae   = mean(mae, na.rm = TRUE)
  )

print(nest_summary)

#----------------------------------------------------------
# Diagnostic plots
#----------------------------------------------------------

# Observed vs modelled temperatures
ggplot(comp_df,
       aes(x = Obs_Temp, y = Mod_Temp)) +
  geom_point(alpha = 0.15) +
  geom_abline(slope = 1, intercept = 0,
              linetype = 2) +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Observed temperature (°C)",
    y = "Modelled temperature (°C)"
  )

# Distribution of nest RMSE values
ggplot(nest_stats,
       aes(x = rmse)) +
  geom_histogram(bins = 30) +
  theme_bw() +
  labs(
    x = "Nest RMSE (°C)",
    y = "Count"
  )

# Distribution of nest R² values
ggplot(nest_stats,
       aes(x = r2)) +
  geom_histogram(bins = 30) +
  theme_bw() +
  labs(
    x = expression(R^2),
    y = "Count"
  )

# ---------------------------------------------------------
# Season-level statistics
# ---------------------------------------------------------
season_stats <- nest_stats %>%
  group_by(Season) %>%
  summarise(
    n_nests = n(),
    mean_r2 = mean(r2),
    mean_rmse = mean(rmse),
    mean_bias = mean(bias)
  )

# ---------------------------------------------------------
# Additional plots
# ---------------------------------------------------------
season_pal <- c("#007191", "#62C9D3", "#F37A00", "#D41F10")

A<-gghistogram(nest_stats, x = "r2", y = "count", fill = "Season", bins = 50, palette = season_pal,
            xlab = paste0("R²"), ylab = "Nest Count", title = "(A)") +
  font("xlab", face = "bold") + font("ylab", face = "bold")
B<-gghistogram(nest_stats, x = "rmse", y = "count", fill = "Season", bins = 50, palette = season_pal,
            xlab = "RMSE", ylab = "Nest Count", title = "(B)") +
  font("xlab", face = "bold") + font("ylab", face = "bold")
C<-gghistogram(nest_stats, x = "mae", y = "count", fill = "Season", bins = 50, palette = season_pal,
            xlab = "MAE", ylab = "Nest Count", title = "(C)") +
  font("xlab", face = "bold") + font("ylab", face = "bold")
D<-gghistogram(nest_stats, x = "bias", y = "count", fill = "Season", bins = 50, palette = season_pal,
            xlab = "Bias", ylab = "Nest Count", title = "(D)") +
  font("xlab", face = "bold") + font("ylab", face = "bold")

suppFig <- ggarrange(A, B, C, D, common.legend = T)
suppFig

ggsave(filename = "Plots/model_validation_plots_by_season.png" , suppFig, height = 16, width = 20, units = "cm")

sfig2<-ggscatter(comp_df, x = "Obs_Temp", y = "Mod_Temp", fill = "Season", shape = 21, alpha = 0.5,
          xlab = "Observed Temperature (°C)", ylab = "Modeled Temperature (°C)",
          add = "reg.line", palette = season_pal) + geom_abline(slope = 1, intercept = 0,
                                          linetype = 2) +
  font("xlab", face = "bold") + font("ylab", face = "bold")
sfig2

ggsave(filename = "Plots/obs_vs_model_suppFig.png", sfig2, height = 12, width = 16, units = "cm")

#----------------------------------------------------------
# Save outputs if desired
#----------------------------------------------------------

# write.csv(nest_stats,
#           "Nest_Model_Performance.csv",
#           row.names = FALSE)

# write.csv(overall_stats,
#           "Overall_Model_Performance.csv",
#           row.names = FALSE)

