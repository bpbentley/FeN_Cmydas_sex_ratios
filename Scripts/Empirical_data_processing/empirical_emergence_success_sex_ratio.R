################################
### Output from embryogrowth ###
###### Empirical nest data #####
################################

library(ggpubr)
library(readxl)
library(openxlsx)
library(viridis)
library(lubridate)
library(dplyr)

season_pal <- c("#007191","#62C9D3", "#F37A00", "#D41F10")

## Hatching success
s1<-read_xlsx(path = "Metadata/2019_2020_Nesting_Season_1.xlsx", sheet = 1, )
s1$Season<-"S1"
s1$DATA_OCORR<-convertToDate(s1$DATA_OCORR)
s1$N_NINHO<-paste0("S1_N", s1$N_NINHO)
s2<-read_xlsx(path = "Metadata/2020_2021_Nesting_Season_2.xlsx", sheet = 1)
s2$Season<-"S2"
s2$N_NINHO<-paste0("S2_N", s2$N_NINHO)
s3<-read_xlsx(path = "Metadata/2021_2022_Nesting_Season_3.xlsx", sheet = 1)
s3$Season<-"S3"
s3$N_NINHO<-paste0("S3_N", s3$N_NINHO)
s4<-read_xlsx(path = "Metadata/2022_2023_Nesting_Season_4.xlsx", sheet = 1)
s4$Season<-"S4"
s4$N_NINHO<-paste0("S4_N", s4$N_NINHO)
s4$DATA_OCORR<-convertToDate(s4$DATA_OCORR)

hs<-as.data.frame(rbind(cbind(s1$Season, s1$`TURTLE ID`, as.character(s1$DATA_OCORR), s1$`Hatching success`),
                        cbind(s2$Season, s2$`TURTLE ID`, as.character(s2$DATA_OCORR), s2$`Hatching success`),
                        cbind(s3$Season, s3$`TURTLE ID`, as.character(s3$DATA_OCORR), s3$`Hatching success`),
                        cbind(s4$Season, s4$`TURTLE ID`, as.character(s4$DATA_OCORR), s4$`Hatching success`)))
hs<-hs[!is.na(as.numeric(hs$V4)),]
hs$V4<-as.numeric(hs$V4)

hs <- hs %>%
  mutate(
    DOY = yday(V3),
    # assign season "year of December"
    season_start = ifelse(month(V3) == 12, year(V3), year(V3) - 1),
    season = paste0("Season ", season_start - min(season_start) + 1)
  ) %>%
  # keep only Dec 1 – Jun 30
  filter((month(V3) == 12 & mday(V3) >= 1) | month(V3) <= 6) %>%
  # shift so Dec 1 = 1
  mutate(
    DOY_shift = ifelse(month(V3) == 12, DOY - 334, DOY + 31)
  )

season_means <- hs %>%
  group_by(V1) %>%
  summarise(mean_V4 = mean(V4, na.rm = TRUE))

season_means$V1<-gsub("S","Season ", season_means$V1)
hs$V1<-gsub("S","Season ", hs$V1)

hs_bp_f<-ggscatter(
  hs,
  x = "DOY_shift",
  y = "V4",
  color = "V1",
  xlim = c(1,180),
  palette = season_pal,
  size = 3
) +
  scale_x_continuous(
    breaks = c(1, 32, 63, 91, 122, 152, 183),
    labels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"),
    expand = c(0, 0)
  ) +
  labs(x = "Month", y = "Hatching Success (%)", color = "Season") +
  theme_classic() +
  # add horizontal lines for each season
  geom_hline(
    data = season_means,
    aes(yintercept = mean_V4, color = V1),
    linetype = "dashed",
    linewidth = 0.8
  )

ggsave("Plots/empirical_hs_all_seasons.svg", hs_bp_f, height = 4, width = 8)

ggdensity(data = hs, x = "V4", y = "..density..", fill = "V1")
hs_bp<-ggboxplot(data = hs, x = "V1", y = "V4", xlab = "Nesting Season",
          ylab = "Hatching Success (%)", fill = "V1", palette = get_palette("Set3", k = 4),
          legend = "none") + font("xlab", face = "bold") + font("ylab", face = "bold")
ggsave(filename = "Plots/Hatching_success_boxplot_by_season.png", hs_bp,
       height = 14, width = 16, units = "cm")

# HS by date for each season
s1$DATA_OCORR<-convertToDate(s1$DATA_OCORR)
s1$OVOS_TOT<-as.numeric(s1$OVOS_TOT)
s1_hs<-ggscatter(data = s1, x = "DATA_OCORR", y = "Hatching success", fill = get_palette("Set3", 4)[1],
          xlab = "Nesting Date", ylab = "Hatching Success (%)", shape = 21, size = 2, title = "A",
          ylim = c(0,100)) +
  font("xlab", face = "bold") + font("ylab", face = "bold")

s2$DATA_OCORR<-as.Date(s2$DATA_OCORR)
s2_hs<-ggscatter(data = s2, x = "DATA_OCORR", y = "Hatching success", fill = get_palette("Set3", 4)[2],
          xlab = "Nesting Date", ylab = "Hatching Success (%)", shape = 21, size = 2, title = "B",
          ylim = c(0,100)) +
  font("xlab", face = "bold") + font("ylab", face = "bold")

s3$DATA_OCORR<-as.Date(s3$DATA_OCORR)
s3$`Hatching success`<-as.numeric(s3$`Hatching success`)
s3_hs<-ggscatter(data = s3, x = "DATA_OCORR", y = "Hatching success", fill = get_palette("Set3", 4)[3],
          xlab = "Nesting Date", ylab = "Hatching Success (%)", shape = 21, size = 2, title = "C",
          ylim = c(0,100)) +
  font("xlab", face = "bold") + font("ylab", face = "bold")

s4$DATA_OCORR<-convertToDate(s4$DATA_OCORR)
s4$`Hatching success`<-as.numeric(s4$`Hatching success`)
s4_hs<-ggscatter(data = s4, x = "DATA_OCORR", y = "Hatching success", fill = get_palette("Set3", 4)[4],
          xlab = "Nesting Date", ylab = "Hatching Success (%)", shape = 21, size = 2, title = "D",
          ylim = c(0,100)) +
  font("xlab", face = "bold") + font("ylab", face = "bold")


all_hs<-ggarrange(s1_hs, s2_hs, s3_hs, s4_hs,ncol = 2, nrow = 2, common.legend = T)

hs

ggsave(filename = "Plots/Hatching_success_per_season_date.png", all_hs,
       height = 18, width = 24, units = "cm")



## Sex ratio model
df<-read.csv(file="dataOut/")
df$Season<-gsub("_.*","",df$Series)
df<-df[order(df$Season),]
sr_all<-ggboxplot(data = df, x = "Season", y = "TSP.GrowthWeighted.sexratio.mean",
          xlab = "Nesting Season", ylab = paste0("Predicted Sex Ratio","\n","(Proportion Male)"),
          fill = "Season", palette = get_palette("Set3", 4), legend = "none") +
  font("xlab", face = "bold") + font("ylab", face = "bold")
ggsave(filename = "Plots/Sex_ratio_by_season.png", sr_all, height = 14, width = 18, units = "cm")

s1_sr<-df[df$Season == "S1",]
s1_sr<-merge(s1_sr, s1, by.x = "Series", by.y = "N_NINHO")
s1_scat<-ggscatter(data = s1_sr, x = "DATA_OCORR", y = "TSP.GrowthWeighted.sexratio.mean",
          xlab = "Nesting Date", ylab = paste0("Predicted Sex Ratio","\n","(Proportion Male)"),
          shape = 21, size = 3, fill = get_palette("Set3",4)[1], ylim = c(0,0.5), title = "(A)",
          xlim = c(as.Date("2019-12-26"), as.Date("2020-04-25"))) +
  font("xlab", face = "bold") + font("ylab", face = "bold") + 
  geom_hline(yintercept = mean(s1_sr$TSP.GrowthWeighted.STRNWeighted.sexratio.mean),
             col = get_palette("Set3",4)[1], linetype = "dashed", size = 1.5)

s2_sr<-df[df$Season == "S2",]
s2_sr<-merge(s2_sr, s2, by.x = "Series", by.y = "N_NINHO")
s2_scat<-ggscatter(data = s2_sr, x = "DATA_OCORR", y = "TSP.GrowthWeighted.sexratio.mean",
                   xlab = "Nesting Date", ylab = paste0("Predicted Sex Ratio","\n","(Proportion Male)"),
                   shape = 21, size = 3, fill = get_palette("Set3",4)[2], ylim = c(0,0.5), title = "(B)",
                   xlim = c(as.Date("2020-12-26"), as.Date("2021-04-25"))) +
  font("xlab", face = "bold") + font("ylab", face = "bold") + 
  geom_hline(yintercept = mean(s2_sr$TSP.GrowthWeighted.STRNWeighted.sexratio.mean),
             col = get_palette("Set3",4)[2], linetype = "dashed", size = 1.5)

s3_sr<-df[df$Season == "S3",]
s3_sr<-merge(s3_sr, s3, by.x = "Series", by.y = "N_NINHO")
s3_scat<-ggscatter(data = s3_sr, x = "DATA_OCORR", y = "TSP.GrowthWeighted.sexratio.mean",
                   xlab = "Nesting Date", ylab = paste0("Predicted Sex Ratio","\n","(Proportion Male)"),
                   shape = 21, size = 3, fill = get_palette("Set3",4)[3], ylim = c(0,0.5), title = "(C)",
                   xlim = c(as.Date("2021-12-26"), as.Date("2022-04-25"))) +
  font("xlab", face = "bold") + font("ylab", face = "bold") + 
  geom_hline(yintercept = mean(s3_sr$TSP.GrowthWeighted.STRNWeighted.sexratio.mean),
             col = get_palette("Set3",4)[3], linetype = "dashed", size = 1.5)

s4_sr<-df[df$Season == "S4",]
s4_sr<-merge(s4_sr, s4, by.x = "Series", by.y = "N_NINHO")
s4_scat<-ggscatter(data = s4_sr, x = "DATA_OCORR", y = "TSP.GrowthWeighted.sexratio.mean",
                   xlab = "Nesting Date", ylab = paste0("Predicted Sex Ratio","\n","(Proportion Male)"),
                   shape = 21, size = 3, fill = get_palette("Set3",4)[4], ylim = c(0,0.5), title = "(D)",
                   xlim = c(as.Date("2022-12-26"), as.Date("2023-04-25"))) +
  font("xlab", face = "bold") + font("ylab", face = "bold") + 
  geom_hline(yintercept = mean(s4_sr$TSP.GrowthWeighted.STRNWeighted.sexratio.mean),
             col = get_palette("Set3",4)[4], linetype = "dashed", size = 1.5)

all_sr<-as.data.frame(rbind(cbind(as.character(s1_sr$DATA_OCORR), s1_sr$TSP.GrowthWeighted.STRNWeighted.sexratio.mean, s1_sr$Season.x),
                            cbind(as.character(s2_sr$DATA_OCORR), s2_sr$TSP.GrowthWeighted.STRNWeighted.sexratio.mean, s2_sr$Season.x),
                            cbind(as.character(s3_sr$DATA_OCORR), s3_sr$TSP.GrowthWeighted.STRNWeighted.sexratio.mean, s3_sr$Season.x),
                            cbind(as.character(s4_sr$DATA_OCORR), s4_sr$TSP.GrowthWeighted.STRNWeighted.sexratio.mean, s4_sr$Season.x)))
all_sr$Date<-as.POSIXct(all_sr$V1)
all_sr$V2<-as.numeric(all_sr$V2)
ggscatter(all_sr, x = "Date", y = "V2")

# Prepare data
all_sr <- all_sr %>%
  mutate(
    Date = as.Date(Date), 
    # assign season based on Dec–Jun
    season_start = ifelse(month(Date) == 12, year(Date), year(Date) - 1),
    season = paste0("Season ", season_start - min(season_start) + 1)
  ) %>%
  # keep only Dec 1 – Jun 30
  filter((month(Date) == 12 & mday(Date) >= 1) | month(Date) <= 6) %>%
  # create shifted day-of-year for x-axis
  mutate(
    DOY_shift = ifelse(month(Date) == 12, yday(Date) - 334, yday(Date) + 31)
  )

# Calculate mean V2 per season
season_means <- all_sr %>%
  group_by(season) %>%
  summarise(mean_V2 = mean(V2, na.rm = TRUE))

# Plot
sr_full<-ggscatter(
  all_sr,
  x = "DOY_shift",
  y = "V2",
  xlim = c(1,180),
  color = "season",
  palette = season_pal,
  size = 3
) +
  scale_x_continuous(
    breaks = c(1, 32, 63, 91, 122, 152, 183),
    labels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"),
    expand = c(0, 0)
  ) +
  labs(x = "Month", y = "Sex ratio (proporion male)", color = "Season") +
  theme_classic() +
  geom_hline(
    data = season_means,
    aes(yintercept = mean_V2, color = season),
    linetype = "dashed",
    size = 0.8
  )

ggsave(filename = "Plots/Sex_Ratio_per_season_date.svg", sr_full, height = 4, width = 8)

all_sr_sc<-ggarrange(s1_scat, s2_scat, s3_scat, s4_scat, nrow = 2, ncol = 2)
ggsave(filename = "Plots/Sex_Ratio_per_season_date.png", all_sr_sc,
       height = 18, width = 24, units = "cm")


## Track sex ratio by female through seasons 3 and 4
# S3
s3_filt<-s3[s3$TIPO_REG == "CD" &
              !is.na(s3$TEMP_LOGGER_ID) &
              s3$TEMP_LOGGER_ID != "NA",]
table(s3_filt$`TURTLE ID`)
#s3_filt$N_NINHO<-paste0("S3_N",s3_filt$N_NINHO)
s3_filt2<-subset(s3_filt, N_NINHO %in% df$Series)
s3_nestdates<-as.data.frame(cbind(s3_filt2$N_NINHO, as.character(s3_filt2$DATA_OCORR)))
s3_nestdates$V2<-as.Date(s3_nestdates$V2)

nest_list<-list()
for(t in 1:length(levels(factor(s3_filt2$`TURTLE ID`)))){
  df2<-s3_filt2[s3_filt2$`TURTLE ID` == levels(factor(s3_filt2$`TURTLE ID`))[t],]
  fnests<-df2$N_NINHO
  df3<-subset(df, Series %in% fnests)
  df3$nest<-as.numeric(gsub(".*N","",df3$Series))
  df3<-df3[order(df3$nest),]
  df3$Turtle_ID<-levels(factor(s3_filt2$`TURTLE ID`))[t]
  nest_list[[t]]<-df3
}

df4<-do.call(rbind, nest_list)
fturt<-as.data.frame(table(df4$Turtle_ID))
fturt<-fturt[fturt$Freq > 1,]
fturt<-as.character(fturt$Var1)
df4<-subset(df4, Turtle_ID %in% fturt)
df5<-merge(df4, s3_nestdates, by.x = "Series", by.y = "V1")

s3_plot<-ggline(df5, x = "V2", y = "TSP.GrowthWeighted.sexratio.mean", color = "Turtle_ID",
       xlab = "Nesting date", ylab = paste0("Predicted Sex Ratio","\n","(Proportion Male)"),
       title = "Nesting Season 3: 2021 - 2022", ylim = c(0,0.35)) +
  font("xlab", face = "bold") + font("ylab", face = "bold")



s3_tab<-as.data.frame(cbind(df5$Turtle_ID,
                            gsub("1951","2019",gsub("1952","2020",as.character(df5$V2))),
                            df5$TSP.GrowthWeighted.temperature.mean,
                            df5$TSP.GrowthWeighted.sexratio.mean))
colnames(s3_tab)<-c("F_Season","Date","CTE","Sex_Ratio_M")
write.csv(file = "dataOut/sex_ratio_byfemale_S3.csv", s3_tab, quote = F)

s3_comb<-merge(df5, s3_filt2, by.x = "Series", by.y = "N_NINHO")
s3_prod<-as.data.frame(cbind(s3_comb$Series, s3_comb$Turtle_ID, as.character(s3_comb$DATA_OCORR),
                             s3_comb$TimeWeighted.temperature.mean,s3_comb$TSP.GrowthWeighted.sexratio.mean,
                             s3_comb$OVOS_TOT, s3_comb$`Hatching success`))
colnames(s3_prod)<-c("Nest","Turtle_ID","Date","Mean_Temp","Sex_Ratio","Total_eggs","Hatching_success")
s3_prod$Dead<-as.numeric(s3_prod$Total_eggs) * (100 - as.numeric(s3_prod$Hatching_success))/100
s3_prod$Alive<-as.numeric(s3_prod$Total_eggs) - s3_prod$Dead
s3_prod$Male<-round(as.numeric(s3_prod$Alive) * as.numeric(s3_prod$Sex_Ratio))
s3_prod$Female<-as.numeric(s3_prod$Alive) - as.numeric(s3_prod$Male)

library(dplyr)

s3_prod_F <- s3_prod %>%
  mutate(
    Total_eggs = as.numeric(Total_eggs),
    Dead       = as.numeric(Dead),
    Alive      = as.numeric(Alive),
    Male       = as.numeric(Male),
    Female     = as.numeric(Female)
  ) %>%
  group_by(Turtle_ID) %>%
  summarise(
    total_eggs = sum(Total_eggs, na.rm = TRUE),
    dead       = sum(Dead, na.rm = TRUE),
    alive      = sum(Alive, na.rm = TRUE),
    male       = sum(Male, na.rm = TRUE),
    female     = sum(Female, na.rm = TRUE),
    n_nests    = n(),
    .groups = "drop"
  )

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

s3_long <- s3_prod_F %>%
  mutate(
    id_num   = as.numeric(str_remove(Turtle_ID, "F")),
    Turtle_ID = paste0("A", id_num)
  ) %>%
  arrange(id_num) %>%
  mutate(
    Turtle_ID = factor(Turtle_ID, levels = unique(Turtle_ID))
  ) %>%
  select(Turtle_ID, dead, male, female) %>%
  pivot_longer(
    cols = c(dead, male, female),
    names_to = "category",
    values_to = "count"
  ) %>%
  mutate(
    category = factor(category, levels = rev(c("dead", "male", "female")))
  )

s3_prod_plot<-ggplot(s3_long, aes(x = Turtle_ID, y = count, fill = category)) +
  geom_col() +
  labs(x = "Individual", y = "Number of eggs") +
  theme_classic() + rotate_x_text(90)
ggsave("Plots/S3_per_female_production.svg", plot = s3_prod_plot, height = 4, width = 8)


# S4
s4_filt<-s4[s4$TIPO_REG == "CD" &
              !is.na(s4$TEMP_LOGGER_ID) &
              s4$TEMP_LOGGER_ID != "NA",]
table(s4_filt$`TURTLE ID`)
#s4_filt$N_NINHO<-paste0("s4_N",s4_filt$N_NINHO)
s4_filt2<-subset(s4_filt, N_NINHO %in% df$Series)
s4_nestdates<-as.data.frame(cbind(s4_filt2$N_NINHO, as.character(s4_filt2$DATA_OCORR)))
s4_nestdates$V2<-as.Date(s4_nestdates$V2)

nest_list<-list()
for(t in 1:length(levels(factor(s4_filt2$`TURTLE ID`)))){
  df2<-s4_filt2[s4_filt2$`TURTLE ID` == levels(factor(s4_filt2$`TURTLE ID`))[t],]
  fnests<-df2$N_NINHO
  df3<-subset(df, Series %in% fnests)
  df3$nest<-as.numeric(gsub(".*N","",df3$Series))
  df3<-df3[order(df3$nest),]
  df3$Turtle_ID<-levels(factor(s4_filt2$`TURTLE ID`))[t]
  nest_list[[t]]<-df3
}

df4<-do.call(rbind, nest_list)
fturt<-as.data.frame(table(df4$Turtle_ID))
fturt<-fturt[fturt$Freq > 1,]
fturt<-as.character(fturt$Var1)
df4<-subset(df4, Turtle_ID %in% fturt)
df5<-merge(df4, s4_nestdates, by.x = "Series", by.y = "V1")

s4_plot<-ggline(df5, x = "V2", y = "TSP.GrowthWeighted.sexratio.mean", color = "Turtle_ID",
                xlab = "Nesting date", ylab = paste0("Predicted Sex Ratio","\n","(Proportion Male)"),
                title = "Nesting Season 4: 2022 - 2023", ylim = c(0,0.35)) +
  font("xlab", face = "bold") + font("ylab", face = "bold")

s4_tab<-as.data.frame(cbind(df5$Turtle_ID,
                            gsub("1952","2022",gsub("1953","2023",as.character(df5$V2))),
                            df5$TSP.GrowthWeighted.temperature.mean,
                            df5$TSP.GrowthWeighted.sexratio.mean))
colnames(s4_tab)<-c("F_Season","Date","CTE","Sex_Ratio_M")
write.csv(file = "dataOut/sex_ratio_byfemale_s4.csv", s4_tab, quote = F, row.names = F)

s4_comb<-merge(df5, s4_filt2, by.x = "Series", by.y = "N_NINHO")
s4_prod<-as.data.frame(cbind(s4_comb$Series, s4_comb$Turtle_ID, as.character(s4_comb$DATA_OCORR),
                             s4_comb$TimeWeighted.temperature.mean,s4_comb$TSP.GrowthWeighted.sexratio.mean,
                             s4_comb$OVOS_TOT, s4_comb$`Hatching success`))
colnames(s4_prod)<-c("Nest","Turtle_ID","Date","Mean_Temp","Sex_Ratio","Total_eggs","Hatching_success")
s4_prod$Dead<-as.numeric(s4_prod$Total_eggs) * (100 - as.numeric(s4_prod$Hatching_success))/100
s4_prod$Alive<-as.numeric(s4_prod$Total_eggs) - s4_prod$Dead
s4_prod$Male<-round(as.numeric(s4_prod$Alive) * as.numeric(s4_prod$Sex_Ratio))
s4_prod$Female<-as.numeric(s4_prod$Alive) - as.numeric(s4_prod$Male)

library(dplyr)

s4_prod_F <- s4_prod %>%
  mutate(
    Total_eggs = as.numeric(Total_eggs),
    Dead       = as.numeric(Dead),
    Alive      = as.numeric(Alive),
    Male       = as.numeric(Male),
    Female     = as.numeric(Female)
  ) %>%
  group_by(Turtle_ID) %>%
  summarise(
    total_eggs = sum(Total_eggs, na.rm = TRUE),
    dead       = sum(Dead, na.rm = TRUE),
    alive      = sum(Alive, na.rm = TRUE),
    male       = sum(Male, na.rm = TRUE),
    female     = sum(Female, na.rm = TRUE),
    n_nests    = n(),
    .groups = "drop"
  )

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

s4_long <- s4_prod_F %>%
  mutate(
    id_num   = as.numeric(str_remove(Turtle_ID, "F")),
    Turtle_ID = paste0("B", id_num)
  ) %>%
  arrange(id_num) %>%
  mutate(
    Turtle_ID = factor(Turtle_ID, levels = unique(Turtle_ID))
  ) %>%
  select(Turtle_ID, dead, male, female) %>%
  pivot_longer(
    cols = c(dead, male, female),
    names_to = "category",
    values_to = "count"
  ) %>%
  mutate(
    category = factor(category, levels = rev(c("dead", "male", "female")))
  )

s4_prod_plot<-ggplot(s4_long, aes(x = Turtle_ID, y = count, fill = category)) +
  geom_col() +
  labs(x = "Individual", y = "Number of eggs") +
  theme_classic() + rotate_x_text(90)
ggsave("Plots/S4_per_female_production.svg", plot = s4_prod_plot, height = 4, width = 8)

# Combined
comb_plots<-ggarrange(s3_plot, s4_plot, nrow = 2)
ggsave(filename = "Plots/Sex_ratio_S3_S4_repeated_nests.png", comb_plots,
       height = 21, width = 21, units = "cm")

#####
s4_tab<-read.csv(file="dataOut/sex_ratio_byfemale_s4.csv")
s3_tab<-read.csv(file="dataOut/sex_ratio_byfemale_s3.csv")
s3_tab$X<-NULL

s4_tab$Season<-"S4"
s3_tab$Season<-"S3"

s3_tab$ID<-as.numeric(gsub("F", "", s3_tab$F_Season))
s4_tab$ID<-as.numeric(gsub("F", "", s4_tab$F_Season))

s3_tab<-s3_tab[order(s3_tab$ID),]
s4_tab<-s4_tab[order(s4_tab$ID),]

p1<-ggboxplot(s3_tab, x = "F_Season", y = "Sex_Ratio_M", fill = "F_Season",
          palette = viridis(n = 20, option = "D"), legend = "none",
          title = "(A)", xlab = "Within season maternal ID",
          ylab = "Sex ratio (proportion male)", add = "jitter",
          add.params = list(size = 2, shape = 21)) +
  rotate_x_text(angle = 45) + font("xlab", face = "bold") + font("ylab", face = "bold")

p2<-ggboxplot(s4_tab, x = "F_Season", y = "Sex_Ratio_M", fill = "F_Season",
              palette = viridis(n = 19, option = "plasma"), legend = "none",
              title = "(C)", xlab = "Within season maternal ID",
              ylab = "Sex ratio (proportion male)", add = "jitter",
              add.params = list(size = 2, shape = 21)) +
  rotate_x_text(angle = 45) + font("xlab", face = "bold") + font("ylab", face = "bold")

p3<-ggarrange(p1, p2, ncol = 1)
p3
ggsave("Plots/per_female_sex_ratio_by_season_boxplot.svg", p3, height = 10,
       width = 8)

library(dplyr)

s3_tab %>%
  group_by(F_Season) %>%
  summarise(
    min_val = min(Sex_Ratio_M, na.rm = TRUE),
    max_val = max(Sex_Ratio_M, na.rm = TRUE),
    range_val = max_val - min_val,
    .groups = "drop"
  )

s4_tab %>%
  group_by(F_Season) %>%
  summarise(
    min_val = min(Sex_Ratio_M, na.rm = TRUE),
    max_val = max(Sex_Ratio_M, na.rm = TRUE),
    range_val = max_val - min_val,
    .groups = "drop"
  )

s3_tab$F_Season <- factor(s3_tab$F_Season, levels = paste0("F", sort(as.integer(gsub("F", "", unique(s3_tab$F_Season)))))
)

s3_tab$Date<-as.Date(s3_tab$Date)
p3<-ggline(s3_tab, x = "Date", y = "Sex_Ratio_M", col = "F_Season",
       palette = viridis(n = 20, option = "D"), legend = "none",
       ylab = "Sex ratio (proportion male)", xlab = "Nesting date",
       title = "(B)") +
  font("xlab", face = "bold") + font("ylab", face = "bold")

s4_tab$F_Season <- factor(s4_tab$F_Season, levels = paste0("F", sort(as.integer(gsub("F", "", unique(s4_tab$F_Season)))))
)

s4_tab$Date<-as.Date(s4_tab$Date)
p4<-ggline(s4_tab, x = "Date", y = "Sex_Ratio_M", col = "F_Season",
           palette = viridis(n = 19, option = "plasma"), legend = "none",
           ylab = "Sex ratio (proportion male)", xlab = "Nesting date",
           title = "(D)") +
  font("xlab", face = "bold") + font("ylab", face = "bold")

p5<-ggarrange(p1, p2, p3, p4, s3_prod_plot, s4_prod_plot, ncol = 2, nrow = 3)
ggsave("Plots/per_female_sex_ratio_by_season_boxplot.svg", p5,
       height = 10, width = 10)


