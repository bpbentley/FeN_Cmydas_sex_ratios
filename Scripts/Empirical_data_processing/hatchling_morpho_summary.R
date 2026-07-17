####################################
### Hatchling morphology summary ###
####################################

library(readxl)
library(openxlsx)
library(ggpubr)
library(beepr)

s1_Nests<-excel_sheets("Metadata/2019_2020_Nesting_Season_1.xlsx")
s1_Nests<-s1_Nests[2:27]

HatchMorph<-list()
for(n in 1:length(s1_Nests)){
  df<-read_excel("Metadata/2019_2020_Nesting_Season_1.xlsx", sheet = s1_Nests[n])
  df2<-as.data.frame(cbind(df$SCL, df$SWC, df$Weight, df$`body depth`, df$`Sample ID`))
  colnames(df2)<-c("SCL","SCW","Weight","Body_Depth","Plate_Well")
  df2$Season<-"S1"
  df2$Nest<-s1_Nests[n]
  df2<-df2[!is.na(df2$SCL),]
  HatchMorph[[n]]<-df2
}

s2_Nests<-excel_sheets("Metadata/2020_2021_Nesting_Season_2.xlsx")
s2_Nests<-s2_Nests[2:79]

x<-length(HatchMorph)
for(n in 1:length(s2_Nests)){
  df<-read_excel("Metadata/2020_2021_Nesting_Season_2.xlsx", sheet = s2_Nests[n])
  df2<-as.data.frame(cbind(df$SCL, df$SWC, df$Weight, df$`body depth`, df$`Sample ID`))
  colnames(df2)<-c("SCL","SCW","Weight","Body_Depth","Plate_Well")
  df2$Season<-"S2"
  df2$Nest<-s2_Nests[n]
  df2<-df2[!is.na(df2$SCL),]
  HatchMorph[[n+x]]<-df2
}

s3_Nests<-excel_sheets("Metadata/2021_2022_Nesting_Season_3.xlsx")
s3_Nests<-s3_Nests[2:114]

y<-length(HatchMorph)
for(n in 1:length(s3_Nests)){
  df<-read_excel("Metadata/2021_2022_Nesting_Season_3.xlsx", sheet = s3_Nests[n])
  df2<-as.data.frame(cbind(df$SCL, df$SWC, df$Weight, df$`body depth`, df$`Sample ID`))
  colnames(df2)<-c("SCL","SCW","Weight","Body_Depth","Plate_Well")
  df2$SCL<-as.numeric(df2$SCL)
  df2$SCW<-as.numeric(df2$SCW)
  df2$Body_Depth<-as.numeric(df2$Body_Depth)
  df2$Weight<-as.numeric(df2$Weight)
  df2$Season<-"S3"
  df2$Nest<-s3_Nests[n]
  df2<-df2[!is.na(df2$SCL),]
  HatchMorph[[n+y]]<-df2
}

s4_Nests<-excel_sheets("Metadata/2022_2023_Nesting_Season_4.xlsx")
s4_Nests<-s4_Nests[2:107]

z<-length(HatchMorph)
for(n in 1:length(s4_Nests)){
  df<-read_excel("Metadata/2022_2023_Nesting_Season_4.xlsx", sheet = s4_Nests[n])
  df2<-as.data.frame(cbind(df$SCL, df$SWC, df$Weight, df$`body depth`, df$`Sample ID`))
  colnames(df2)<-c("SCL","SCW","Weight","Body_Depth","Plate_Well")
  df2$Season<-"S4"
  df2$Nest<-s4_Nests[n]
  df2<-df2[!is.na(df2$SCL),]
  HatchMorph[[n+z]]<-df2
}
AllHatch<-do.call(rbind, HatchMorph)
beep(sound = 2)

write.csv(file = "Morphology/All_hatchling_data.csv", AllHatch, row.names = F,
          quote = F)
AllHatch<-read.csv(file = "Morphology/All_hatchling_data.csv")

AllHatch$SCL<-as.numeric(AllHatch$SCL)
AllHatch$SCW<-as.numeric(AllHatch$SCW)
AllHatch$Weight<-as.numeric(AllHatch$Weight)
AllHatch$Body_Depth<-as.numeric(AllHatch$Body_Depth)
AllHatch$Unique<-paste0(AllHatch$Season,"_",AllHatch$Nest)

AllHatch<-AllHatch[!is.na(AllHatch$SCL),]

mean(AllHatch$SCL)
sd(AllHatch$SCL)

mean(AllHatch$SCW)
sd(AllHatch$SCW)

mean(AllHatch$Weight, na.rm = T)
sd(AllHatch$Weight, na.rm = T)

mean(AllHatch$Body_Depth, na.rm = T)
sd(AllHatch$Body_Depth, na.rm = T)

A<-ggdensity(AllHatch, x = "SCL", y = "..density..", add = "mean",
          fill = "Season", xlim = c(42,62), title = "(A)", add.params = list(color = c("red", "green", "blue", "purple"), linetype = 2)) +
  geom_vline(xintercept = mean(AllHatch$SCL, na.rm = T), linewidth = 1.5)
B<-ggdensity(AllHatch, x = "SCW", y = "..density..", add = "mean",
          fill = "Season", xlim = c(30, 45), title = "(B)", add.params = list(color = c("red", "green", "blue", "purple"), linetype = 2)) +
  geom_vline(xintercept = mean(AllHatch$SCW, na.rm = T), linewidth = 1.5)
C<-ggdensity(AllHatch, x = "Weight", y = "..density..", add = "mean",
          fill = "Season", xlim = c(16,32), title = "(C)", add.params = list(color = c("red", "green", "blue", "purple"), linetype = 2)) +
  geom_vline(xintercept = mean(AllHatch$Weight, na.rm = T), linewidth = 1.5)
D<-ggdensity(AllHatch, x = "Body_Depth", y = "..density..", add = "mean",
          fill = "Season", xlim = c(15, 25), title = "(D)", add.params = list(color = c("red", "green", "blue", "purple"), linetype = 2)) +
  geom_vline(xintercept = mean(AllHatch$Body_Depth, na.rm = T), linewidth = 1.5)
E<-ggarrange(A, B, C, D, common.legend = T)
ggsave(filename = "Morphology/hatch_morpho_density_per_season.png", E, height = 16, width = 24, units = "cm")

# Boxplots:
season_pal <- c("#001524","#15616d", "#ffecd1", "#ff7d00")
A<-ggboxplot(data = AllHatch, x = "Season", y = "SCL", fill = "Season",
             legend = "none", palette = season_pal,
             ylab = "Straight carapace length (SCL; cm)",
             title = "(A)") + font("xlab", face = "bold") + font("ylab", face = "bold")
B<-ggboxplot(data = AllHatch, x = "Season", y = "SCW", fill = "Season",
             legend = "none", palette = season_pal,
             ylab = "Straight carapace width (SCW; cm)",
             title = "(B)") + font("xlab", face = "bold") + font("ylab", face = "bold")
C<-ggboxplot(data = AllHatch, x = "Season", y = "Weight", fill = "Season",
             legend = "none", palette = season_pal,
             ylab = "Weight (g)",
             title = "(C)") + font("xlab", face = "bold") + font("ylab", face = "bold")
D<-ggboxplot(data = AllHatch, x = "Season", y = "Body_Depth", fill = "Season",
             legend = "none", palette = season_pal,
             ylab = "Body depth (g)",
             title = "(D)") + font("xlab", face = "bold") + font("ylab", face = "bold")
E<-ggarrange(A, B, C, D)

ggsave(filename = "Morphology/hatch_morpho_boxplot_per_season.png", E, height = 16, width = 24, units = "cm")

ggboxplot(data = AllHatch, x = "Season", y = "SCL", fill = "Season")
scl_anova<-aov(AllHatch$SCL ~ AllHatch$Season)
summary(scl_anova)
TukeyHSD(scl_anova)

ggboxplot(data = AllHatch, x = "Season", y = "SCW", fill = "Season")
scw_anova<-aov(AllHatch$SCW ~ AllHatch$Season)
summary(scw_anova)
TukeyHSD(scw_anova)

ggboxplot(data = AllHatch, x = "Season", y = "Body_Depth", fill = "Season")
AllHatch<-AllHatch[AllHatch$Body_Depth <= 30,]
bd_anova<-aov(AllHatch$Body_Depth ~ AllHatch$Season)
summary(bd_anova)
TukeyHSD(bd_anova)

ggboxplot(data = AllHatch, x = "Season", y = "Weight", fill = "Season")
weight_anova<-aov(AllHatch$Weight ~ AllHatch$Season)
summary(weight_anova)
TukeyHSD(weight_anova)

# SCL
aggregate(AllHatch$SCL ~ AllHatch$Season, FUN = "mean")
aggregate(AllHatch$SCL ~ AllHatch$Season, FUN = "sd")

# SCW
aggregate(AllHatch$SCW ~ AllHatch$Season, FUN = "mean")
aggregate(AllHatch$SCW ~ AllHatch$Season, FUN = "sd")

# Weight
aggregate(AllHatch$Weight ~ AllHatch$Season, FUN = "mean")
aggregate(AllHatch$Weight ~ AllHatch$Season, FUN = "sd")

# BD
aggregate(AllHatch$Body_Depth ~ AllHatch$Season, FUN = "mean")
aggregate(AllHatch$Body_Depth ~ AllHatch$Season, FUN = "sd")

# GLMM to account for nest ID
library(lme4)
library(lmerTest)
library(emmeans)

traits <- c("Weight", "SCL", "SCW", "Body_Depth")

models <- lapply(traits, function(trait) {
  formula <- as.formula(paste(trait, "~ Season + (1 | Unique)"))
  lmer(formula, data = AllHatch)
})

names(models) <- traits

anova_results <- lapply(models, anova)
anova_results

emm_list <- lapply(models, function(m) {
  emmeans(m, ~ Season)
})

pairwise_list <- lapply(emm_list, function(e) {
  pairs(e, adjust = "tukey")
})

library(dplyr)

results <- lapply(names(pairwise_list), function(trait) {
  as.data.frame(pairwise_list[[trait]]) %>%
    mutate(trait = trait)
}) %>%
  bind_rows()

results

write.csv(file="Morphology/GLMM_outputs.csv", results, quote = F, row.names = F)

# Plots:
library(tidyr)

df_long <- AllHatch %>%
  pivot_longer(cols = c(Weight, SCL, SCW, Body_Depth),
               names_to = "trait",
               values_to = "value")

ggplot(df_long, aes(x = Season, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2) +
  facet_wrap(~ trait, scales = "free_y") +
  theme_classic()

emm_all <- lapply(names(models), function(trait) {
  emm <- emmeans(models[[trait]], ~ Season)
  df <- as.data.frame(emm)
  df$trait <- trait
  df
}) %>% bind_rows()

ggplot(emm_all, aes(x = Season, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  facet_wrap(~ trait, scales = "free_y") +
  theme_classic() +
  labs(y = "Estimated mean")

library(emmeans)
library(multcomp)

cld_all <- lapply(names(models), function(trait) {
  emm <- emmeans(models[[trait]], ~ Season)
  df <- as.data.frame(cld(emm, Letters = letters))
  df$trait <- trait
  df
}) %>% dplyr::bind_rows()

cld_all$.group <- gsub(" ", "", cld_all$.group)

emm_all <- merge(emm_all,
                 cld_all[, c("Season", "trait", ".group")],
                 by = c("Season", "trait"))
ggplot(emm_all, aes(x = Season, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  geom_text(aes(label = .group), vjust = -0.8, size = 3) +
  facet_wrap(~ trait, scales = "free_y") +
  theme_classic() +
  labs(y = "Estimated mean")
