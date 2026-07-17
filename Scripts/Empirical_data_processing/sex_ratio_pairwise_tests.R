##################################################
### Formal tests of sex ratios between females ###
##################################################

library(dplyr)
library(tidyr)
library(ggpubr)
library(reshape2)
library(ggnewscale)

s4_tab<-read.csv(file="dataOut/sex_ratio_byfemale_s4.csv")
s3_tab<-read.csv(file="dataOut/sex_ratio_byfemale_s3.csv")
s3_tab$X<-NULL

s3_tab$Date<-as.Date(s3_tab$Date)
s4_tab$Date<-as.Date(s4_tab$Date)

ggline(s3_tab, x = "Date", y = "Sex_Ratio_M", col = "F_Season")
ggline(s4_tab, x = "Date", y = "Sex_Ratio_M", col = "F_Season")

s3aov<-aov(Sex_Ratio_M ~ F_Season, data = s3_tab)
summary(s3aov)
s4aov<-aov(Sex_Ratio_M ~ F_Season, data = s4_tab)
summary(s4aov)

# Function to create heatmap from TukeyHSD results
tukey_heatmap <- function(tukey_result, factor_name = NULL) {
  
  # Extract the results for the specified factor
  if (is.null(factor_name)) {
    # If only one factor, use the first one
    tk_data <- tukey_result[[1]]
  } else {
    tk_data <- tukey_result[[factor_name]]
  }
  
  # Extract comparison names and convert to character
  comparisons <- as.character(rownames(tk_data))
  
  # Parse comparison pairs - split only on first hyphen to handle group names with hyphens
  pairs <- strsplit(comparisons, "-")
  
  # Get unique groups and sort them numerically
  groups <- unique(c(sapply(pairs, function(x) trimws(x[1])), 
                     sapply(pairs, function(x) trimws(x[2]))))
  
  # Sort groups numerically (extract numbers and sort)
  groups <- groups[order(as.numeric(gsub("\\D", "", groups)))]
  n_groups <- length(groups)
  
  # Initialize matrices for differences and p-values
  diff_matrix <- matrix(NA, n_groups, n_groups, dimnames = list(groups, groups))
  pval_matrix <- matrix(NA, n_groups, n_groups, dimnames = list(groups, groups))
  
  # Fill matrices
  for (i in 1:nrow(tk_data)) {
    pair <- pairs[[i]]
    group1 <- trimws(pair[2])  # Second group in comparison
    group2 <- trimws(pair[1])  # First group in comparison
    
    # Find positions in sorted groups
    pos1 <- which(groups == group1)
    pos2 <- which(groups == group2)
    
    # Put differences in lower triangle (row > col)
    if (pos1 > pos2) {
      diff_matrix[group1, group2] <- tk_data[i, "diff"]
      pval_matrix[group2, group1] <- tk_data[i, "p adj"]
    } else {
      diff_matrix[group2, group1] <- tk_data[i, "diff"]
      pval_matrix[group1, group2] <- tk_data[i, "p adj"]
    }
  }
  
  # Create combined matrix for plotting
  combined_matrix <- diff_matrix
  combined_matrix[upper.tri(combined_matrix)] <- pval_matrix[upper.tri(pval_matrix)]
  diag(combined_matrix) <- 0
  
  # Prepare data for ggplot
  melted <- melt(combined_matrix, na.rm = FALSE)
  colnames(melted) <- c("Group1", "Group2", "value")
  
  # Add a variable to distinguish between diff and pval
  melted$type <- ifelse(as.numeric(melted$Group1) > as.numeric(melted$Group2), 
                        "Difference", "P-value")
  melted$type[melted$Group1 == melted$Group2] <- "Diagonal"
  
  # Normalize values for each triangle separately to use full color range
  melted$norm_value <- melted$value
  diff_vals <- melted$value[melted$type == "Difference"]
  pval_vals <- melted$value[melted$type == "P-value"]
  
  # Create the heatmap with ggnewscale for multiple color scales
  library(ggnewscale)
  
  # Split data by type
  diff_data <- subset(melted, type == "Difference")
  pval_data <- subset(melted, type == "P-value")
  diag_data <- subset(melted, type == "Diagonal")
  
  # Add significance labels for p-values
  pval_data$sig_label <- ifelse(pval_data$value < 0.05, "*", "")
  
  p <- ggplot() +
    # Lower triangle (differences) - Viridis
    geom_tile(data = diff_data, aes(x = Group2, y = Group1, fill = value), 
              color = "white", linewidth = 1) +
    scale_fill_viridis_c(name = "Difference", na.value = "grey90") +
    new_scale_fill() +
    # Upper triangle (p-values) - Magma
    geom_tile(data = pval_data, aes(x = Group2, y = Group1, fill = value), 
              color = "white", linewidth = 1) +
    scale_fill_viridis_c(option = "magma", name = "P-value", na.value = "grey90") +
    # Add asterisks for significant p-values
    geom_text(data = pval_data, aes(x = Group2, y = Group1, label = sig_label), 
              size = 8, color = "white") +
    # Diagonal - grey80
    geom_tile(data = diag_data, aes(x = Group2, y = Group1), 
              fill = "grey80", color = "white", linewidth = 1) +
    scale_y_discrete(limits = rev(levels(factor(melted$Group1)))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank(),
          panel.grid = element_blank()) +
    coord_equal() 
  return(p)
}


# Example usage:
# s3aov <- aov(Sex_Ratio_M ~ F_Season, data = s3_tab)
tk <- TukeyHSD(s3aov, "F_Season")
s3_mp <- tukey_heatmap(tk, "F_Season")
ggsave(filename = "Plots/pairwise_hatmap_sexratio_S3.svg",s3_mp,width = 8, height = 8)

tk2 <- TukeyHSD(s4aov, "F_Season")
s4_mp <- tukey_heatmap(tk2, "F_Season")
ggsave(filename = "Plots/pairwise_hatmap_sexratio_S4.svg",s4_mp,width = 8, height = 8)

# 
# Or if TukeyHSD has only one factor:
# tukey_heatmap(tk)

library(lme4)
library(performance)

model <- lmer(Sex_Ratio_M ~ 1 + (1|F_Season), data = s3_tab)

# Calculate ICC
icc(model)

vc <- as.data.frame(VarCorr(model))

within_var <- vc$vcov[vc$grp == "Residual"]
between_var <- vc$vcov[vc$grp == "F_Season"]

# Compare them
within_var / between_var  # >1 means more within-female variation
