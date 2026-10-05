##################################################################################
# SCRIPT 3 - EXPLORING THE METRICS FOR FOREST INVENTORY
# BOOK: LIDAR APPLICATIONS FOR FOREST INVENTORY - AREA-BASED APPROACH
# AUTHORS: DALLA CORTE, ET AL. 2026
# FEDERAL UNIVERSITY OF PARANA - UFPR
# FORESTRY ENGINEERING PROGRAM
##################################################################################

##################################################################################
############### EXPLORING THE METRICS FOR FOREST INVENTORY #######################
##################################################################################

########## RSTUDIO ENVIRONMENT SETUP ########## ----------------------------------

# Clearing the RStudio memory
rm(list = ls())

# Indicating the directory
setwd("C:/Lidar_Book")

# Installing and loading the packages
# install.packages("dplyr")
# install.packages("corrplot")

require(dplyr)
require(corrplot)

########## ANALYSIS OF THE SAMPLE PLOT METRICS ########## ------------------------

# Opening the table with the biometric data and the LiDAR metrics
plot_database <- read.csv("Item3_Correlation/Item_3_Tables/PLOT_METRICS_DATA.csv",
                          header = TRUE)
View(plot_database)
names(plot_database)

# Selecting the variables that can take part in the correlation calculation
cor_data <- plot_database %>%
  dplyr::select(Hmean, Hdom, BA, VT, VC, VP, AGB, zmax:zq95)
View(cor_data)

sapply(cor_data, is.numeric)

# Calculation of the correlations between the variables
correlation <- cor(cor_data)
View(correlation)

# Saving the correlation table
write.csv(correlation, "Item3_Correlation/Item_3_Tables/correlation.csv")

# Building the correlation matrix between the variables
res1 <- cor.mtest(cor_data, conf.level = 0.95)

jpeg("Item3_Correlation/Item_3_Figures/correlation.jpeg",
     width = 5800, height = 5000, units = "px", res = 300, quality = 100)
corrplot.mixed(correlation,
               lower = "number", upper = "ellipse", lower.col = "black",
               tl.cex = 1, number.font = 2, cl.ratio = 0.2, order = "hclust",
               p.mat = res1$p, sig.level = 0.05)
dev.off()

# Saving the paired data table
write.csv(cor_data, "Item3_Correlation/Item_3_Tables/paired_data.csv",
          row.names = FALSE)
