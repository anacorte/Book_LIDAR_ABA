##################################################################################
# SCRIPT 2
# BOOK: LIDAR APPLICATIONS FOR FOREST INVENTORY - AREA-BASED APPROACH
# AUTHORS: DALLA CORTE, ET AL. 2026
# FEDERAL UNIVERSITY OF PARANA - UFPR
# FORESTRY ENGINEERING PROGRAM
##################################################################################

# Clearing the RStudio memory
rm(list = ls())

# Setting the directory
setwd("C:/Lidar_Book")

##################################################################################
############### GENERATION OF METRICS FOR FOREST INVENTORY #######################
##################################################################################

# Packages used in this section
# install.packages("lidR", repos = c("https://r-lidar.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("rgl")
# install.packages("sf")

require(lidR)
require(rgl)
require(sf)

# Opening and inspecting the point cloud
las_norm <- readLAS("Item2_Metrics/Item_2_LiDAR_Data/normalized_cloud.las")
las_check(las_norm)
# Load the normalized point cloud (Z = height above ground), add
# filter = "-drop_z_below 0" to readLAS() to remove points below 0 m

# Clipping the cloud to the area of interest - sample plot
# Option 1 - clipping with a shapefile
plot_shape <- st_read("Item2_Metrics/Item_2_Shapefile/sample_plot.shp")
st_crs(las_norm) <- st_crs(plot_shape)
plot_shape

plot_1 <- clip_roi(las_norm, plot_shape)
plot(plot_1, size = 15, bg = "white")
rgl::box3d(color = "black")
rgl::axes3d(color = "black", labels = TRUE)

# Option 2 - clipping with the plot centre point and a radius
plot_2 <- clip_circle(las_norm, 558800, 7306500, 13.82)
plot(plot_2, size = 15, bg = "white")
rgl::box3d(color = "black")
rgl::axes3d(color = "black", labels = TRUE)

# Maximum Z metric for the whole point cloud
max_z <- cloud_metrics(las_norm, ~max(Z))
max_z

# Default metrics for the sample plot
plot_metrics <- cloud_metrics(plot_2, .stdmetrics_z)
plot_metrics_df <- as.data.frame(plot_metrics)

# Saving the "cloud metrics" data table
write.csv(plot_metrics_df, "Item2_Metrics/Item_2_Tables/plot_metrics.csv",
          row.names = FALSE)
metrics_table <- read.csv("Item2_Metrics/Item_2_Tables/plot_metrics.csv")
View(metrics_table)
