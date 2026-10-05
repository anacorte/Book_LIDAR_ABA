##################################################################################
# SCRIPT 1 - INITIAL PROCESSING
# BOOK: LIDAR APPLICATIONS FOR FOREST INVENTORY - AREA-BASED APPROACH
# AUTHORS: DALLA CORTE, ET AL. 2026
# FEDERAL UNIVERSITY OF PARANA - UFPR
# FORESTRY ENGINEERING PROGRAM
##################################################################################

##################################################################################
################### INITIAL PROCESSING - LIDAR BOOK ##############################
##################################################################################

########## RSTUDIO ENVIRONMENT SETUP ########## ----------------------------------

# Clearing the RStudio memory
rm(list = ls())

# Checking the directory
getwd()

# Setting the specific location on the computer
setwd("C:/Lidar_Book")

# Installing the required packages and loading them
# install.packages("lidR", repos = c("https://r-lidar.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("RCSF")
# install.packages("ggplot2")
# install.packages("rlang")
# install.packages("terra")

require("lidR")
require("RCSF")
require("terra")
require("ggplot2")
require("rlang")

########## INITIAL EXPLORATION OF THE POINT CLOUD ########## ---------------------

# Opening and inspecting the point cloud
las_data <- readLAS("Item1_Preliminary/Item_1_LiDAR_Data/lidar_data.las",
                    select = "xyzia")
plot(las_data, bg = "white", color = "Intensity", legend = FALSE)

# Inspecting the header of the data
las_head <- head(las_data@data)
View(las_head)

write.csv(las_head, "Item1_Preliminary/Item_1_Tables/head.csv")

# Information about the point cloud
las_check(las_data, print = TRUE)

# Histogram of the Z values
hist(las_data@data$Z,
     col  = height.colors(19),
     main = "Histogram (Z)",
     xlab = "Elevation range (m)",
     ylab = "Frequency")

# Spatial parameters
print(las_data)

########## POINT CLOUD PROCESSING AND MANIPULATION ########## --------------------

# FILTERING AND CLASSIFICATION OF THE GROUND POINTS
las_clean  <- filter_duplicates(las_data)
las_ground <- classify_ground(las_clean, csf())
plot(las_ground, bg = "white", color = "Classification")

# Plotting the elevation profile between two points of the cloud
plot_crossection <- function(las,
                             p1 = c(x1, y1),
                             p2 = c(x2, y2),
                             width = 4,
                             colour_by = NULL) {
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X, Z)) +
    geom_point(size = 0.5) +
    coord_equal() +
    theme_minimal()
  if (!quo_is_null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  return(p)
}

p1 <- c(558700, 7306200)
p2 <- c(558850, 7306500)

jpeg("Item1_Preliminary/Item_1_Images/crossection.jpeg",
     width = 770, height = 350, units = "px", res = 150, quality = 100)
print(plot_crossection(las_ground, p1 = p1, p2 = p2,
                       colour_by = factor(Classification)))
dev.off()

# DTM (Digital Terrain Model)
DTM_data <- rasterize_terrain(las_ground, res = 1, algorithm = tin())

jpeg("Item1_Preliminary/Item_1_Images/DTM_data.jpeg",
     width = 770, height = 990, units = "px", res = 150, quality = 100)
plot(DTM_data, main = "DTM (m)")
dev.off()

# Digital terrain model in 3D
plot_dtm3d(DTM_data, bg = "white")

# DSM (Digital Surface Model)
DSM_data <- rasterize_canopy(las_ground, res = 1, algorithm = p2r())

jpeg("Item1_Preliminary/Item_1_Images/DSM_data.jpeg",
     width = 770, height = 990, units = "px", res = 150, quality = 100)
plot(DSM_data, main = "DSM (m)")
dev.off()

# Normalization of the point cloud
las_norm <- normalize_height(las_ground, tin())
plot(las_norm, legend = TRUE, bg = "white")

# Saving the normalized cloud
writeLAS(las_norm, "Item2_Metrics/Item_2_LiDAR_Data/normalized_cloud.las")

# CHM (Canopy Height Model)
CHM_data <- rasterize_canopy(las_norm, res = 1, algorithm = p2r())

jpeg("Item1_Preliminary/Item_1_Images/CHM_data.jpeg",
     width = 770, height = 990, units = "px", res = 150, quality = 100)
plot(CHM_data, col = height.colors(30), main = "CHM (m)")
dev.off()
