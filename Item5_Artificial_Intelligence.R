##################################################################################
# SCRIPT 5
# BOOK: LIDAR APPLICATIONS FOR FOREST INVENTORY - AREA-BASED APPROACH
# AUTHORS: DALLA CORTE, ET AL. 2026
# FEDERAL UNIVERSITY OF PARANA - UFPR
# FORESTRY ENGINEERING PROGRAM
##################################################################################

########## RSTUDIO ENVIRONMENT SETUP ########## ----------------------------------

# Clearing the RStudio memory
rm(list = ls())

# Indicating the directory
setwd("C:/Lidar_Book")

# Shared theme of the observed versus estimated plots
book_theme <- function() {
  require(ggplot2)
  theme_bw() +
    theme(text        = element_text(colour = "black", family = "Times", size = 12),
          axis.text.x = element_text(colour = "black", family = "Times", size = 12),
          axis.text.y = element_text(colour = "black", angle = 90,
                                     family = "Times", size = 12),
          axis.title  = element_text(colour = "black", family = "Times", size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

### RANDOM FOREST (RF) -----------------------------------------------------------

# Installing and loading the required packages
# install.packages("dplyr")
# install.packages("randomForest")
# install.packages("ggplot2")
# install.packages("caTools")
# install.packages("miscTools")

require(dplyr)
require(randomForest)
require(ggplot2)
require(caTools)
require(miscTools)

# Opening the "paired_data.csv" file
ai_data <- read.csv("Item5_Artificial_Intelligence/Item_5_Tables/paired_data.csv",
                    header = TRUE)
View(ai_data)

# Temporary file for the Random Forest
data_RF <- ai_data %>% dplyr::select(VT, zmax:zq95)
View(data_RF)

# Assigning the seed
set.seed(123)

# Splitting the database into training and test with 80% for training
split       <- sample.split(data_RF$VT, SplitRatio = 0.8)
train_RF    <- subset(data_RF, split == TRUE)
test_RF     <- subset(data_RF, split == FALSE)

# RF model
RF <- randomForest(x = train_RF[, -1], y = train_RF$VT,
                   ntree = 501, mtry = 7, importance = TRUE)
print(RF)

# Prediction and statistics
test_RF$pred_RF <- predict(RF, newdata = test_RF[, -1])
rsq_RF <- rSquared(test_RF$VT,
                   resid = test_RF$VT - test_RF$pred_RF)
rsq_RF
rmse_RF <- sqrt(mean((test_RF$VT - test_RF$pred_RF)^2))
rmse_RF

# Plot
lim_RF <- ceiling(max(test_RF$VT, test_RF$pred_RF, na.rm = TRUE) / 10) * 10

g1 <- ggplot(test_RF, aes(x = VT, y = pred_RF)) +
  labs(x = expression("Observed VT (m"^3*" ha"^-1*")"),
       y = expression("Estimated VT (m"^3*" ha"^-1*")")) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, lim_RF)) +
  scale_y_continuous(limits = c(0, lim_RF)) +
  annotate("text", x = 0, y = lim_RF, hjust = 0, vjust = 1, parse = TRUE,
           family = "Times", size = 4.5,
           label = sprintf('R^2 == "%.4f"', rsq_RF)) +
  annotate("text", x = 0, y = 0.92 * lim_RF, hjust = 0, vjust = 1, parse = TRUE,
           family = "Times", size = 4.5,
           label = sprintf('RMSE == "%.4f"', rmse_RF)) +
  book_theme()
print(g1)

# Saving the model
saveRDS(RF, "Item5_Artificial_Intelligence/Item_5_Models/RF.rds")

### SUPPORT VECTOR MACHINES (SVM) ------------------------------------------------

# Installing the required packages and loading them
# install.packages("e1071")
# install.packages("caTools")
# install.packages("dplyr")
# install.packages("miscTools")
# install.packages("ggplot2")

require(e1071)
require(caTools)
require(dplyr)
require(miscTools)
require(ggplot2)

# Opening the "paired_data.csv" file
ai_data <- read.csv("Item5_Artificial_Intelligence/Item_5_Tables/paired_data.csv",
                    header = TRUE)

# Assigning the file to data_SVM
data_SVM <- ai_data %>% dplyr::select(VT, zmax:zq95)
View(data_SVM)

# Assigning the seed
set.seed(123)

# Splitting the database into training and test with 80% for training
split      <- sample.split(data_SVM$VT, SplitRatio = 0.8)
train_SVM  <- subset(data_SVM, split == TRUE)
test_SVM   <- subset(data_SVM, split == FALSE)

# SVM model
SVM <- svm(VT ~ ., data = train_SVM,
           type = "eps-regression", kernel = "radial")
print(SVM)

# Prediction and statistics
test_SVM$pred_SVM <- predict(SVM, newdata = test_SVM[, -1])
rsq_SVM <- rSquared(test_SVM$VT,
                    resid = test_SVM$VT - test_SVM$pred_SVM)
rsq_SVM
rmse_SVM <- sqrt(mean((test_SVM$VT - test_SVM$pred_SVM)^2))
rmse_SVM

# Plot
lim_SVM <- ceiling(max(test_SVM$VT, test_SVM$pred_SVM, na.rm = TRUE) / 10) * 10

g1 <- ggplot(test_SVM, aes(x = VT, y = pred_SVM)) +
  labs(x = expression("Observed VT (m"^3*" ha"^-1*")"),
       y = expression("Estimated VT (m"^3*" ha"^-1*")")) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, lim_SVM)) +
  scale_y_continuous(limits = c(0, lim_SVM)) +
  annotate("text", x = 0, y = lim_SVM, hjust = 0, vjust = 1, parse = TRUE,
           family = "Times", size = 4.5,
           label = sprintf('R^2 == "%.4f"', rsq_SVM)) +
  annotate("text", x = 0, y = 0.92 * lim_SVM, hjust = 0, vjust = 1, parse = TRUE,
           family = "Times", size = 4.5,
           label = sprintf('RMSE == "%.4f"', rmse_SVM)) +
  book_theme()
print(g1)

# Saving the model
saveRDS(SVM, "Item5_Artificial_Intelligence/Item_5_Models/SVM.rds")

### ARTIFICIAL NEURAL NETWORKS (ANN) ---------------------------------------------

# Importing the packages
# install.packages("caret")
# install.packages("nnet")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("miscTools")
# install.packages("caTools")

require(caret)
require(nnet)
require(dplyr)
require(ggplot2)
require(miscTools)
require(caTools)

# Opening the "paired_data.csv" file
ai_data <- read.csv("Item5_Artificial_Intelligence/Item_5_Tables/paired_data.csv",
                    header = TRUE)
View(ai_data)

# Assigning the file to data_ANN
data_ANN <- ai_data %>% dplyr::select(VT, zmax:zq95)
View(data_ANN)

# Checking for missing data
sum(is.na(data_ANN))

# Defining the seed
set.seed(123)

# Splitting the database into training and test
split      <- sample.split(data_ANN$VT, SplitRatio = 0.8)
train_ANN  <- subset(data_ANN, split == TRUE)
test_ANN   <- subset(data_ANN, split == FALSE)

# Building the training model
nnet_grid <- expand.grid(size  = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))

ANN <- train(VT ~ ., data = train_ANN,
             method     = "nnet",
             trControl  = trainControl(method = "cv", number = 10),
             preProcess = c("scale", "center"),
             tuneGrid   = nnet_grid,
             linout     = TRUE,
             trace      = FALSE)
print(ANN)

# Prediction and statistics
test_ANN$pred_ANN <- predict(ANN, newdata = test_ANN[, -1])
rsq_ANN <- rSquared(test_ANN$VT,
                    resid = test_ANN$VT - test_ANN$pred_ANN)
rsq_ANN
rmse_ANN <- sqrt(mean((test_ANN$VT - test_ANN$pred_ANN)^2))
rmse_ANN

# Plot
lim_ANN <- ceiling(max(test_ANN$VT, test_ANN$pred_ANN, na.rm = TRUE) / 10) * 10

g1 <- ggplot(test_ANN, aes(x = VT, y = pred_ANN)) +
  labs(x = expression("Observed VT (m"^3*" ha"^-1*")"),
       y = expression("Estimated VT (m"^3*" ha"^-1*")")) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, lim_ANN)) +
  scale_y_continuous(limits = c(0, lim_ANN)) +
  annotate("text", x = 0, y = lim_ANN, hjust = 0, vjust = 1, parse = TRUE,
           family = "Times", size = 4.5,
           label = sprintf('R^2 == "%.4f"', rsq_ANN)) +
  annotate("text", x = 0, y = 0.92 * lim_ANN, hjust = 0, vjust = 1, parse = TRUE,
           family = "Times", size = 4.5,
           label = sprintf('RMSE == "%.4f"', rmse_ANN)) +
  book_theme()
print(g1)

# Saving the model
saveRDS(ANN, "Item5_Artificial_Intelligence/Item_5_Models/ANN.rds")

### k-NEAREST NEIGHBOUR (k-NN) ---------------------------------------------------

# Importing the packages
# install.packages("caret")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("miscTools")
# install.packages("caTools")

require(caret)
require(dplyr)
require(ggplot2)
require(miscTools)
require(caTools)

# Opening the "paired_data.csv" file
ai_data <- read.csv("Item5_Artificial_Intelligence/Item_5_Tables/paired_data.csv",
                    header = TRUE)

# Assigning the file to data_KNN
data_KNN <- ai_data %>% dplyr::select(VT, zmax:zq95)
View(data_KNN)

# Checking for missing data
sum(is.na(data_KNN))

# Defining the seed
set.seed(123)

# Splitting the database into training and test
split      <- sample.split(data_KNN$VT, SplitRatio = 0.8)
train_KNN  <- subset(data_KNN, split == TRUE)
test_KNN   <- subset(data_KNN, split == FALSE)

# Building the training model
ctrl <- trainControl(method = "cv", number = 10)

KNN <- train(VT ~ .,
             method     = "knn",
             preProcess = c("center", "scale"),
             tuneLength = 10,
             trControl  = ctrl,
             metric     = "RMSE",
             data       = train_KNN)
print(KNN)

# Prediction and statistics
test_KNN$pred_KNN <- predict(KNN, newdata = test_KNN[, -1])
rsq_KNN <- rSquared(test_KNN$VT,
                    resid = test_KNN$VT - test_KNN$pred_KNN)
rsq_KNN
rmse_KNN <- sqrt(mean((test_KNN$VT - test_KNN$pred_KNN)^2))
rmse_KNN

# Plot
lim_KNN <- ceiling(max(test_KNN$VT, test_KNN$pred_KNN, na.rm = TRUE) / 10) * 10

g1 <- ggplot(test_KNN, aes(x = VT, y = pred_KNN)) +
  labs(x = expression("Observed VT (m"^3*" ha"^-1*")"),
       y = expression("Estimated VT (m"^3*" ha"^-1*")")) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, lim_KNN)) +
  scale_y_continuous(limits = c(0, lim_KNN)) +
  annotate("text", x = 0, y = lim_KNN, hjust = 0, vjust = 1, parse = TRUE,
           family = "Times", size = 4.5,
           label = sprintf('R^2 == "%.4f"', rsq_KNN)) +
  annotate("text", x = 0, y = 0.92 * lim_KNN, hjust = 0, vjust = 1, parse = TRUE,
           family = "Times", size = 4.5,
           label = sprintf('RMSE == "%.4f"', rmse_KNN)) +
  book_theme()
print(g1)

# Saving the model
saveRDS(KNN, "Item5_Artificial_Intelligence/Item_5_Models/KNN.rds")

##################################################################################
############################### SPATIALIZATION ###################################
##################################################################################

# Installing and requiring the necessary packages
# install.packages("lidR", repos = c("https://r-lidar.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("terra")
# install.packages("sf")

require(lidR)
require(terra)
require(sf)
require(randomForest)
require(e1071)
require(caret)
require(nnet)

# Importing the las database and plotting it
las_norm <- readLAS("Item5_Artificial_Intelligence/Item_5_LiDAR_Data/normalized_cloud.las",
                    select = "xyz", filter = "-drop_z_below 0")
plot(las_norm, color = "Z", size = 3, bg = "white", legend = TRUE)

# Importing the shapefile to clip the area of interest
clip_area <- st_read("Item5_Artificial_Intelligence/Item_5_Shapefile/clip_area.shp")
st_crs(las_norm) <- st_crs(clip_area)
plot(st_geometry(clip_area))

# Clipping the area of interest
las_clip <- clip_roi(las_norm, clip_area)
plot(las_clip, bg = "white")

# Nominal plot area of the inventory design (600 m2 -> 24.49 m pixel);
# measured plot areas in PLOT_METRICS_DATA.csv range from 517 to 770 m2
plot_area_m2 <- 600
res_lidar    <- round(sqrt(plot_area_m2), 2)   # 24.49 m; pixel size must match plot size in ABA

# Generating the variables derived from the LiDAR data over the sample area
metrics_raster <- pixel_metrics(las_clip, .stdmetrics_z, res = res_lidar)
plot(metrics_raster)
names(metrics_raster)

# Models generated
RF  <- readRDS("Item5_Artificial_Intelligence/Item_5_Models/RF.rds")
SVM <- readRDS("Item5_Artificial_Intelligence/Item_5_Models/SVM.rds")
ANN <- readRDS("Item5_Artificial_Intelligence/Item_5_Models/ANN.rds")
KNN <- readRDS("Item5_Artificial_Intelligence/Item_5_Models/KNN.rds")

# Applying the models through the variables generated by the metrics
volume_RF  <- predict(metrics_raster, RF,  na.rm = TRUE)
volume_SVM <- predict(metrics_raster, SVM, na.rm = TRUE)
volume_ANN <- predict(metrics_raster, ANN, na.rm = TRUE)
volume_KNN <- predict(metrics_raster, KNN, na.rm = TRUE)

# Plot of the spatialization of the models
plot(volume_RF,  main = expression("Estimated VT - RF (m"^3*" ha"^-1*")"))
plot(volume_SVM, main = expression("Estimated VT - SVM (m"^3*" ha"^-1*")"))
plot(volume_ANN, main = expression("Estimated VT - ANN (m"^3*" ha"^-1*")"))
plot(volume_KNN, main = expression("Estimated VT - k-NN (m"^3*" ha"^-1*")"))

# Saving the output rasters
writeRaster(volume_RF,
            "Item5_Artificial_Intelligence/Item_5_Images/volume_RF.tif",
            overwrite = TRUE)
writeRaster(volume_SVM,
            "Item5_Artificial_Intelligence/Item_5_Images/volume_SVM.tif",
            overwrite = TRUE)
writeRaster(volume_ANN,
            "Item5_Artificial_Intelligence/Item_5_Images/volume_ANN.tif",
            overwrite = TRUE)
writeRaster(volume_KNN,
            "Item5_Artificial_Intelligence/Item_5_Images/volume_KNN.tif",
            overwrite = TRUE)

##################################################################################
######################## COMPARISON OF THE FOUR MODELS ###########################
##################################################################################

# R2 and RMSE of each model on the test plots
model_stats <- data.frame(
  Statistic = c("R2", "RMSE"),
  RF  = round(c(rsq_RF, rmse_RF),  2),
  SVM = round(c(rsq_SVM, rmse_SVM), 2),
  ANN = round(c(rsq_ANN, rmse_ANN), 2),
  KNN = round(c(rsq_KNN, rmse_KNN), 2))
print(model_stats)

write.csv(model_stats, "Item5_Artificial_Intelligence/Item_5_Tables/model_stats.csv",
          row.names = FALSE)

# Observed versus estimated panel for the four models
models <- c("(a) RF", "(b) SVM", "(c) ANN", "(d) KNN")
panel_data <- rbind(
  data.frame(model = models[1], obs = test_RF$VT,  pred = test_RF$pred_RF),
  data.frame(model = models[2], obs = test_SVM$VT, pred = test_SVM$pred_SVM),
  data.frame(model = models[3], obs = test_ANN$VT, pred = test_ANN$pred_ANN),
  data.frame(model = models[4], obs = test_KNN$VT, pred = test_KNN$pred_KNN))
panel_data$model <- factor(panel_data$model, levels = models)
panel_stats <- data.frame(
  model = factor(models, levels = models),
  r2    = c(rsq_RF, rsq_SVM, rsq_ANN, rsq_KNN),
  rmse  = c(rmse_RF, rmse_SVM, rmse_ANN, rmse_KNN))
panel_stats$r2_lab   <- sprintf('R^2 == "%.4f"', panel_stats$r2)
panel_stats$rmse_lab <- sprintf('RMSE == "%.4f"', panel_stats$rmse)

lim <- ceiling(max(panel_data$obs, panel_data$pred, na.rm = TRUE) / 10) * 10
g_panel <- ggplot(panel_data, aes(x = obs, y = pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, color = "red", linewidth = 0.8) +
  geom_text(data = panel_stats, aes(x = 0, y = lim, label = r2_lab),
            hjust = 0, vjust = 1, parse = TRUE, family = "Times",
            size = 4.5, inherit.aes = FALSE) +
  geom_text(data = panel_stats, aes(x = 0, y = 0.88 * lim, label = rmse_lab),
            hjust = 0, vjust = 1, parse = TRUE, family = "Times",
            size = 4.5, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(0, lim)) +
  scale_y_continuous(limits = c(0, lim)) +
  facet_wrap(~ model, ncol = 2) +
  labs(x = expression("Observed VT (m"^3*" ha"^-1*")"),
       y = expression("Estimated VT (m"^3*" ha"^-1*")")) +
  book_theme()
print(g_panel)

ggsave("Item5_Artificial_Intelligence/Item_5_Images/observed_vs_predicted_panel.jpeg",
       g_panel, width = 16, height = 16, units = "cm", dpi = 300)
