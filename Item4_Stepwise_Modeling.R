##################################################################################
# SCRIPT 4
# BOOK: LIDAR APPLICATIONS FOR FOREST INVENTORY - AREA-BASED APPROACH
# AUTHORS: DALLA CORTE, ET AL. 2026
# FEDERAL UNIVERSITY OF PARANA - UFPR
# FORESTRY ENGINEERING PROGRAM
##################################################################################

##################################################################################
################################### MODELING #####################################
##################################################################################

########## RSTUDIO ENVIRONMENT SETUP ########## ----------------------------------

# Clearing the RStudio memory
rm(list = ls())

# Indicating the directory
setwd("C:/Lidar_Book")

# Shared theme of the observed versus estimated plots
book_theme <- function() {
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


# Installing the required packages and loading them
# install.packages("dplyr")
# install.packages("olsrr")
# install.packages("car")
# install.packages("lmtest")
# install.packages("ggplot2")
# install.packages("Metrics")

require(dplyr)
require(car)
require(olsrr)
require(lmtest)
require(ggplot2)
require(Metrics)

# Opening the "paired_data" file
model_data <- read.csv("Item4_Stepwise_Modeling/Item_4_Tables/paired_data.csv",
                       header = TRUE)
View(model_data)
names(model_data)

############################## STEPWISE ##########################################
#### Stepwise for the volume variable ####

# Separating the x and y columns to run the step function
step_vol <- model_data %>% dplyr::select(VT, zmax:zq95)
View(step_vol)

# Applying the regression and the step
full_vol <- lm(VT ~ ., data = step_vol)

# Forward, backward and both stepwise models
step_vol_forward  <- ols_step_forward_p(full_vol,  p_val = 0.05, details = TRUE)
step_vol_forward
step_vol_backward <- ols_step_backward_p(full_vol, p_val = 0.05, details = TRUE)
step_vol_backward
step_vol_both     <- ols_step_both_p(full_vol, p_enter = 0.05, p_remove = 0.1,
                                     details = TRUE)
step_vol_both

# Application of the model
lm_volume <- step_vol_both$model

# Coefficients read directly from the fitted model
b0_vol  <- round(coef(lm_volume)[1], 3)
b1_vol  <- round(coef(lm_volume)[2], 3)
var_vol <- names(coef(lm_volume))[2]

# Estimated values from the model
model_data$volume_est <- predict(lm_volume, newdata = model_data)

# R2, RMSE and BIAS
r2_vol   <- round(summary(lm_volume)$r.squared, 4)
rmse_vol <- round(rmse(model_data$VT, model_data$volume_est), 4)
bias_vol <- round(bias(model_data$VT, model_data$volume_est), 4)

# Plot with the statistics developed
plot(model_data$VT, model_data$volume_est,
     xlab = expression("Total volume (m"^3*" ha"^-1*")"),
     ylab = expression("Estimated volume (m"^3*" ha"^-1*")"))
abline(0, 1)
# Annotation positions relative to the plot extent
usr <- par("usr")
x_txt <- usr[1] + 0.03 * diff(usr[1:2])
y_txt <- usr[4] - c(0.06, 0.13, 0.20, 0.27) * diff(usr[3:4])
text(x_txt, y_txt[1], bquote(Y == .(b0_vol) + .(b1_vol) * .(as.name(var_vol))), adj = 0, cex = 1)
text(x_txt, y_txt[2], bquote(R^2 == .(r2_vol)), adj = 0, cex = 1)
text(x_txt, y_txt[3], bquote(RMSE == .(rmse_vol)), adj = 0, cex = 1)
text(x_txt, y_txt[4], bquote(BIAS == .(bias_vol)), adj = 0, cex = 1)

########## BASAL AREA MODELING ########## ----------------------------------------

# Separating the x and y columns to run the step function
step_ba <- model_data %>% dplyr::select(BA, zmax:zq95)
View(step_ba)

# Applying the regression and the step
full_ba <- lm(BA ~ ., data = step_ba)

# Forward, backward and both stepwise models
step_ba_forward  <- ols_step_forward_p(full_ba,  p_val = 0.05, details = TRUE)
step_ba_forward
step_ba_backward <- ols_step_backward_p(full_ba, p_val = 0.05, details = TRUE)
step_ba_backward 
step_ba_both     <- ols_step_both_p(full_ba, p_enter = 0.05, p_remove = 0.1,
                                    details = TRUE)
step_ba_both

# Application of the model
lm_ba <- step_ba_both$model

b0_ba  <- round(coef(lm_ba)[1], 3)
b1_ba  <- round(coef(lm_ba)[2], 3)
var_ba <- names(coef(lm_ba))[2]

model_data$basal_area_est <- predict(lm_ba, newdata = model_data)

r2_ba   <- round(summary(lm_ba)$r.squared, 4)
rmse_ba <- round(rmse(model_data$BA, model_data$basal_area_est), 4)
bias_ba <- round(bias(model_data$BA, model_data$basal_area_est), 4)

# Plot with the statistics developed
plot(model_data$BA, model_data$basal_area_est,
     xlab = expression("Basal area (m"^2*" ha"^-1*")"),
     ylab = expression("Estimated basal area (m"^2*" ha"^-1*")"))
abline(0, 1)
# Annotation positions relative to the plot extent
usr <- par("usr")
x_txt <- usr[1] + 0.03 * diff(usr[1:2])
y_txt <- usr[4] - c(0.06, 0.13, 0.20, 0.27) * diff(usr[3:4])
text(x_txt, y_txt[1], bquote(Y == .(b0_ba) + .(b1_ba) * .(as.name(var_ba))), adj = 0, cex = 1)
text(x_txt, y_txt[2], bquote(R^2 == .(r2_ba)), adj = 0, cex = 1)
text(x_txt, y_txt[3], bquote(RMSE == .(rmse_ba)), adj = 0, cex = 1)
text(x_txt, y_txt[4], bquote(BIAS == .(bias_ba)), adj = 0, cex = 1)

########## BIOMASS MODELING ########## -------------------------------------------

# Separating the x and y columns to run the step function
step_bio <- model_data %>% dplyr::select(AGB, zmax:zq95)
View(step_bio)

# Applying the regression and the step
full_bio <- lm(AGB ~ ., data = step_bio)

# Forward, backward and both stepwise models
step_bio_forward  <- ols_step_forward_p(full_bio,  p_val = 0.05, details = TRUE)
step_bio_forward
step_bio_backward <- ols_step_backward_p(full_bio, p_val = 0.05, details = TRUE)
step_bio_backward
step_bio_both     <- ols_step_both_p(full_bio, p_enter = 0.05, p_remove = 0.1,
                                     details = TRUE)
step_bio_both

# Application of the model
lm_biomass <- step_bio_both$model

b0_bio  <- round(coef(lm_biomass)[1], 3)
b1_bio  <- round(coef(lm_biomass)[2], 3)
var_bio <- names(coef(lm_biomass))[2]

model_data$biomass_est <- predict(lm_biomass, newdata = model_data)

r2_bio   <- round(summary(lm_biomass)$r.squared, 4)
rmse_bio <- round(rmse(model_data$AGB, model_data$biomass_est), 4)
bias_bio <- round(bias(model_data$AGB, model_data$biomass_est), 4)

# Plot with the statistics developed
plot(model_data$AGB, model_data$biomass_est,
     xlab = expression("Biomass (Mg ha"^-1*")"),
     ylab = expression("Estimated biomass (Mg ha"^-1*")"))
abline(0, 1)
# Annotation positions relative to the plot extent
usr <- par("usr")
x_txt <- usr[1] + 0.03 * diff(usr[1:2])
y_txt <- usr[4] - c(0.06, 0.13, 0.20, 0.27) * diff(usr[3:4])
text(x_txt, y_txt[1], bquote(Y == .(b0_bio) + .(b1_bio) * .(as.name(var_bio))), adj = 0, cex = 1)
text(x_txt, y_txt[2], bquote(R^2 == .(r2_bio)), adj = 0, cex = 1)
text(x_txt, y_txt[3], bquote(RMSE == .(rmse_bio)), adj = 0, cex = 1)
text(x_txt, y_txt[4], bquote(BIAS == .(bias_bio)), adj = 0, cex = 1)

##################################################################################
########################## STATISTICAL ASSUMPTIONS ###############################
##################################################################################

# Normality of the residuals by the Shapiro-Wilk test
# H0: normal distribution      (p > 0.05)
# H1: not normally distributed (p <= 0.05)
shapiro_volume  <- shapiro.test(lm_volume$residuals)
shapiro_ba      <- shapiro.test(lm_ba$residuals)
shapiro_biomass <- shapiro.test(lm_biomass$residuals)
print(shapiro_volume)
print(shapiro_ba)
print(shapiro_biomass)

# Outliers in the residuals - Z-SCORE
summary(rstandard(lm_volume))
summary(rstandard(lm_ba))
summary(rstandard(lm_biomass))

# Independence of the residuals (Durbin-Watson)
# H0: there is no correlation between the residuals - they are independent (p > 0.05)
# H1: there is correlation between the residuals - they are not independent (p <= 0.05)
car::durbinWatsonTest(lm_volume)
car::durbinWatsonTest(lm_ba)
car::durbinWatsonTest(lm_biomass)

# Homoscedasticity (Breusch-Pagan)
# H0: there is homoscedasticity    (p > 0.05)
# H1: there is no homoscedasticity (p <= 0.05)
bptest(lm_volume)
bptest(lm_ba)
bptest(lm_biomass)

# Multicollinearity
# by the Variance Inflation Factor (VIF), applicable to models with two or
# more predictors
vif_safe <- function(model) {
  if (length(attr(terms(model), "term.labels")) < 2) {
    message("VIF not applicable: the model has a single predictor.")
    return(invisible(NULL))
  }
  car::vif(model)
}

vif_safe(lm_volume)
vif_safe(lm_ba)
vif_safe(lm_biomass)

# Plotting the estimated value against the observed value

max_vol <- max(model_data$VT, model_data$volume_est, na.rm = TRUE)
lim_vol <- ceiling(max_vol / 10) * 10

g_volume <- ggplot(model_data, aes(x = volume_est, y = VT)) +
  labs(x = expression("Estimated volume (m"^3*" ha"^-1*")"),
       y = expression("Observed volume (m"^3*" ha"^-1*")")) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, lim_vol)) +
  scale_y_continuous(limits = c(0, lim_vol)) +
  book_theme()

print(g_volume)

max_ba <- max(model_data$BA, model_data$basal_area_est, na.rm = TRUE)
lim_ba <- ceiling(max_ba / 10) * 10

g_ba <- ggplot(model_data, aes(x = basal_area_est, y = BA)) +
  labs(x = expression("Estimated basal area (m"^2*" ha"^-1*")"),
       y = expression("Observed basal area (m"^2*" ha"^-1*")")) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, lim_ba)) +
  scale_y_continuous(limits = c(0, lim_ba)) +
  book_theme()
print(g_ba)

max_bio <- max(model_data$AGB, model_data$biomass_est, na.rm = TRUE)
lim_bio <- ceiling(max_bio / 10) * 10

g_biomass <- ggplot(model_data, aes(x = biomass_est, y = AGB)) +
  labs(x = expression("Estimated biomass (Mg ha"^-1*")"),
       y = expression("Observed biomass (Mg ha"^-1*")")) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE, color = "red", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, lim_bio)) +
  scale_y_continuous(limits = c(0, lim_bio)) +
  book_theme()
print(g_biomass)

##################################################################################
############################### SPATIALIZATION ###################################
##################################################################################

# install.packages("lidR", repos = c("https://r-lidar.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("terra")
# install.packages("sf")

require(lidR)
require(terra)
require(sf)

# Importing the las database and plotting it
las_norm <- readLAS("Item4_Stepwise_Modeling/Item_4_LiDAR_Data/normalized_cloud.las",
                    select = "xyz", filter = "-drop_z_below 0")
plot(las_norm, color = "Z", size = 3, bg = "white", legend = TRUE)

# Importing the shapefile to clip the area of interest
clip_area <- st_read("Item4_Stepwise_Modeling/Item_4_Shapefile/clip_area.shp")
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

# Prediction between the models and the raster files
volume_map  <- predict(metrics_raster, lm_volume,  na.rm = TRUE)
ba_map      <- predict(metrics_raster, lm_ba,      na.rm = TRUE)
biomass_map <- predict(metrics_raster, lm_biomass, na.rm = TRUE)

# Spatializing the information calculated for the area of interest
plot(volume_map,  main = expression("Spatialization - Volume (m"^3*" ha"^-1*")"))
plot(ba_map,      main = expression("Spatialization - Basal area (m"^2*" ha"^-1*")"))
plot(biomass_map, main = expression("Spatialization - Biomass (Mg ha"^-1*")"))

# Saving the output rasters
writeRaster(volume_map,
            "Item4_Stepwise_Modeling/Item_4_Images/volume_map.tif",
            overwrite = TRUE)
writeRaster(ba_map,
            "Item4_Stepwise_Modeling/Item_4_Images/basal_area_map.tif",
            overwrite = TRUE)
writeRaster(biomass_map,
            "Item4_Stepwise_Modeling/Item_4_Images/biomass_map.tif",
            overwrite = TRUE)
