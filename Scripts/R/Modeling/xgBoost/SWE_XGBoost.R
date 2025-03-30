##### Trying XGBoost for modeling maximum SWE (mSWE) #####

##### Loading packages, reading in data #####
library(pacman)
p_load(xgboost, caret, sf, terra, here, sfext, rsample, lime, parallel, fastDummies)
options(mc.cores = parallel::detectCores())

### AOI
CONUS_AOI <- read_sf(here("Data", "L3_Ecoregions_USB", "CONUS", "CONUS_AOI.gpkg"))
### SNOTEL Climatologies used to fit the model
Snotel_Clim_sf <- read_sf(here("Data", "SNOTEL", "Combined", "GIS", "SnotelCombined_ClimatologyCovars.gpkg"))

### SNOTEL Annual values for testing
Snotel_sf <- read_sf(here("Data", "SNOTEL", "Combined", "GIS", "SnotelCombined_AnnualCovars.gpkg"))
Snotel_Clim_df <- sf_to_df(Snotel_Clim_sf) |>
  drop_na()
Snotel_df <- sf_to_df(Snotel_sf) |>
  drop_na()
### making sure landcover is type factor
Snotel_Clim_df$landcover <- as.factor(Snotel_Clim_df$landcover)
Snotel_Clim_df$landcover_triclass <- as.factor(Snotel_Clim_df$landcover_triclass)
Snotel_df$landcover <- as.factor(Snotel_df$landcover)
Snotel_df$landcover_triclass <- as.factor(Snotel_df$landcover_triclass)

##### Creating Dummy Variables for xgBoost modeling #####
ClimLandcoverDummyVars <- dummy_cols(Snotel_Clim_df[, "landcover_triclass"], select_columns = "landcover_triclass")
LandcoverDummyVars <- dummy_cols(Snotel_df[, "landcover_triclass"], select_columns = "landcover_triclass")
### binding dummy var columns to main dfs
Snotel_Clim_df <- cbind(Snotel_Clim_df, ClimLandcoverDummyVars[,2:4])
Snotel_df <- cbind(Snotel_df, LandcoverDummyVars[,2:4])

# set.seed(802)
# Snotel_split <- initial_split(Snotel_df, prop = 0.7)
# Snotel_train <- training(Snotel_split)
# Snotel_test <- testing(Snotel_split)

##### Separating variables for modeling and model eval #####
### keeping as dataframes for caret
### OctApr
train_OctApr_x <- Snotel_Clim_df[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
train_y <- Snotel_Clim_df$peak_swe
test_OctApr_x <- Snotel_df[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
test_y <- Snotel_df$peak_swe
### OctApr aggregates and DecFeb aggregates for prcpSum and tmean
train_OctAprDecFeb_x <- Snotel_Clim_df[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
test_OctAprDecFeb_x <- Snotel_df[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
### OctMay
train_OctMay_x <- Snotel_Clim_df[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
test_OctMay_x <- Snotel_df[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
### OctMay aggregates and DecFeb aggregates for prcpSum and tmean
train_OctMayDecFeb_x <- Snotel_Clim_df[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
test_OctMayDecFeb_x <- Snotel_df[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
### SepMay
train_SepMay_x <- Snotel_Clim_df[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
test_SepMay_x <- Snotel_df[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
### SepMay aggregates and DecFeb aggregates for prcpSum and tmean
train_SepMayDecFeb_x <- Snotel_Clim_df[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]
test_SepMayDecFeb_x <- Snotel_df[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean", "landcover_triclass_0", "landcover_triclass_1", "landcover_triclass_2")]

##### reading in rasters #####
prcpSum_OctAprClimSum_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "prcpSum_OctAprSum_ClimatologyCDM.tif"))
prcpMean_OctAprClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "prcpMean_OctAprMean_ClimatologyCDM.tif"))
tmean_OctAprClimSum_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmean_OctAprSum_ClimatologyCDM.tif"))
tmin_OctAprClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmin_OctAprMean_ClimatologyCDM.tif"))
tmax_OctAprClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmax_OctAprMean_ClimatologyCDM.tif"))
srad_OctAprClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "srad_OctAprMean_ClimatologyCDM.tif"))
### OctMay Rasters
prcpSum_OctMayClimSum_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "prcpSum_OctMaySum_ClimatologyCDM.tif"))
prcpMean_OctMayClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "prcpMean_OctMayMean_ClimatologyCDM.tif"))
tmean_OctMayClimSum_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmean_OctMaySum_ClimatologyCDM.tif"))
tmin_OctMayClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmin_OctMayMean_ClimatologyCDM.tif"))
tmax_OctMayClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmax_OctMayMean_ClimatologyCDM.tif"))
srad_OctMayClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "srad_OctMayMean_ClimatologyCDM.tif"))
### SepMay Rasters
prcpSum_SepMayClimSum_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "prcpSum_SepMaySum_ClimatologyCDM.tif"))
prcpMean_SepMayClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "prcpMean_SepMayMean_ClimatologyCDM.tif"))
tmean_SepMayClimSum_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmean_SepMaySum_ClimatologyCDM.tif"))
tmin_SepMayClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmin_SepMayMean_ClimatologyCDM.tif"))
tmax_SepMayClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "tmax_SepMayMean_ClimatologyCDM.tif"))
srad_SepMayClimMean_rast <- rast(here("Data", "Daymet", "CONUS", "Climatology", "Resampled", "CDM", "CDM_Aggregates", "srad_SepMayMean_ClimatologyCDM.tif"))
### CONUS topography
CONUS_DEM <- rast(here("Data", "DEM", "CONUS", "CONUSDEMMosaic.tif"))
CONUS_Slope <- rast(here("Data", "DEM", "CONUS", "CONUSSlope.tif"))
CONUS_Aspect <- rast(here("Data", "DEM", "CONUS", "CONUSAspect.tif"))
### CONUS Landcover
# CONUS_LCClim_rast <- rast(here("Data", "Landcover", "CONUS", "ESACCI", "Clipped", "Climatology", "LC_CONUS_Climatology.tif"))



##### Training xgBoost Model #####
### converting matrices to be compatible with xgBoost package
# dtrain <- xgb.DMatrix(data = train_x, label = train_y)
# dtest <- xgb.DMatrix(data = test_x, label = test_y)

# params <- list(
#   objective = "reg:squarederror", # Regression task
#   eta = 0.1, # Learning rate
#   max_depth = 6, # Maximum depth of trees
#   min_child_weight = 1, # Minimum sum of instance weight needed in a child
#   subsample = 0.8, # Subsample ratio of the training instances
#   colsample_bytree = 0.8 # Subsample ratio of columns when constructing each tree
# )
# 
# set.seed(802)
# xgb_model <- xgb.train(
#   params = params,
#   data = dtrain,
#   nrounds = 500, # Number of boosting rounds
#   watchlist = list(train = dtrain, eval = dtest),
#   early_stopping_rounds = 10 # Stop if no improvement after 10 rounds
# )


tune_grid_xgb <- expand.grid(
  nrounds = seq(50, 1000, by = 50),
  max_depth = c(2:8),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = seq(0.5, 1, by = 0.1),
  min_child_weight = 1,
  subsample = seq(0, 1, by = 0.25)
)

tune_control_xgb <- trainControl(
  method = "cv",
  number = 10, # 10-fold cross-validation
  verboseIter = TRUE
)

### train the model with cross validation and different possible hyperparameters
trainstarttime <- Sys.time()
set.seed(802)
xgb_model1 <- train(
  x = train_OctApr_x,
  y = train_y,
  method = "xgbTree",
  trControl = tune_control_xgb,
  tuneGrid = tune_grid_xgb
)
set.seed(802)
xgb_model2 <- train(
  x = train_OctMay_x,
  y = train_y,
  method = "xgbTree",
  trControl = tune_control_xgb,
  tuneGrid = tune_grid_xgb
)
set.seed(802)
xgb_model3 <- train(
  x = train_SepMay_x,
  y = train_y,
  method = "xgbTree",
  trControl = tune_control_xgb,
  tuneGrid = tune_grid_xgb
)
set.seed(802)
xgb_model4 <- train(
  x = train_OctAprDecFeb_x,
  y = train_y,
  method = "xgbTree",
  trControl = tune_control_xgb,
  tuneGrid = tune_grid_xgb
)
### xgb model 5
set.seed(802)
xgb_model5 <- train(
  x = train_OctMayDecFeb_x,
  y = train_y,
  method = "xgbTree",
  trControl = tune_control_xgb,
  tuneGrid = tune_grid_xgb
)
### xgb model 6
set.seed(802)
xgb_model6 <- train(
  x = train_SepMayDecFeb_x,
  y = train_y,
  method = "xgbTree",
  trControl = tune_control_xgb,
  tuneGrid = tune_grid_xgb
)
trainendtime <- Sys.time()
trainendtime - trainstarttime
### saving model to avoid fitting it again
saveRDS(xgb_model1, file = here("Data", "FittedModels", "Caret", "xgBoost", "OctApr_xgBModel.rds"))
saveRDS(xgb_model2, file = here("Data", "FittedModels", "Caret", "xgBoost", "OctMay_xgBModel.rds"))
saveRDS(xgb_model3, file = here("Data", "FittedModels", "Caret", "xgBoost", "SepMay_xgBModel.rds"))
saveRDS(xgb_model4, file = here("Data", "FittedModels", "Caret", "xgBoost", "OctAprDecFeb_xgBModel.rds"))
saveRDS(xgb_model5, file = here("Data", "FittedModels", "Caret", "xgBoost", "OctMayDecFeb_xgBModel.rds"))
saveRDS(xgb_model6, file = here("Data", "FittedModels", "Caret", "xgBoost", "SepMayDecFeb_xgBModel.rds"))
xgb_model1 <- readRDS(here("Data", "FittedModels", "Caret", "xgBoost", "OctApr_xgBModel.rds"))
xgb_model2 <- readRDS(here("Data", "FittedModels", "Caret", "xgBoost", "OctMay_xgBModel.rds"))
xgb_model3 <- readRDS(here("Data", "FittedModels", "Caret", "xgBoost", "SepMay_xgBModel.rds"))
xgb_model4 <- readRDS(here("Data", "FittedModels", "Caret", "xgBoost", "OctAprDecFeb_xgBModel.rds"))
xgb_model5 <- readRDS(here("Data", "FittedModels", "Caret", "xgBoost", "OctMayDecFeb_xgBModel.rds"))
xgb_model6 <- readRDS(here("Data", "FittedModels", "Caret", "xgBoost", "SepMayDecFeb_xgBModel.rds"))

### Display the best parameters and results for best models
### Model 1
print(xgb_model1$bestTune)
xgb_model1_BestTune_results <- xgb_model1$results |>
  filter(RMSE == min(RMSE, na.rm = TRUE))
xgb_model1_BestTune_results$RMSE
xgb_model1_BestTune_results$Rsquared
set.seed(802)
xgb_model1_preds <- predict(xgb_model1, test_OctApr_x)
rmse(test_y, xgb_model1_preds)
### Model 2
print(xgb_model2$bestTune)
xgb_model2_BestTune_results <- xgb_model2$results |>
  filter(RMSE == min(RMSE, na.rm = TRUE))
xgb_model2_BestTune_results$RMSE
xgb_model2_BestTune_results$Rsquared
set.seed(802)
xgb_model2_preds <- predict(xgb_model2, test_OctMay_x)
rmse(test_y, xgb_model2_preds)
### Model 3
print(xgb_model3$bestTune)
xgb_model3_BestTune_results <- xgb_model3$results |>
  filter(RMSE == min(RMSE, na.rm = TRUE))
xgb_model3_BestTune_results$RMSE
xgb_model3_BestTune_results$Rsquared
set.seed(802)
xgb_model3_preds <- predict(xgb_model3, test_SepMay_x)
rmse(test_y, xgb_model3_preds)
### Model 4
print(xgb_model4$bestTune)
xgb_model4_BestTune_results <- xgb_model4$results |>
  filter(RMSE == min(RMSE, na.rm = TRUE))
xgb_model4_BestTune_results$RMSE
xgb_model4_BestTune_results$Rsquared
set.seed(802)
xgb_model4_preds <- predict(xgb_model4, test_OctAprDecFeb_x)
rmse(test_y, xgb_model4_preds)
### Model 5
print(xgb_model5$bestTune)
xgb_model5_BestTune_results <- xgb_model5$results |>
  filter(RMSE == min(RMSE, na.rm = TRUE))
xgb_model5_BestTune_results$RMSE
xgb_model5_BestTune_results$Rsquared
set.seed(802)
xgb_model5_preds <- predict(xgb_model5, test_OctMayDecFeb_x)
rmse(test_y, xgb_model5_preds)
### Model 6
print(xgb_model6$bestTune)
xgb_model6_BestTune_results <- xgb_model6$results |>
  filter(RMSE == min(RMSE, na.rm = TRUE))
xgb_model6_BestTune_results$RMSE
xgb_model6_BestTune_results$Rsquared
set.seed(802)
xgb_model6_preds <- predict(xgb_model6, test_SepMayDecFeb_x)
rmse(test_y, xgb_model6_preds)


##### Stacking Rasters for making spatial predictions #####
# Prediction_Rasters <- c(prcpSum_OctAprClimSum_rast, tmean_OctAprClimSum_rast, CONUS_DEM, CONUS_Slope,  tmin_OctAprClimMean_rast, tmax_OctAprClimMean_rast, srad_OctAprClimMean_rast)
# Prediction_Rasters
# names(Prediction_Rasters) <- c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean")
# names(Prediction_Rasters)
# 
# ### Prediction Rasters for Model 2
# Prediction_Rasters2 <- c(prcpSum_OctMayClimSum_rast, tmean_OctMayClimSum_rast, CONUS_DEM, CONUS_Slope,  tmin_OctMayClimMean_rast, tmax_OctMayClimMean_rast, srad_OctMayClimMean_rast)
# Prediction_Rasters2
# names(Prediction_Rasters2) <- c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean")
# names(Prediction_Rasters2)
# 
# ### Prediction Rasters for Model 3
# Prediction_Rasters3 <- c(prcpSum_SepMayClimSum_rast, tmean_SepMayClimSum_rast, CONUS_DEM, CONUS_Slope,  tmin_SepMayClimMean_rast, tmax_SepMayClimMean_rast, srad_SepMayClimMean_rast)
# Prediction_Rasters3
# names(Prediction_Rasters3) <- c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean")
# names(Prediction_Rasters3)
# 
# ### Prediction Rasters for Model 4
# Prediction_Rasters <- c(prcpSum_OctAprClimSum_rast, tmean_OctAprClimSum_rast, prcpSum_DecFebClimSum_rast, tmean_DecFebClimSum_rast, CONUS_DEM, CONUS_Slope,  tmin_OctAprClimMean_rast, tmax_OctAprClimMean_rast, srad_OctAprClimMean_rast)
# names(Prediction_Rasters4) <- c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean")
# names(Prediction_Rasters4)


##### Making Spatial Predictions #####
### using tuned model from caret, not xgBoost directly
# set.seed(802)
# xgBoost1_PredictionMap <- terra::predict(Prediction_Rasters, tuned_xgb_model1)
# xgBoost2_PredictionMap <- terra::predict(Prediction_Rasters2, tuned_xgb_model2)
# xgBoost3_PredictionMap <- terra::predict(Prediction_Rasters3, tuned_xgb_model3)
# ### cropping model because it predicts outside the land parts
# xgBoost1_PredictionMap_cropmask <- crop(xgBoost1_PredictionMap, CONUS_AOI, mask = TRUE)
# xgBoost2_PredictionMap_cropmask <- crop(xgBoost2_PredictionMap, CONUS_AOI, mask = TRUE)
# xgBoost3_PredictionMap_cropmask <- crop(xgBoost3_PredictionMap, CONUS_AOI, mask = TRUE)
# xgBoost1_PredictionMap_cropmask
# xgBoost2_PredictionMap_cropmask
# xgBoost3_PredictionMap_cropmask
# 
# # plot(xgBoost1_PredictionMap, main = "xgBoost Model of Peak SWE Climatologies")
# plot(xgBoost1_PredictionMap_cropmask, main = "xgBoost Model of Peak SWE Climatologies, cropped and masked to AOI, model 1")
# plot(xgBoost2_PredictionMap_cropmask, main = "xgBoost Model of Peak SWE Climatologies, cropped and masked to AOI, model 2")
# plot(xgBoost3_PredictionMap_cropmask, main = "xgBoost Model of Peak SWE Climatologies, cropped and masked to AOI, model 3")




