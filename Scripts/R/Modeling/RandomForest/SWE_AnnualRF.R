##### SWE Random Forest Model for CONUS #####
### script by Griffin Shelor

##### Loading packages #####
library(pacman)
p_load(here, tidyverse, randomForest, ranger, ModelMetrics, sf, terra, sfext, rsample, caret, parallel, grateful, ModelMetrics, Metrics, mcprogress)
# options(mc.cores = parallel::detectCores())

##### reading in data #####
### CONUS AOI
CONUS_AOI <- read_sf(here("Data", "L3_Ecoregions_USB", "CONUS", "CONUS_AOI.gpkg"))
### SNOTEL Annual values for model fitting and evaluation
Snotel_sf <- read_sf(here("Data", "SNOTEL", "Combined", "GIS", "SnotelCombined_AnnualCovars_AnnualZeroPts.gpkg"))

### converting to df for model fitting process
Snotel_df <- sf_to_df(Snotel_sf) |>
  drop_na(OctApr_prcpSumCDMSum, OctApr_tmeanCDMSum, elevation, slope, aspect, OctApr_tminMean, OctApr_tmaxMean, OctApr_sradMean, landcover_triclass, OctMay_prcpSumCDMSum, OctMay_tmeanCDMSum, OctMay_tminMean, OctMay_tmaxMean, OctMay_sradMean, SepMay_prcpSumCDMSum, SepMay_tmeanCDMSum, SepMay_tminMean, SepMay_tmaxMean, SepMay_sradMean)
### making sure landcover is type integer
Snotel_df$landcover <- as.integer(Snotel_df$landcover)
Snotel_df$landcover_triclass <- as.integer(Snotel_df$landcover_triclass)

##### splitting data into training and testing datasets #####
set.seed(802)
Snotel_split <- initial_split(Snotel_df, prop = 0.75)
Snotel_train <- training(Snotel_split)
Snotel_test <- testing(Snotel_split)
### keeping as dataframes for caret
### OctApr aggregates only
train_OctApr_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass")]
train_y <- Snotel_train$peak_swe
test_OctApr_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass")]
test_y <- Snotel_test$peak_swe
### OctApr aggregates only but with aspect
train_OctAprAspect_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass")]
test_OctAprAspect_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass")]
### OctApr aggregates only but with aspect, and srad removed
train_OctAprAspect_Nosrad_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tminMean", "OctApr_tmaxMean", "landcover_triclass")]
test_OctAprAspect_Nosrad_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tminMean", "OctApr_tmaxMean", "landcover_triclass")]
### OctApr aggregates only but with aspect, and srad, tmin removed
train_OctAprAspect_Nosradtmin_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tmaxMean", "landcover_triclass")]
test_OctAprAspect_Nosradtmin_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tmaxMean", "landcover_triclass")]
### OctApr aggregates only but with aspect, and srad, tmin, tmax removed
train_OctAprAspect_Nosradtmintmax_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
test_OctAprAspect_Nosradtmintmax_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
### OctApr aggregates only but with aspect, and no tmin
train_OctAprAspect_Notmin_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass")]
test_OctAprAspect_Notmin_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass")]
### OctApr aggregates only but with aspect, and no tmin, tmax
train_OctAprAspect_Notmintmax_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_sradMean", "landcover_triclass")]
test_OctAprAspect_Notmintmax_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_sradMean", "landcover_triclass")]
### OctApr aggregates only but with aspect, and no tmax
train_OctAprAspect_Notmax_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tminMean", "OctApr_sradMean", "landcover_triclass")]
test_OctAprAspect_Notmax_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "OctApr_tminMean", "OctApr_sradMean", "landcover_triclass")]
### OctApr aggregates and DecFeb aggregates for prcpSum and tmean
train_OctAprDecFeb_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass")]
test_OctAprDecFeb_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctApr_tminMean", "OctApr_tmaxMean", "OctApr_sradMean", "landcover_triclass")]
### Testing OctMay aggregations in model instead of OctApr
train_OctMay_x <- Snotel_train[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean", "landcover_triclass")]
test_OctMay_x <- Snotel_test[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean", "landcover_triclass")]
### OctMay aggregates only but with aspect, and srad, tmin removed
train_OctMayAspect_Nosradtmin_x <- Snotel_train[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "aspect", "OctMay_tmaxMean", "landcover_triclass")]
test_OctMayAspect_Nosradtmin_x <- Snotel_test[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "aspect", "OctMay_tmaxMean", "landcover_triclass")]
### OctMay aggregates only but with aspect, and srad, tmin, tmax removed
train_OctMayAspect_Nosradtmintmax_x <- Snotel_train[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
test_OctMayAspect_Nosradtmintmax_x <- Snotel_test[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
### OctMay aggregates only but with aspect, and no tmin, tmax
train_OctMayAspect_Notmintmax_x <- Snotel_train[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "aspect", "OctMay_sradMean", "landcover_triclass")]
test_OctMayAspect_Notmintmax_x <- Snotel_test[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "elevation", "slope", "aspect", "OctMay_sradMean", "landcover_triclass")]
### OctMay aggregates and DecFeb aggregates for prcpSum and tmean
train_OctMayDecFeb_x <- Snotel_train[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean", "landcover_triclass")]
test_OctMayDecFeb_x <- Snotel_test[, c("OctMay_prcpSumCDMSum", "OctMay_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "OctMay_tminMean", "OctMay_tmaxMean", "OctMay_sradMean", "landcover_triclass")]
### Testing SepMay aggregations in model instead of OctApr
train_SepMay_x <- Snotel_train[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean", "landcover_triclass")]
test_SepMay_x <- Snotel_test[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean", "landcover_triclass")]
### SepMay aggregates and DecFeb aggregates for prcpSum and tmean
train_SepMayDecFeb_x <- Snotel_train[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean", "landcover_triclass")]
test_SepMayDecFeb_x <- Snotel_test[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "DecFeb_prcpSumCDMSum", "DecFeb_tmeanCDMSum", "elevation", "slope", "SepMay_tminMean", "SepMay_tmaxMean", "SepMay_sradMean", "landcover_triclass")]
### SepMay aggregates only but with aspect, and srad, tmin removed
train_SepMayAspect_Nosradtmin_x <- Snotel_train[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "aspect", "SepMay_tmaxMean", "landcover_triclass")]
test_SepMayAspect_Nosradtmin_x <- Snotel_test[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "aspect", "SepMay_tmaxMean", "landcover_triclass")]
### SepMay aggregates only but with aspect, and srad, tmin, tmax removed
train_SepMayAspect_Nosradtmintmax_x <- Snotel_train[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
test_SepMayAspect_Nosradtmintmax_x <- Snotel_test[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
### SepMay aggregates only but with aspect, and no tmin, tmax
train_SepMayAspect_Notmintmax_x <- Snotel_train[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "aspect", "SepMay_sradMean", "landcover_triclass")]
test_SepMayAspect_Notmintmax_x <- Snotel_test[, c("SepMay_prcpSumCDMSum", "SepMay_tmeanCDMSum", "elevation", "slope", "aspect", "SepMay_sradMean", "landcover_triclass")]


##### Fitting RF Model with Caret #####
### Hyperparameter Tuning Grid
tune_grid_OctApr_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctApr_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctAprAspect_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprAspect_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctAprAspect_Nosrad_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprAspect_Nosrad_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctAprAspect_Nosradtmin_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprAspect_Nosradtmin_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

##### Tune grid final model #####
tune_grid_OctAprAspect_Nosradtmintmax_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprAspect_Nosradtmintmax_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctAprAspect_Notmin_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprAspect_Notmin_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctAprAspect_Notmintmax_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprAspect_Notmintmax_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctAprAspect_Notmax_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprAspect_Notmax_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctMay_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctMay_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctMayAspect_Nosradtmin_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctMayAspect_Nosradtmin_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctMayAspect_Nosradtmintmax_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctMayAspect_Nosradtmintmax_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctMayAspect_Notmintmax_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctMayAspect_Notmintmax_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_SepMay_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_SepMay_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctAprDecFeb_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprDecFeb_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_OctMayDecFeb_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctMayDecFeb_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_SepMayDecFeb_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_SepMayDecFeb_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_SepMayAspect_Nosradtmin_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_SepMayAspect_Nosradtmin_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_SepMayAspect_Nosradtmintmax_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_SepMayAspect_Nosradtmintmax_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

tune_grid_SepMayAspect_Notmintmax_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_SepMayAspect_Notmintmax_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)

##### Training Control with Cross-Validation #####
tune_control_oob_rf <- trainControl(
  method = "oob", # out of bag
  number = 10, # 10-fold cross-validation
  # repeats = 3, # Repeat cross-validation 3 times (more robust)
  verboseIter = TRUE, # Print progress during training
  returnData = FALSE, # Don't save the training data (saves memory)
  savePredictions = "none", # Save predictions from the best model
  returnResamp = "final", # Save resampling results from the best model
  allowParallel = TRUE, # Enable parallel processing (if available)
  predictionBounds = c(0, NA)
)

tune_control_loocv_rf <- trainControl(
  method = "LOOCV", # leave one out CV
  number = 10, # 10-fold cross-validation
  # repeats = 3, # Repeat cross-validation 3 times (more robust)
  verboseIter = TRUE, # Print progress during training
  returnData = FALSE, # Don't save the training data (saves memory)
  savePredictions = "none", # Save predictions from the best model
  returnResamp = "final", # Save resampling results from the best model
  allowParallel = TRUE, # Enable parallel processing (if available)
  predictionBounds = c(0, NA)
)

tune_control_boot_rf <- trainControl(
  method = "boot", # bootstrapping
  number = 10,
  # repeats = 3, # Repeat cross-validation 3 times (more robust)
  verboseIter = TRUE, # Print progress during training
  returnData = FALSE, # Don't save the training data (saves memory)
  savePredictions = "none", # Save predictions from the best model
  returnResamp = "final", # Save resampling results from the best model
  allowParallel = TRUE, # Enable parallel processing (if available)
  predictionBounds = c(0, NA)
)

tune_control_cv_rf <- trainControl(
  method = "cv", # out of bag
  number = 10, # 10-fold cross-validation
  # repeats = 3, # Repeat cross-validation 3 times (more robust)
  verboseIter = TRUE, # Print progress during training
  returnData = FALSE, # Don't save the training data (saves memory)
  savePredictions = "none", # Save predictions from the best model
  returnResamp = "final", # Save resampling results from the best model
  allowParallel = TRUE, # Enable parallel processing (if available)
  predictionBounds = c(0, NA)
)

##### Train the Random Forest Models #####
### Function to parallelize the model-fitting process
fit_RF_models <- function(i, train_data, tune_grid, tune_control_grid_rf){
  ### training model
  set.seed(802)
  train(
    x = train_data[[i]],
    y = train_y,
    method = "rf",
    trControl = tune_control_grid_rf,
    tuneGrid = tune_grid[[i]],
    importance = TRUE,
    n.tree = 2500
  )
}

### making lists of training dfs and tune_grids to input to function
train_df_list <- list(train_OctApr_x, train_OctMay_x, train_SepMay_x, train_OctAprDecFeb_x, train_OctMayDecFeb_x, train_SepMayDecFeb_x, train_OctAprAspect_x, train_OctAprAspect_Nosrad_x, train_OctAprAspect_Nosradtmin_x, train_OctAprAspect_Nosradtmintmax_x, train_OctAprAspect_Notmin_x, train_OctAprAspect_Notmintmax_x, train_OctAprAspect_Notmax_x, train_OctMayAspect_Nosradtmin_x, train_OctMayAspect_Nosradtmintmax_x, train_OctMayAspect_Notmintmax_x, train_SepMayAspect_Nosradtmin_x, train_SepMayAspect_Nosradtmintmax_x, train_SepMayAspect_Notmintmax_x)
tune_grid_list <- list(tune_grid_OctApr_rf, tune_grid_OctMay_rf, tune_grid_SepMay_rf, tune_grid_OctAprDecFeb_rf, tune_grid_OctMayDecFeb_rf, tune_grid_SepMayDecFeb_rf, tune_grid_OctAprAspect_rf, tune_grid_OctAprAspect_Nosrad_rf, tune_grid_OctAprAspect_Nosradtmin_rf, tune_grid_OctAprAspect_Nosradtmintmax_rf, tune_grid_OctAprAspect_Notmin_rf, tune_grid_OctAprAspect_Notmintmax_rf, tune_grid_OctAprAspect_Notmax_rf, tune_grid_OctMayAspect_Nosradtmin_rf, tune_grid_OctMayAspect_Nosradtmintmax_rf, tune_grid_OctMayAspect_Notmintmax_rf, tune_grid_SepMayAspect_Nosradtmin_rf, tune_grid_SepMayAspect_Nosradtmintmax_rf, tune_grid_SepMayAspect_Notmintmax_rf)
### list of test dfs to evaluate model
test_df_list <- list(test_OctApr_x, test_OctMay_x, test_SepMay_x, test_OctAprDecFeb_x, test_OctMayDecFeb_x, test_SepMayDecFeb_x, test_OctAprAspect_x, test_OctAprAspect_Nosrad_x, test_OctAprAspect_Nosradtmin_x, test_OctAprAspect_Nosradtmintmax_x, test_OctAprAspect_Notmin_x, test_OctAprAspect_Notmintmax_x, test_OctAprAspect_Notmax_x, test_OctMayAspect_Nosradtmin_x, test_OctMayAspect_Nosradtmintmax_x, test_OctMayAspect_Notmintmax_x, test_SepMayAspect_Nosradtmin_x, test_SepMayAspect_Nosradtmintmax_x, test_SepMayAspect_Notmintmax_x)

##### training models resampled using OOB error #####
OOBModels <- pmclapply(X = 1:19, FUN = fit_RF_models, train_data = train_df_list, tune_grid = tune_grid_list, tune_control_grid_rf = tune_control_oob_rf, mc.cores = length(tune_grid_list), mc.silent = FALSE)
### evaluating test metrics
for (x in 1:length(OOBModels)){
  print(x)
  temp_model <- OOBModels[[x]]
  print(temp_model$bestTune)
  print(temp_model$results)
  set.seed(802)
  RF_model_preds <- predict(temp_model, test_df_list[[x]])
  print(rmse(test_y, RF_model_preds))
}
##### training models resampled using bootstrapping #####
BootModels <- pmclapply(X = 1:19, FUN = fit_RF_models, train_data = train_df_list, tune_grid = tune_grid_list, tune_control_grid_rf = tune_control_boot_rf, mc.cores = length(tune_grid_list), mc.silent = FALSE)
### evaluating test metrics
for (x in 1:length(BootModels)){
  print(x)
  temp_model <- BootModels[[x]]
  print(temp_model$bestTune)
  print(temp_model$results)
  set.seed(802)
  RF_model_preds <- predict(temp_model, test_df_list[[x]])
  print(rmse(test_y, RF_model_preds))
}
##### training models resampled using cross-validation #####
CVModels <- pmclapply(X = 1:19, FUN = fit_RF_models, train_data = train_df_list, tune_grid = tune_grid_list, tune_control_grid_rf = tune_control_cv_rf, mc.cores = length(tune_grid_list), mc.silent = FALSE)
### evaluating test metrics
for (x in 1:length(CVModels)){
  print(x)
  temp_model <- CVModels[[x]]
  print(temp_model$bestTune)
  print(temp_model$results)
  set.seed(802)
  RF_model_preds <- predict(temp_model, test_df_list[[x]])
  print(rmse(test_y, RF_model_preds))
}



# set.seed(802)
# RF_model1 <- train(
#   x = train_OctApr_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctApr_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# set.seed(802)
# RF_model2 <- train(
#   x = train_OctMay_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctMay_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# set.seed(802)
# RF_model3 <- train(
#   x = train_SepMay_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_SepMay_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 4, OctApr aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_model4 <- train(
#   x = train_OctAprDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctAprDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 5, OctMay aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_model5 <- train(
#   x = train_OctMayDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctMayDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 6, SepMay aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_model6 <- train(
#   x = train_SepMayDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_SepMayDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 7, OctApr aggregates and aspect
# set.seed(802)
# RF_model7 <- train(
#   x = train_OctAprAspect_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctAprAspect_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 8, OctApr aggregates and aspect, no srad
# set.seed(802)
# RF_model8 <- train(
#   x = train_OctAprAspect_Nosrad_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosrad_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 9, OctApr aggregates and aspect, no srad or tmin
# set.seed(802)
# RF_model9 <- train(
#   x = train_OctAprAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 10, OctApr aggregates and aspect, no srad or tmin or tmax
# set.seed(802)
# RF_model10 <- train(
#   x = train_OctAprAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 11, OctApr aggregates and aspect, no tmin
# set.seed(802)
# RF_model11 <- train(
#   x = train_OctAprAspect_Notmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 12, OctApr aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_model12 <- train(
#   x = train_OctAprAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 13, OctApr aggregates and aspect, no tmax
# set.seed(802)
# RF_model13 <- train(
#   x = train_OctAprAspect_Notmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 14, OctMay aggregates and aspect, no srad, tmin
# set.seed(802)
# RF_model14 <- train(
#   x = train_OctMayAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctMayAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 15, OctMay aggregates and aspect, no srad, tmin, tmax
# set.seed(802)
# RF_model15 <- train(
#   x = train_OctMayAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctMayAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 16, OctMay aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_model16 <- train(
#   x = train_OctMayAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_OctMayAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 17, SepMay aggregates and aspect, no srad, tmin
# set.seed(802)
# RF_model17 <- train(
#   x = train_SepMayAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_SepMayAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 18, SepMay aggregates and aspect, no srad, tmin, tmax
# set.seed(802)
# RF_model18 <- train(
#   x = train_SepMayAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_SepMayAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 19, SepMay aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_model19 <- train(
#   x = train_SepMayAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_oob_rf,
#   tuneGrid = tune_grid_SepMayAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# 
# ##### Train the Random Forest Models but with cross validation instead of oob #####
# set.seed(802)
# RF_modelCV1 <- train(
#   x = train_OctApr_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctApr_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# set.seed(802)
# RF_modelCV2 <- train(
#   x = train_OctMay_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctMay_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# set.seed(802)
# RF_modelCV3 <- train(
#   x = train_SepMay_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_SepMay_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 4, OctApr aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_modelCV4 <- train(
#   x = train_OctAprDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctAprDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 5, OctMay aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_modelCV5 <- train(
#   x = train_OctMayDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctMayDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 6, SepMay aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_modelCV6 <- train(
#   x = train_SepMayDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_SepMayDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 7, OctApr aggregates and aspect
# set.seed(802)
# RF_modelCV7 <- train(
#   x = train_OctAprAspect_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctAprAspect_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 8, OctApr aggregates and aspect, no srad
# set.seed(802)
# RF_modelCV8 <- train(
#   x = train_OctAprAspect_Nosrad_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosrad_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 9, OctApr aggregates and aspect, no srad or tmin
# set.seed(802)
# RF_modelCV9 <- train(
#   x = train_OctAprAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 10, OctApr aggregates and aspect, no srad or tmin or tmax
# set.seed(802)
# RF_modelCV10 <- train(
#   x = train_OctAprAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 11, OctApr aggregates and aspect, no tmin
# set.seed(802)
# RF_modelCV11 <- train(
#   x = train_OctAprAspect_Notmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 12, OctApr aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_modelCV12 <- train(
#   x = train_OctAprAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 13, OctApr aggregates and aspect, no tmax
# set.seed(802)
# RF_modelCV13 <- train(
#   x = train_OctAprAspect_Notmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 14, OctMay aggregates and aspect, no srad, tmin
# set.seed(802)
# RF_modelCV14 <- train(
#   x = train_OctMayAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctMayAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 15, OctMay aggregates and aspect, no srad, tmin, tmax
# set.seed(802)
# RF_modelCV15 <- train(
#   x = train_OctMayAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctMayAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 16, OctMay aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_modelCV16 <- train(
#   x = train_OctMayAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_OctMayAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 17, SepMay aggregates and aspect, no srad, tmin
# set.seed(802)
# RF_modelCV17 <- train(
#   x = train_SepMayAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_SepMayAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 18, SepMay aggregates and aspect, no srad, tmin, tmax
# set.seed(802)
# RF_modelCV18 <- train(
#   x = train_SepMayAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_SepMayAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 19, SepMay aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_modelCV19 <- train(
#   x = train_SepMayAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_cv_rf,
#   tuneGrid = tune_grid_SepMayAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# 
# 
# ##### Train the Random Forest Models but with bootstrapping #####
# set.seed(802)
# RF_modelBoot1 <- train(
#   x = train_OctApr_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctApr_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# set.seed(802)
# RF_modelBoot2 <- train(
#   x = train_OctMay_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctMay_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# set.seed(802)
# RF_modelBoot3 <- train(
#   x = train_SepMay_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_SepMay_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 4, OctApr aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_modelBoot4 <- train(
#   x = train_OctAprDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctAprDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 5, OctMay aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_modelBoot5 <- train(
#   x = train_OctMayDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctMayDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 6, SepMay aggregates and DecFeb aggregates for prcpSum and tmean
# set.seed(802)
# RF_modelBoot6 <- train(
#   x = train_SepMayDecFeb_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_SepMayDecFeb_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 7, OctApr aggregates and aspect
# set.seed(802)
# RF_modelBoot7 <- train(
#   x = train_OctAprAspect_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctAprAspect_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 8, OctApr aggregates and aspect, no srad
# set.seed(802)
# RF_modelBoot8 <- train(
#   x = train_OctAprAspect_Nosrad_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosrad_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 9, OctApr aggregates and aspect, no srad or tmin
# set.seed(802)
# RF_modelBoot9 <- train(
#   x = train_OctAprAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 10, OctApr aggregates and aspect, no srad or tmin or tmax
# set.seed(802)
# RF_modelBoot10 <- train(
#   x = train_OctAprAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctAprAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 11, OctApr aggregates and aspect, no tmin
# set.seed(802)
# RF_modelBoot11 <- train(
#   x = train_OctAprAspect_Notmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 12, OctApr aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_modelBoot12 <- train(
#   x = train_OctAprAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 13, OctApr aggregates and aspect, no tmax
# set.seed(802)
# RF_modelBoot13 <- train(
#   x = train_OctAprAspect_Notmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctAprAspect_Notmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 14, OctMay aggregates and aspect, no srad, tmin
# set.seed(802)
# RF_modelBoot14 <- train(
#   x = train_OctMayAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctMayAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 15, OctMay aggregates and aspect, no srad, tmin, tmax
# set.seed(802)
# RF_modelBoot15 <- train(
#   x = train_OctMayAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctMayAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 16, OctMay aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_modelBoot16 <- train(
#   x = train_OctMayAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_OctMayAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 17, SepMay aggregates and aspect, no srad, tmin
# set.seed(802)
# RF_modelBoot17 <- train(
#   x = train_SepMayAspect_Nosradtmin_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_SepMayAspect_Nosradtmin_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 18, SepMay aggregates and aspect, no srad, tmin, tmax
# set.seed(802)
# RF_modelBoot18 <- train(
#   x = train_SepMayAspect_Nosradtmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_SepMayAspect_Nosradtmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )
# ### Model 19, SepMay aggregates and aspect, no tmin or tmax
# set.seed(802)
# RF_modelBoot19 <- train(
#   x = train_SepMayAspect_Notmintmax_x,
#   y = train_y,
#   method = "rf",
#   trControl = tune_control_boot_rf,
#   tuneGrid = tune_grid_SepMayAspect_Notmintmax_rf,
#   importance = TRUE,
#   n.tree = 2500
# )






##### saving model to avoid fitting it again #####
saveRDS(RF_model1, file = here("Data", "FittedModels", "Caret", "RF", "OctApr_RFModel_1km.rds"))
saveRDS(RF_model2, file = here("Data", "FittedModels", "Caret", "RF", "OctMay_RFModel_1km.rds"))
saveRDS(RF_model3, file = here("Data", "FittedModels", "Caret", "RF", "SepMay_RFModel_1km.rds"))
saveRDS(RF_model4, file = here("Data", "FittedModels", "Caret", "RF", "OctAprDecFeb_RFModel_1km.rds"))
saveRDS(RF_model5, file = here("Data", "FittedModels", "Caret", "RF", "OctMayDecFeb_RFModel_1km.rds"))
saveRDS(RF_model6, file = here("Data", "FittedModels", "Caret", "RF", "SepMayDecFeb_RFModel_1km.rds"))
saveRDS(RF_model7, file = here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_RFModel_1km.rds"))
saveRDS(RF_model8, file = here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_Nosrad_RFModel_1km.rds"))
saveRDS(RF_model9, file = here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_Nosradtmin_RFModel_1km.rds"))
saveRDS(RF_model10, file = here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_Nosradtmintmax_RFModel_1km.rds"))
saveRDS(RF_model11, file = here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_Notmin_RFModel_1km.rds"))
saveRDS(RF_model12, file = here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_Notmintmax_RFModel_1km.rds"))
saveRDS(RF_model13, file = here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_Notmax_RFModel_1km.rds"))
saveRDS(RF_model14, file = here("Data", "FittedModels", "Caret", "RF", "OctMayAspect_Nosradtmin_RFModel_1km.rds"))
saveRDS(RF_model15, file = here("Data", "FittedModels", "Caret", "RF", "OctMayAspect_Nosradtmintmax_RFModel_1km.rds"))
saveRDS(RF_model16, file = here("Data", "FittedModels", "Caret", "RF", "OctMayAspect_Notmintmax_RFModel_1km.rds"))
saveRDS(RF_model17, file = here("Data", "FittedModels", "Caret", "RF", "SepMayAspect_Nosradtmin_RFModel_1km.rds"))
saveRDS(RF_model18, file = here("Data", "FittedModels", "Caret", "RF", "SepMayAspect_Nosradtmintmax_RFModel_1km.rds"))
saveRDS(RF_model19, file = here("Data", "FittedModels", "Caret", "RF", "SepMayAspect_Notmintmax_RFModel_1km.rds"))

##### Examine the Best Parameters and Results #####
### Model 1
print(RF_model1$bestTune)
print(RF_model1$results)
set.seed(802)
RF_model1_preds <- predict(RF_model1, test_OctApr_x)
rmse(test_y, RF_model1_preds)
### Model 2
print(RF_model2$bestTune)
print(RF_model2$results)
set.seed(802)
RF_model2_preds <- predict(RF_model2, test_OctMay_x)
rmse(test_y, RF_model2_preds)
### Model 3
print(RF_model3$bestTune)
print(RF_model3$results)
set.seed(802)
RF_model3_preds <- predict(RF_model3, test_SepMay_x)
rmse(test_y, RF_model3_preds)
### Model 4
print(RF_model4$bestTune)
print(RF_model4$results)
set.seed(802)
RF_model4_preds <- predict(RF_model4, test_OctAprDecFeb_x)
rmse(test_y, RF_model4_preds)
### Model 5
print(RF_model5$bestTune)
print(RF_model5$results)
set.seed(802)
RF_model5_preds <- predict(RF_model5, test_OctMayDecFeb_x)
rmse(test_y, RF_model5_preds)
### Model 6
print(RF_model6$bestTune)
print(RF_model6$results)
set.seed(802)
RF_model6_preds <- predict(RF_model6, test_SepMayDecFeb_x)
rmse(test_y, RF_model6_preds)
### Model 7
print(RF_model7$bestTune)
print(RF_model7$results)
set.seed(802)
RF_model7_preds <- predict(RF_model7, test_OctAprAspect_x)
rmse(test_y, RF_model7_preds)
### Model 8
print(RF_model8$bestTune)
print(RF_model8$results)
set.seed(802)
RF_model8_preds <- predict(RF_model8, test_OctAprAspect_Nosrad_x)
rmse(test_y, RF_model8_preds)
### Model 9
print(RF_model9$bestTune)
print(RF_model9$results)
set.seed(802)
RF_model9_preds <- predict(RF_model9, test_OctAprAspect_Nosradtmin_x)
rmse(test_y, RF_model9_preds)
### Model 10
# RF_model10 <- read_rds(here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_Nosradtmintmax_RFModel_1km.rds"))
print(RF_model10$bestTune)
print(RF_model10$results)
set.seed(802)
RF_model10_preds <- predict(RF_model10, test_OctAprAspect_Nosradtmintmax_x)
set.seed(802)
Snotel_SWEpreds <- Snotel_test |>
  mutate(SWE_preds = predict(RF_model10, Snotel_test),
         SWE_ae = ae(peak_swe, SWE_preds)) |>
  select(site_id, site_name, state, WaterYear, peak_swe, SWE_preds, SWE_ae)
Snotel_SWEpreds_WYErrorMeans <- Snotel_SWEpreds |>
  dplyr::group_by(WaterYear) |>
  summarise(mean_SWE_ae = mean(SWE_ae),
            median_SWE_ae = median(SWE_ae))
plot(Snotel_SWEpreds$peak_swe, Snotel_SWEpreds$SWE_ae, main = "SNOTEL Peak SWE Value on X-axis, Absolute Error of Predictions on Y")
hist(Snotel_SWEpreds$SWE_ae, main = "Histogram of Absolute Error at All Annual SNOTEL Sites")
plot(Snotel_SWEpreds$WaterYear, Snotel_SWEpreds$SWE_ae, main = "Water Year on X-axis, Absolute Error of Predictions on Y")
plot(Snotel_SWEpreds_WYErrorMeans$WaterYear, Snotel_SWEpreds_WYErrorMeans$mean_SWE_ae, main = "Water Year on X-axis, Mean Absolute Error of Predictions on Y")
plot(Snotel_SWEpreds_WYErrorMeans$WaterYear, Snotel_SWEpreds_WYErrorMeans$median_SWE_ae, main = "Water Year on X-axis, Median Absolute Error of Predictions on Y")
rmse(test_y, RF_model10_preds)
### Model 11
print(RF_model11$bestTune)
print(RF_model11$results)
set.seed(802)
RF_model11_preds <- predict(RF_model11, test_OctAprAspect_Notmin_x)
rmse(test_y, RF_model11_preds)
### Model 12
print(RF_model12$bestTune)
print(RF_model12$results)
set.seed(802)
RF_model12_preds <- predict(RF_model12, test_OctAprAspect_Notmintmax_x)
rmse(test_y, RF_model12_preds)
### Model 13
print(RF_model13$bestTune)
print(RF_model13$results)
set.seed(802)
RF_model13_preds <- predict(RF_model13, test_OctAprAspect_Notmax_x)
rmse(test_y, RF_model13_preds)
### Model 14
print(RF_model14$bestTune)
print(RF_model14$results)
set.seed(802)
RF_model14_preds <- predict(RF_model14, test_OctMayAspect_Nosradtmin_x)
rmse(test_y, RF_model14_preds)
### Model 15
print(RF_model15$bestTune)
print(RF_model15$results)
set.seed(802)
RF_model15_preds <- predict(RF_model15, test_OctMayAspect_Nosradtmintmax_x)
rmse(test_y, RF_model15_preds)
### Model 16
print(RF_model16$bestTune)
print(RF_model16$results)
set.seed(802)
RF_model16_preds <- predict(RF_model16, test_OctMayAspect_Notmintmax_x)
rmse(test_y, RF_model16_preds)
### Model 17
print(RF_model17$bestTune)
print(RF_model17$results)
set.seed(802)
RF_model17_preds <- predict(RF_model17, test_SepMayAspect_Nosradtmin_x)
rmse(test_y, RF_model17_preds)
### Model 18
print(RF_model18$bestTune)
print(RF_model18$results)
set.seed(802)
RF_model18_preds <- predict(RF_model18, test_SepMayAspect_Nosradtmintmax_x)
rmse(test_y, RF_model18_preds)
### Model 19
print(RF_model19$bestTune)
print(RF_model19$results)
set.seed(802)
RF_model19_preds <- predict(RF_model19, test_SepMayAspect_Notmintmax_x)
rmse(test_y, RF_model19_preds)


