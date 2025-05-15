##### testing a number of folds for my RF model in caret #####
##### Loading packages #####
library(pacman)
p_load(here, tidyverse, randomForest, ranger, sf, terra, sfext, rsample, caret, parallel, grateful, ModelMetrics, Metrics, mcprogress)
# options(mc.cores = parallel::detectCores())

##### reading in data #####
### CONUS AOI
CONUS_AOI <- read_sf(here("Data", "L3_Ecoregions_USB", "CONUS", "CONUS_AOI.gpkg"))
### SNOTEL Annual values
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
### OctApr aggregates only but with aspect, and srad, tmin, tmax removed
train_OctAprAspect_Nosradtmintmax_x <- Snotel_train[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
test_OctAprAspect_Nosradtmintmax_x <- Snotel_test[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
train_y <- Snotel_train$peak_swe
test_y <- Snotel_test$peak_swe



##### Tune grid final model #####
tune_grid_OctAprAspect_Nosradtmintmax_rf <- expand.grid(
  ### Tune mtry (number of variables randomly sampled)
  mtry = seq(2, ncol(train_OctAprAspect_Nosradtmintmax_x) - 1, by = 1) #,
  # splitrule = "variance",
  # min.node.size = seq(5, 25, by = 1)
)


cv_func <- function(fold){
  tune_control_rf <- trainControl(
    method = "cv", # out of bag
    number = fold, # 10-fold cross-validation
    # repeats = 3, # Repeat cross-validation 3 times (more robust)
    verboseIter = TRUE, # Print progress during training
    returnData = FALSE, # Don't save the training data (saves memory)
    savePredictions = "none", # Save predictions from the best model
    returnResamp = "final", # Save resampling results from the best model
    allowParallel = TRUE, # Enable parallel processing (if available)
    predictionBounds = c(0, NA)
  )
  
  ##### Model 10, OctApr aggregates and aspect, no srad or tmin or tmax #####
  set.seed(802)
  RF_model10 <- train(
    x = train_OctAprAspect_Nosradtmintmax_x,
    y = train_y,
    method = "rf",
    trControl = tune_control_rf,
    tuneGrid = tune_grid_OctAprAspect_Nosradtmintmax_rf,
    importance = TRUE,
    n.tree = 2500
  )
  
  return(RF_model10)
}




folds <- 2:50
FoldsTest <- pmclapply(folds, cv_func, mc.cores = ceiling(detectCores() / 1.5), mc.silent = FALSE)

### storing key metrics from each round of CV
rsquared_vals <- c()
rmse_vals <- c()
mae_vals <- c()
test_rmse_vals <- c()
for (x in 1:length(FoldsTest)){
  temp_model <- FoldsTest[[x]]
  temp_results <- temp_model$results
  rsquared_vals <- c(rsquared_vals, max(temp_results$Rsquared))
  rmse_vals <- c(rmse_vals, min(temp_results$RMSE))
  mae_vals <- c(mae_vals, min(temp_results$MAE))
  set.seed(802)
  temp_preds <- predict(temp_model, test_OctAprAspect_Nosradtmintmax_x)
  test_rmse_vals <- c(test_rmse_vals, rmse(test_y, temp_preds))
}

which.max(rsquared_vals)
which.min(rmse_vals)
which.min(mae_vals)
which.min(test_rmse_vals)

rsquared_vals[which.max(rsquared_vals)]
rmse_vals[which.min(rmse_vals)]
mae_vals[which.min(mae_vals)]
test_rmse_vals[which.min(test_rmse_vals)]
rsquared_vals[which.min(test_rmse_vals)]
rmse_vals[which.min(test_rmse_vals)]
mae_vals[which.min(test_rmse_vals)]

hist(rsquared_vals)
hist(rmse_vals)
hist(mae_vals)
hist(test_rmse_vals)

plot(folds, rsquared_vals)
plot(folds, test_rmse_vals)
plot(folds, mae_vals)


##### Final Model Extraction #####
FinalModel <- FoldsTest[[which.min(test_rmse_vals)]]
FinalModel
FinalModel$bestTune
FinalModel$results

write_rds(FinalModel, here("Data", "FittedModels", "Caret", "RF", "AnnualRFModel10.rds"), compress = "gz")
