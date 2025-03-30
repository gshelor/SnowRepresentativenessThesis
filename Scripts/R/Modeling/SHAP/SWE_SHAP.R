##### SHAP #####
totalstarttime <- Sys.time()
##### Loading packages #####
library(pacman)
p_load(here, tidyverse, randomForest, ranger, ModelMetrics, gt, gtExtras, sf, terra, sfext, rsample, caret, parallel, grateful, ModelMetrics, treeshap)
options(mc.cores = parallel::detectCores())

##### reading in data #####
RFModel <- readRDS(here("Data", "FittedModels", "Caret", "RF", "OctAprAspect_Nosradtmintmax_RFModel.rds"))
### CONUS AOI
CONUS_AOI <- read_sf(here("Data", "L3_Ecoregions_USB", "CONUS", "CONUS_AOI.gpkg"))
### Alaska AOI
AK_AOI <- read_sf(here("Data", "L3_Ecoregions_USB", "Alaska", "AK_AOI.gpkg"))
### SNOTEL Climatologies used to fit the model
Snotel_Clim_sf <- read_sf(here("Data", "SNOTEL", "Combined", "GIS", "SnotelCombined_ClimatologyCovars.gpkg"))
### SNOTEL Annual values for testing
Snotel_sf <- read_sf(here("Data", "SNOTEL", "Combined", "GIS", "SnotelCombined_AnnualCovars.gpkg"))

### converting to dfs for model fitting process
Snotel_Clim_df <- sf_to_df(Snotel_Clim_sf) |>
  drop_na()
Snotel_df <- sf_to_df(Snotel_sf) |>
  drop_na()
### making sure landcover is type factor
Snotel_Clim_df$landcover <- as.integer(Snotel_Clim_df$landcover)
Snotel_Clim_df$landcover_triclass <- as.integer(Snotel_Clim_df$landcover_triclass)
Snotel_df$landcover <- as.integer(Snotel_df$landcover)
Snotel_df$landcover_triclass <- as.integer(Snotel_df$landcover_triclass)

##### splitting data into training and testing datasets #####
train_y <- Snotel_Clim_df$peak_swe
test_y <- Snotel_df$peak_swe
### OctApr aggregates only but with aspect, and srad, tmin, tmax removed
train_OctAprAspect_Nosradtmintmax_x <- Snotel_Clim_df[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]
test_OctAprAspect_Nosradtmintmax_x <- Snotel_df[, c("OctApr_prcpSumCDMSum", "OctApr_tmeanCDMSum", "elevation", "slope", "aspect", "landcover_triclass")]



##### SHAP stuff #####
### in order to run treeshap, the model must be "unified"
# ?unify
UnifiedRF <- unify(RFModel$finalModel, data = train_OctAprAspect_Nosradtmintmax_x)

### years of study period
years <- 1993:2020
for (i in 1:length(years)){
  temp_starttime <- Sys.time()
  print(temp_starttime)
  print(years[i])
  print("CONUS")
  ### reading in csv versions of data frames so treeshap can understand them
  ### SWE_max predictions
  print("reading in prediction csv")
  CONUSPredictions <- read_csv(here("Data", "SHAP", "PredictionCSVs", "CONUS", paste0("CONUS_SWEmaxPredictions", years[i], ".csv")))
  ### removing xy
  CONUSPredictions_NoXY <- CONUSPredictions[,3]
  colnames(CONUSPredictions_NoXY) <- "peak_SWE"
  
  print("reading in predictor csv")
  CONUSPredictors <- read_csv(here("Data", "SHAP", "PredictorCSVs", "CONUS", paste0("CONUS_SWEmaxPredictors", years[i], ".csv"))) |>
    filter(paste0(x,y) %in% paste0(CONUSPredictions$x, CONUSPredictions$y))
  ### removing xy data for treeshap
  CONUSPredictors_NoXY <- CONUSPredictors |>
    select(OctApr_prcpSumCDMSum, OctApr_tmeanCDMSum, elevation, slope, aspect, landcover_triclass)
  ### storing xy separately
  CONUSPredictors_XY <- CONUSPredictors |>
    select(x,y)
  
  ### binding predictors with associated predictions
  # print("binding predictors with associated predictions")
  # CONUSVals <- cbind(CONUSPredictors_NoXY, CONUSPredictions_NoXY)
  
  ### Running Treeshap
  print("running treeshap")
  set.seed(802)
  CONUSTreeshap <- treeshap(unified_model = UnifiedRF, x = CONUSPredictors[1:500,])
  write_rds(CONUSTreeshap, here("Data", "SHAP", "Treeshap", "CONUS", paste0("CONUSSWEmax_SHAPs", years[i], ".rds")))
  
  
  ### Alaska
  print("Alaska")
  ### reading in csv versions of data frames so treeshap can understand them
  ### SWE_max predictions
  print("reading in prediction csv")
  AKPredictions <- read_csv(here("Data", "SHAP", "PredictionCSVs", "Alaska", paste0("AK_SWEmaxPredictions", years[i], ".csv")))
  ### removing xy
  AKPredictions_NoXY <- AKPredictions[,3]
  colnames(AKPredictions_NoXY) <- "peak_SWE"
  
  print("reading in predictor csv")
  AKPredictors <- read_csv(here("Data", "SHAP", "PredictorCSVs", "Alaska", paste0("AK_SWEmaxPredictors", years[i], ".csv"))) |>
    filter(paste0(x,y) %in% paste0(AKPredictions$x, AKPredictions$y))
  ### removing xy data for treeshap
  AKPredictors_NoXY <- AKPredictors |>
    select(OctApr_prcpSumCDMSum, OctApr_tmeanCDMSum, elevation, slope, aspect, landcover_triclass)
  ### storing xy separately
  AKPredictors_XY <- AKPredictors |>
    select(x,y)
  
  ### binding predictors with associated predictions
  # print("binding predictors with associated predictions")
  # AKVals <- cbind(AKPredictors_NoXY, AKPredictions_NoXY)
  
  ### Running Treeshap
  print("running treeshap")
  AKTreeshap <- treeshap(unified_model = UnifiedRF, x = AKPredictors)
  write_rds(AKTreeshap, here("Data", "SHAP", "Treeshap", "Alaska", paste0("AKSWEmax_SHAPs", years[i], ".rds")))
  
  temp_endtime <- Sys.time()
  print(temp_endtime)
  temp_endtime - temp_starttime
}


##### Converting CSVs to smaller RDS files #####
### converting csvs to RDS files
# AKPredictor_csv_list <- list.files(path = here("Data", "SHAP", "PredictorCSVs", "Alaska"), pattern = "^AK_SWEmaxPredictors.*\\.csv$")
# AKPrediction_csv_list <- list.files(path = here("Data", "SHAP", "PredictionCSVs", "Alaska"), pattern = "^AK_SWEmaxPredictions.*\\.csv$")
# CONUSPredictor_csv_list <- list.files(path = here("Data", "SHAP", "PredictorCSVs", "CONUS"), pattern = "^CONUS_SWEmaxPredictors.*\\.csv$")
# CONUSPrediction_csv_list <- list.files(path = here("Data", "SHAP", "PredictionCSVs", "CONUS"), pattern = "^CONUS_SWEmaxPredictions.*\\.csv$")
# 
# years <- 1993:2020
# for (i in 1:length(AKPredictor_csv_list)){
#   print(years[i])
#   ### Alaska Predictors
#   temp_csv <- read_csv(here("Data", "SHAP", "PredictorCSVs", "Alaska", AKPredictor_csv_list[i]))
#   write_rds(temp_csv, here("Data", "SHAP", "PredictorCSVs", "Alaska", "RDS", paste0("AK_SWEmaxPredictors", years[i], ".rds")), compress = "gz")
#   
#   ### Alaska Predictions
#   temp_csv <- read_csv(here("Data", "SHAP", "PredictionCSVs", "Alaska", AKPrediction_csv_list[i]))
#   write_rds(temp_csv, here("Data", "SHAP", "PredictionCSVs", "Alaska", "RDS", paste0("AK_SWEmaxPredictions", years[i], ".rds")), compress = "gz")
#   
#   ### CONUS Predictors
#   temp_csv <- read_csv(here("Data", "SHAP", "PredictorCSVs", "CONUS", CONUSPredictor_csv_list[i]))
#   write_rds(temp_csv, here("Data", "SHAP", "PredictorCSVs", "CONUS", "RDS", paste0("CONUS_SWEmaxPredictors", years[i], ".rds")), compress = "gz")
#   
#   ### CONUS Predictions
#   temp_csv <- read_csv(here("Data", "SHAP", "PredictionCSVs", "CONUS", CONUSPrediction_csv_list[i]))
#   write_rds(temp_csv, here("Data", "SHAP", "PredictionCSVs", "CONUS", "RDS", paste0("CONUS_SWEmaxPredictions", years[i], ".rds")), compress = "gz")
# }


totalendtime <- Sys.time()
totaltime <- totalendtime - totalstarttime
totaltime
