##### Script to be used for downloading and cleaning SNOTEL, Daymet, Elevation, and NLCD data for my thesis work #####
### script by Griffin Shelor
TotalStartTime <- Sys.time()

##### installing/loading packages #####
# install.packages(c("pacman", "here", "tidyverse", "daymetr", "snotelr", "rstac", "rsi"))
library(pacman)
p_load(here, tidyverse, daymetr, snotelr, rstac, rsi, ncdf4, parallel, svMisc, sf, terra, future)
future::plan("multisession")

here()

### putting this function here, gonna use it later
`%nin%` = Negate(`%in%`)

### Snotel Download Start time
SnotelStartTime <- Sys.time()
##### Downloading SNOTEL data #####
### snotel data already downloaded in a previous run of this script, so I'm just reading in those csvs at the end of this section
### calling snotel_info first to initialize what sites I actually want to download
SnotelInfo <- snotel_info()


### ensuring start and end columns in SnotelInfo are in date format
SnotelInfo$start <- ymd(SnotelInfo$start)
SnotelInfo$end <- ymd(SnotelInfo$end)

### filtering to only include sites that cover from October 1, 1980 to September 30, 2022
SnotelInfo_Clean <- SnotelInfo |>
  filter(site_id != 201 & site_id != 1315) |>
  filter(start <= "1992-10-01" & end >= "2023-09-30") |>
  arrange(site_id)

### separating Snotel info by whether it is in Alaska or not
SnotelInfo_AK <- SnotelInfo_Clean |>
  filter(state == "AK")

SnotelInfo_CONUS <- SnotelInfo_Clean |>
  filter(state != "AK") |>
  filter(site_id != 657)

### downloading snotel data for conus fails at site 657 for some reason, but it works when I download it separately????
SnotelData_657 <- snotel_download(657, internal = TRUE)


### now downloading SNOTEL data only for sites that cover my whole study period
SnotelData_AK <- snotel_download(site_id = SnotelInfo_AK$site_id, internal = TRUE)
SnotelData_CONUS <- snotel_download(site_id = SnotelInfo_CONUS$site_id, internal = TRUE)

### binding site 657 and rest of CONUS Snotel data back together
SnotelData_CONUS <- rbind(SnotelData_CONUS, SnotelData_657)

### removing NAs, arranging by both site_id and date
SnotelData_AK <- SnotelData_AK |>
  drop_na(snow_water_equivalent) |>
  separate(date, into = c("Year", "Month", "Day"), sep = "-") |>
  mutate(date = ymd(paste0(Year, "-", Month, "-", Day)), .before = 15)

### Converting individual year, month, and day columns to be numeric
SnotelData_AK$Year <- as.numeric(SnotelData_AK$Year)
SnotelData_AK$Month <- as.numeric(SnotelData_AK$Month)
SnotelData_AK$Day <- as.numeric(SnotelData_AK$Day)

### filtering out just relevant dates, adding new columns to assist with grabbing daymet data
SnotelData_AK <- SnotelData_AK |>
  filter(date > "1992-09-30") |>
  filter(date < "2023-10-01")

### Daymet is annoyingly weird about leap years so I'm separating those out and creating the yearday column differently for those years
### it drops Dec 31 for some reason, why is Oak Ridge National Lab doing this to me
SnotelData_AK_DecLeapYears <- SnotelData_AK |>
  filter(Year %in% c(1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)) |>
  filter(Month == 12) |>
  filter(Day != 31) |>
  mutate(yearday = 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + Day, .before = 15)

SnotelData_AK_JanNovLeapYears <- SnotelData_AK |>
  filter(Year %in% c(1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)) |>
  filter(Month != 12) |>
  mutate(yearday = case_when(Month == 1 ~ Day,
                             Month == 2 ~ 31 + Day,
                             Month == 3 ~ 31 + 28 + Day,
                             Month == 4 ~ 31 + 28 + 31 + Day,
                             Month == 5 ~ 31 + 28 + 31 + 30 + Day,
                             Month == 6 ~ 31 + 28 + 31 + 30 + 31 + Day,
                             Month == 7 ~ 31 + 28 + 31 + 30 + 31 + 30 + Day,
                             Month == 8 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + Day,
                             Month == 9 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + Day,
                             Month == 10 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + Day,
                             TRUE ~ 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + Day), .before = 15)

SnotelData_AK_LeapYears <- rbind(SnotelData_AK_DecLeapYears, SnotelData_AK_JanNovLeapYears)


SnotelData_AK_NonLeapYears <- SnotelData_AK |>
  filter(Year %nin% c(1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)) |>
  mutate(yearday = case_when(Month == 1 ~ Day,
                             Month == 2 ~ 31 + Day,
                             Month == 3 ~ 31 + 28 + Day,
                             Month == 4 ~ 31 + 28 + 31 + Day,
                             Month == 5 ~ 31 + 28 + 31 + 30 + Day,
                             Month == 6 ~ 31 + 28 + 31 + 30 + 31 + Day,
                             Month == 7 ~ 31 + 28 + 31 + 30 + 31 + 30 + Day,
                             Month == 8 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + Day,
                             Month == 9 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + Day,
                             Month == 10 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + Day,
                             Month == 11 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + Day,
                             TRUE ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + Day), .before = 15)
  


SnotelData_AK <- rbind(SnotelData_AK_LeapYears, SnotelData_AK_NonLeapYears) |>
  mutate(WaterYear = case_when(Month >= 10 ~ Year + 1,
                               TRUE ~ Year),
         study_year = WaterYear - min(WaterYear)) |>
  arrange(date, site_id)


### now repeating process for CONUS
### removing NAs, arranging by both site_id and date
SnotelData_CONUS <- SnotelData_CONUS |>
  drop_na(snow_water_equivalent) |>
  separate(date, into = c("Year", "Month", "Day"), sep = "-") |>
  mutate(date = ymd(paste0(Year, "-", Month, "-", Day)), .before = 15)

### Converting individual year, month, and day columns to be numeric
SnotelData_CONUS$Year <- as.numeric(SnotelData_CONUS$Year)
SnotelData_CONUS$Month <- as.numeric(SnotelData_CONUS$Month)
SnotelData_CONUS$Day <- as.numeric(SnotelData_CONUS$Day)

### filtering out just relevant dates, adding new columns to assist with grabbing daymet data
SnotelData_CONUS <- SnotelData_CONUS |>
  filter(date > "1992-09-30") |>
  filter(date < "2023-10-01")

### Daymet is annoyingly weird about leap years so I'm separating those out and creating the yearday column differently for those years
### it drops Dec 31 for some reason, why is Oak Ridge National Lab doing this to me
SnotelData_CONUS_DecLeapYears <- SnotelData_CONUS |>
  filter(Year %in% c(1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)) |>
  filter(Month == 12) |>
  filter(Day != 31) |>
  mutate(yearday = 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + Day, .before = 15)

SnotelData_CONUS_JanNovLeapYears <- SnotelData_CONUS |>
  filter(Year %in% c(1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)) |>
  filter(Month != 12) |>
  mutate(yearday = case_when(Month == 1 ~ Day,
                             Month == 2 ~ 31 + Day,
                             Month == 3 ~ 31 + 28 + Day,
                             Month == 4 ~ 31 + 28 + 31 + Day,
                             Month == 5 ~ 31 + 28 + 31 + 30 + Day,
                             Month == 6 ~ 31 + 28 + 31 + 30 + 31 + Day,
                             Month == 7 ~ 31 + 28 + 31 + 30 + 31 + 30 + Day,
                             Month == 8 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + Day,
                             Month == 9 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + Day,
                             Month == 10 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + Day,
                             TRUE ~ 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + Day), .before = 15)

SnotelData_CONUS_LeapYears <- rbind(SnotelData_CONUS_DecLeapYears, SnotelData_CONUS_JanNovLeapYears)


SnotelData_CONUS_NonLeapYears <- SnotelData_CONUS |>
  filter(Year %nin% c(1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)) |>
  mutate(yearday = case_when(Month == 1 ~ Day,
                             Month == 2 ~ 31 + Day,
                             Month == 3 ~ 31 + 28 + Day,
                             Month == 4 ~ 31 + 28 + 31 + Day,
                             Month == 5 ~ 31 + 28 + 31 + 30 + Day,
                             Month == 6 ~ 31 + 28 + 31 + 30 + 31 + Day,
                             Month == 7 ~ 31 + 28 + 31 + 30 + 31 + 30 + Day,
                             Month == 8 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + Day,
                             Month == 9 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + Day,
                             Month == 10 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + Day,
                             Month == 11 ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + Day,
                             TRUE ~ 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + Day), .before = 15)



SnotelData_CONUS <- rbind(SnotelData_CONUS_LeapYears, SnotelData_CONUS_NonLeapYears) |>
  mutate(WaterYear = case_when(Month >= 10 ~ Year + 1,
                               TRUE ~ Year),
         study_year = WaterYear - min(WaterYear)) |>
  arrange(date, site_id)

### writing SNOTEL data as csvs
write_csv(SnotelData_AK, here("Data", "SNOTEL", "Alaska", "SnotelData_AK.csv"))
write_csv(SnotelData_CONUS, here("Data", "SNOTEL", "CONUS", "SnotelData_CONUS.csv"))

### reading in SNOTEL data from csvs which were saved in a previous run of this script
SnotelData_AK <- read_csv(here("Data", "SNOTEL", "Alaska", "SnotelData_AK.csv"))
SnotelData_CONUS <- read_csv(here("Data", "SNOTEL", "CONUS", "SnotelData_CONUS.csv"))

### converting SNOTEL data to extract peak SWE at each SNOTEL site for each year
### Alaska first
### separating out each site, grouping by Water year, identifying peak SWE
for (id in 1:length(unique(SnotelData_AK$site_id))){
  if (id == 1){
    temp_id <- SnotelData_AK |>
      filter(site_id == site_id[id])
    SnotelData_AK_PeakSWE <- temp_id |>
      group_by(WaterYear, site_id, site_name, description, state, start, end, longitude, latitude, Year) |>
      summarise(peak_swe = max(snow_water_equivalent))
  } else{
    temp_id <- SnotelData_AK |>
      filter(site_id == site_id[id])
    temp_id_peakSWE <- temp_id |>
      group_by(WaterYear, site_id, site_name, description, state, start, end, longitude, latitude, Year) |>
      summarise(peak_swe = max(snow_water_equivalent))
    SnotelData_AK_PeakSWE <- rbind(SnotelData_AK_PeakSWE, temp_id_peakSWE)
  }
}


### Now CONUS
### separating out each site, grouping by Water year, identifying peak SWE
for (id in 1:length(unique(SnotelData_CONUS$site_id))){
  if (id == 1){
    temp_id <- SnotelData_CONUS |>
      filter(site_id == site_id[id])
    SnotelData_CONUS_PeakSWE <- temp_id |>
      group_by(WaterYear, site_id, site_name, description, state, start, end, longitude, latitude, Year) |>
      summarise(peak_swe = max(snow_water_equivalent))
  } else{
    temp_id <- SnotelData_CONUS |>
      filter(site_id == site_id[id])
    temp_id_peakSWE <- temp_id |>
      group_by(WaterYear, site_id, site_name, description, state, start, end, longitude, latitude, Year) |>
      summarise(peak_swe = max(snow_water_equivalent))
    SnotelData_CONUS_PeakSWE <- rbind(SnotelData_CONUS_PeakSWE, temp_id_peakSWE)
  }
}


### writing SNOTEL data as csvs
write_csv(SnotelData_AK_PeakSWE, here("Data", "SNOTEL", "Alaska", "SnotelData_AK_WY9323_PeakSWE.csv"))
write_csv(SnotelData_CONUS_PeakSWE, here("Data", "SNOTEL", "CONUS", "SnotelData_CONUS_WY9323_PeakSWE.csv"))

### reading in SNOTEL data from csvs which were saved in a previous run of this script
SnotelData_AK_PeakSWE <- read_csv(here("Data", "SNOTEL", "Alaska", "SnotelData_AK_WY9323_PeakSWE.csv"))
SnotelData_CONUS_PeakSWE <- read_csv(here("Data", "SNOTEL", "CONUS", "SnotelData_CONUS_WY9323_PeakSWE.csv"))

### Converting csvs to sf object, then to shapefile to be imported to Earth Engine
### converting to sf
## Alaska first
SnotelData_AK_PeakSWE_sf <- st_as_sf(SnotelData_AK_PeakSWE, coords = c("longitude", "latitude"), crs = 4326)
## CONUS
SnotelData_CONUS_PeakSWE_sf <- st_as_sf(SnotelData_CONUS_PeakSWE, coords = c("longitude", "latitude"), crs = 4326)

### exporting as geopackage, and as shapefile
### Alaska
st_write(SnotelData_AK_PeakSWE_sf, here("Data", "SNOTEL", "Alaska", "GIS", "SnotelData_AK_PeakSWE.gpkg"))
### CONUS
st_write(SnotelData_CONUS_PeakSWE_sf, here("Data", "SNOTEL", "CONUS", "GIS", "SnotelData_CONUS_PeakSWE.gpkg"))
st_write(SnotelData_AK_PeakSWE_sf, here("Data", "SNOTEL", "Alaska", "GIS", "SnotelData_AK_PeakSWE.shp"))
### CONUS
st_write(SnotelData_CONUS_PeakSWE_sf, here("Data", "SNOTEL", "CONUS", "GIS", "SnotelData_CONUS_PeakSWE.shp"))

### Snotel download end time
SnotelEndTime <- Sys.time()
SnotelDownloadTime <- SnotelEndTime - SnotelStartTime




### Daymet NetCDF tile download start time
DaymetTileStartTime <- Sys.time()
DaymetTileStartTime
##### Downloading Daymet netcdf files for whole study area #####
### Alaska first
### ended up splitting daymet downloads between like 9 different computers, but in total I am downloading daymet data from 1992-2022

### fixing SnotelInfo_CONUS so it includes all of the relevant sites, not just the ones that work in the batch download call
SnotelInfo_CONUS <- SnotelInfo_Clean |>
  filter(state != "AK")

# download_daymet_tiles(
#   location = c(min(SnotelInfo_AK$latitude), min(SnotelInfo_AK$longitude), max(SnotelInfo_AK$latitude), max(SnotelInfo_AK$longitude)),
#   # tiles,
#   start = 2023,
#   end = 2023,
#   path = here("Data", "Daymet", "Alaska"),
#   param = c("tmin", "tmax", "srad", "prcp"),
#   silent = FALSE,
#   force = FALSE
# )
# 
# download_daymet_tiles(
#   location = c(min(SnotelInfo_CONUS$latitude), min(SnotelInfo_CONUS$longitude), max(SnotelInfo_CONUS$latitude), max(SnotelInfo_CONUS$longitude)),
#   # tiles,
#   start = 2023,
#   end = 2023,
#   path = here("Data", "Daymet", "CONUS"),
#   param = c("tmin", "tmax", "srad", "prcp"),
#   silent = FALSE,
#   force = FALSE
# )

### Daymet netcdf tile end time
DaymetTileEndTime <- Sys.time()
DaymetTileDownloadTime <- DaymetTileEndTime - DaymetTileStartTime


##### Reading in Ecoregions to use as bounding boxes for DEM download #####
## reading in ecoregions with state boundaries
Ecoregions_sf <- read_sf(here("Data", "L3_Ecoregions_USB", "us_eco_l3_state_boundaries.shp")) |> 
  filter(STATE_NAME == "California" | STATE_NAME == "Oregon" | STATE_NAME == "Colorado" | STATE_NAME == "Idaho" | STATE_NAME == "Wyoming" | STATE_NAME == "New Mexico" | STATE_NAME == "Montana" | STATE_NAME == "Arizona" | STATE_NAME == "South Dakota" | STATE_NAME == "Nevada" | STATE_NAME == "Utah" | STATE_NAME == "Washington" | STATE_NAME == "Alaska")
crs(Ecoregions_sf, describe = TRUE)

##### Converting AK and CONUS Snotel Obs to sf objects for land cover download #####
### Converting AK SNOTEL from data frame to sf object
SnotelInfo_AK_sf <- st_as_sf(SnotelInfo_AK, coords = c("longitude", "latitude"))
## setting CRS to WGS 84 initially
st_crs(SnotelInfo_AK_sf) <- 4326

### transforming snotel data
SnotelInfo_AK_sf <- st_transform(SnotelInfo_AK_sf, 6393)


### now doing the same process for CONUS
SnotelInfo_CONUS_sf <- st_as_sf(SnotelInfo_CONUS, coords = c("longitude", "latitude"))
### setting CRS to WGS 84 initially
st_crs(SnotelInfo_CONUS_sf) <- 4326

### transforming snotel data
SnotelInfo_CONUS_sf <- st_transform(SnotelInfo_CONUS_sf, 6341)

### DEM Download Start time
DEMStartTime <- Sys.time()
##### Downloading DEM data #####

# DEM_AK <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = NULL, end_date = NULL, pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "elevation", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "nasadem")

# DEM_CONUS <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = NULL, end_date = NULL, pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "elevation", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "nasadem")

### downloading DEM data via rstac package
# states <- c("California", "Oregon", "Colorado", "Idaho", "Wyoming", "New Mexico", "Montana", "Arizona", "South Dakota", "Nevada", "Utah", "Washington", "Alaska")
# for (state in states){
#   if (state == "Alaska"){
    ### creating bounding box which gets fed into rstac process
    # bbox_temp <- SnotelInfo_AK_sf |>
    #   st_transform(4326) |>
    #   st_bbox()
    
    ### connecting to the appropriate stac API
    # stac_source <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    # stac_source
    
    # rstac::get_request(stac_source)
    
    # collections_query <- stac_source |>
    #   rstac::collections()
    # collections_query
    
    # class(stac_source)
    # class(collections_query)
    
    # available_collections <- rstac::get_request(collections_query)
    # available_collections
    
    ## setting up STAC search for NASA DEM data
    # rstac::stac_search(
    #   q = stac_source,
    #   collections = "nasadem",
    #   bbox = bbox_temp) |>
    #   rstac::get_request()
    
    
    # stac_query <- rstac::stac_search(
    #   q = stac_source,
    #   collections = "nasadem",
    #   bbox = bbox_temp)
    
    # executed_stac_query <- rstac::get_request(stac_query)
    # executed_stac_query
    
    ### signing in to MPC in order to access STAC with rstac
    # signed_stac_query <- rstac::items_sign(
    #   executed_stac_query,
    #   rstac::sign_planetary_computer())
    # signed_stac_query
    
    ### downloading elevation data
    # rstac::assets_download(signed_stac_query, "elevation", output_dir = here("Data", "DEM", "Alaska"))
    
  # } else if (state == "New Mexico"){
    ### setting bounding box for DEM download
    # bbox_temp <- Ecoregions_sf |>
    #   filter(STATE_NAME == state) |>
    #   st_transform(4326) |>
    #   st_bbox()
    
    
    ### connecting to the appropriate stac API
    # stac_source <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    # stac_source
    
    # rstac::get_request(stac_source)
    
    # collections_query <- stac_source |>
      # rstac::collections()
    # collections_query
    
    # class(stac_source)
    # class(collections_query)
    
    # available_collections <- rstac::get_request(collections_query)
    # available_collections
    
    ## setting up STAC search for NASA DEM data
    # rstac::stac_search(
    #   q = stac_source,
    #   collections = "nasadem",
    #   bbox = bbox_temp) |>
    #   rstac::get_request()
    # 
    # 
    # stac_query <- rstac::stac_search(
    #   q = stac_source,
    #   collections = "nasadem",
    #   bbox = bbox_temp)
    # 
    # executed_stac_query <- rstac::get_request(stac_query)
    # executed_stac_query
    
    ### signing in to MPC in order to access STAC with rstac
    # signed_stac_query <- rstac::items_sign(
    #   executed_stac_query,
    #   rstac::sign_planetary_computer())
    # signed_stac_query
    
    ### downloading elevation data
    # rstac::assets_download(signed_stac_query, "elevation", output_dir = here("Data", "DEM", "NewMexico"))
  # } else if (state == "South Dakota") {
    ### setting bounding box for DEM download
    # bbox_temp <- Ecoregions_sf |>
    #   filter(STATE_NAME == state) |>
    #   st_transform(4326) |>
    #   st_bbox()
    
    ### connecting to the appropriate stac API
    # stac_source <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    # stac_source
    
    # rstac::get_request(stac_source)
    
    # collections_query <- stac_source |>
    #   rstac::collections()
    # collections_query
    
    # class(stac_source)
    # class(collections_query)
    
    # available_collections <- rstac::get_request(collections_query)
    # available_collections
    
    ## setting up STAC search for NASA DEM data
    # rstac::stac_search(
    #   q = stac_source,
    #   collections = "nasadem",
    #   bbox = bbox_temp) |>
    #   rstac::get_request()
    # 
    # 
    # stac_query <- rstac::stac_search(
    #   q = stac_source,
    #   collections = "nasadem",
    #   bbox = bbox_temp)
    # 
    # executed_stac_query <- rstac::get_request(stac_query)
    # executed_stac_query
    
    ### signing in to MPC in order to access STAC with rstac
    # signed_stac_query <- rstac::items_sign(
    #   executed_stac_query,
    #   rstac::sign_planetary_computer())
    # signed_stac_query
    # 
    ### downloading elevation data
    # rstac::assets_download(signed_stac_query, "elevation", output_dir = here("Data", "DEM", "SouthDakota"))
  # } else {
  #   bbox_temp <- Ecoregions_sf |>
  #     filter(STATE_NAME == state) |>
  #     st_transform(4326) |>
  #     st_bbox()
    
    
    ### connecting to the appropriate stac API
    # stac_source <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    # stac_source
    
    # rstac::get_request(stac_source)
    
    # collections_query <- stac_source |>
    #   rstac::collections()
    # collections_query
    
    # class(stac_source)
    # class(collections_query)
    
    # available_collections <- rstac::get_request(collections_query)
    # available_collections
    
    ## setting up STAC search for NASA DEM data
    # rstac::stac_search(
    #   q = stac_source,
    #   collections = "nasadem",
    #   bbox = bbox_temp) |>
    #   rstac::get_request()
    # 
    # 
    # stac_query <- rstac::stac_search(
    #   q = stac_source,
    #   collections = "nasadem",
    #   bbox = bbox_temp)
    # 
    # executed_stac_query <- rstac::get_request(stac_query)
    # executed_stac_query
    
    ### signing in to MPC in order to access STAC with rstac
    # signed_stac_query <- rstac::items_sign(
    #   executed_stac_query,
    #   rstac::sign_planetary_computer())
    # signed_stac_query
    
    ### downloading elevation data
#     rstac::assets_download(signed_stac_query, "elevation", output_dir = here("Data", "DEM", paste0(state)))
#   }
# }

### DEM Download End Time
DEMEndTime <- Sys.time()
DEMDownloadTime <- DEMEndTime - DEMStartTime

### Land Cover start time
LCStartTime <- Sys.time()
##### Land cover download #####

### starting with Alaska
## 1992
# LC_AK_1992 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "1992-01-01", end_date = "1992-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_1992.tif"))
# 
# ## 1993
# LC_AK_1993 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "1993-01-01", end_date = "1993-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_1993.tif"))
# 
# ## 1994
# LC_AK_1994 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "1994-01-01", end_date = "1994-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_1994.tif"))
# 
# ## 1995
# LC_AK_1995 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "1995-01-01", end_date = "1995-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_1995.tif"))
# 
# ## 1996
# LC_AK_1996 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "1996-01-01", end_date = "1996-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_1996.tif"))
# 
# ## 1997
# LC_AK_1997 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "1997-01-01", end_date = "1997-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_1997.tif"))
# 
# ## 1998
# LC_AK_1998 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "1998-01-01", end_date = "1998-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_1998.tif"))
# 
# ## 1999
# LC_AK_1999 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "1999-01-01", end_date = "1999-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_1999.tif"))
# 
# ## 2000
# LC_AK_2000 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2000-01-01", end_date = "2000-12-31", pixel_x_size = NULL, pizel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2000.tif"))
# 
# ## 2001
# LC_AK_2001 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2001-01-01", end_date = "2001-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2001.tif"))
# 
# ## 2002
# LC_AK_2002 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2002-01-01", end_date = "2002-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2002.tif"))
# 
# ## 2003
# LC_AK_2003 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2003-01-01", end_date = "2003-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2003.tif"))
# 
# ## 2004
# LC_AK_2004 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2004-01-01", end_date = "2004-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2004.tif"))
# 
# ## 2005
# LC_AK_2005 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2005-01-01", end_date = "2005-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2005.tif"))
# 
# ## 2006
# LC_AK_2006 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2006-01-01", end_date = "2006-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2006.tif"))
# 
# ## 2007
# LC_AK_2007 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2007-01-01", end_date = "2007-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2007.tif"))
# 
# ## 2008
# LC_AK_2008 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2008-01-01", end_date = "2008-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2008.tif"))
# 
# ## 2009
# LC_AK_2009 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2009-01-01", end_date = "2009-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2009.tif"))
# 
# ## 2010
# LC_AK_2010 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2010-01-01", end_date = "2010-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2010.tif"))
# 
# ## 2011
# LC_AK_2011 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2011-01-01", end_date = "2011-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2011.tif"))
# 
# ## 2012
# LC_AK_2012 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2012-01-01", end_date = "2012-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2012.tif"))
# 
# ## 2013
# LC_AK_2013 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2013-01-01", end_date = "2013-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2013.tif"))
# 
# ## 2014
# LC_AK_2014 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2014-01-01", end_date = "2014-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2014.tif"))
# 
# ## 2015
# LC_AK_2015 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2015-01-01", end_date = "2015-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2015.tif"))
# 
# ## 2016
# LC_AK_2016 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2016-01-01", end_date = "2016-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2016.tif"))
# 
# ## 2017
# LC_AK_2017 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2017-01-01", end_date = "2017-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2017.tif"))
# 
# ## 2018
# LC_AK_2018 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2018-01-01", end_date = "2018-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2018.tif"))
# 
# ## 2019
# LC_AK_2019 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2019-01-01", end_date = "2019-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2019.tif"))
# 
# ## 2020
# LC_AK_2020 <- get_stac_data(aoi = SnotelInfo_AK_sf, start_date = "2020-01-01", end_date = "2020-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "Alaska", "LC_AK_2020.tif"))
# 
# 
# ### Now Conus
# ## 1992
# LC_CONUS_1992 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "1992-01-01", end_date = "1992-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_1992.tif"))
# 
# 
# ## 1993
# LC_CONUS_1993 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "1993-01-01", end_date = "1993-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_1993.tif"))
# 
# ## 1994
# LC_CONUS_1994 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "1994-01-01", end_date = "1994-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_1994.tif"))
# 
# ## 1995
# LC_CONUS_1995 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "1995-01-01", end_date = "1995-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_1995.tif"))
# 
# ## 1996
# LC_CONUS_1996 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "1996-01-01", end_date = "1996-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_1996.tif"))
# 
# ## 1997
# LC_CONUS_1997 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "1997-01-01", end_date = "1997-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_1997.tif"))
# 
# ## 1998
# LC_CONUS_1998 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "1998-01-01", end_date = "1998-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_1998.tif"))
# 
# ## 1999
# LC_CONUS_1999 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "1999-01-01", end_date = "1999-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_1999.tif"))
# 
# ## 2000
# LC_CONUS_2000 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2000-01-01", end_date = "2000-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2000.tif"))
# 
# ## 2001
# LC_CONUS_2001 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2001-01-01", end_date = "2001-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2001.tif"))
# 
# ## 2002
# LC_CONUS_2002 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2002-01-01", end_date = "2002-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2002.tif"))
# 
# ## 2003
# LC_CONUS_2003 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2003-01-01", end_date = "2003-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2003.tif"))
# 
# ## 2004
# LC_CONUS_2004 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2004-01-01", end_date = "2004-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2004.tif"))
# 
# ## 2005
# LC_CONUS_2005 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2005-01-01", end_date = "2005-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2005.tif"))
# 
# ## 2006
# LC_CONUS_2006 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2006-01-01", end_date = "2006-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2006.tif"))
# 
# ## 2007
# LC_CONUS_2007 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2007-01-01", end_date = "2007-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2007.tif"))
# 
# ## 2008
# LC_CONUS_2008 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2008-01-01", end_date = "2008-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2008.tif"))
# 
# ## 2009
# LC_CONUS_2009 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2009-01-01", end_date = "2009-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2009.tif"))
# 
# ## 2010
# LC_CONUS_2010 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2010-01-01", end_date = "2010-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2010.tif"))
# 
# ## 2011
# LC_CONUS_2011 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2011-01-01", end_date = "2011-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2011.tif"))
# 
# ## 2012
# LC_CONUS_2012 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2012-01-01", end_date = "2012-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2012.tif"))
# 
# ## 2013
# LC_CONUS_2013 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2013-01-01", end_date = "2013-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2013.tif"))
# 
# ## 2014
# LC_CONUS_2014 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2014-01-01", end_date = "2014-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2014.tif"))
# 
# ## 2015
# LC_CONUS_2015 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2015-01-01", end_date = "2015-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2015.tif"))
# 
# ## 2016
# LC_CONUS_2016 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2016-01-01", end_date = "2016-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2016.tif"))
# 
# ## 2017
# LC_CONUS_2017 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2017-01-01", end_date = "2017-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2017.tif"))
# 
# ## 2018
# LC_CONUS_2018 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2018-01-01", end_date = "2018-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2018.tif"))
# 
# ## 2019
# LC_CONUS_2019 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2019-01-01", end_date = "2019-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2019.tif"))
# 
# ## 2020
# LC_CONUS_2020 <- get_stac_data(aoi = SnotelInfo_CONUS_sf, start_date = "2020-01-01", end_date = "2020-12-31", pixel_x_size = NULL, pixel_y_size = NULL, asset_names = "lccs_class", stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/", collection = "esa-cci-lc", output_filename = here("Data", "Landcover", "CONUS", "LC_CONUS_2020.tif"))

### Land Cover end time
LCEndTime <- Sys.time()
LCDownloadTime <- LCEndTime - LCStartTime

##### End of Script #####
TotalEndTime <- Sys.time()
TotalTime <- TotalEndTime - TotalStartTime
SnotelDownloadTime
DaymetTileDownloadTime
DEMDownloadTime
LCDownloadTime
TotalTime
