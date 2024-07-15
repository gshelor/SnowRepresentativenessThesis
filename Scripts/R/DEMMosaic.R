##### Script for Mosaicing and Cleaning DEM and Daymet Datasets #####
### Script by Griffin Shelor
### start time
TotalStartTime <- Sys.time()

##### loading packages #####
library(pacman)
p_load(here, tidyverse, sf, terra, ncdf4, parallel, future)
future::plan("multisession")


##### Alaska DEM Mosaicing #####
### making list of all the different tif tiles
# AKrastlist <- list.files(path = here("Data", "DEM", "Alaska", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)

### converting all the files to spatraster
# AK_rasts <- lapply(AKrastlist, rast)

### making a spatraster collection
# AKdem_sprc <- sprc(AK_rasts)
### mosaicing dem_sprc instead of using for loop to merge
# AKdem_rast <- mosaic(AKdem_sprc)

### writing AKdem_rast to a tif file
# writeRaster(AKdem_rast, here("Data", "DEM", "Alaska", "AKDEMMosaic.tif"))


##### CONUS DEM Mosaicing #####
### making lists of all the different tif tiles for each state
# CONUSrastlist <- list.files(path = here("Data", "DEM", "CONUS", "Washington", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist2 <- list.files(path = here("Data", "DEM", "CONUS", "Oregon", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist3 <- list.files(path = here("Data", "DEM", "CONUS", "California", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist4 <- list.files(path = here("Data", "DEM", "CONUS", "NewMexico", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist5 <- list.files(path = here("Data", "DEM", "CONUS", "Arizona", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist6 <- list.files(path = here("Data", "DEM", "CONUS", "Colorado", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist7 <- list.files(path = here("Data", "DEM", "CONUS", "Wyoming", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist8 <- list.files(path = here("Data", "DEM", "CONUS", "Nevada", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist9 <- list.files(path = here("Data", "DEM", "CONUS", "Utah", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist10 <- list.files(path = here("Data", "DEM", "CONUS", "Idaho", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist11 <- list.files(path = here("Data", "DEM", "CONUS", "Montana", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist12 <- list.files(path = here("Data", "DEM", "CONUS", "SouthDakota", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist13 <- list.files(path = here("Data", "DEM", "CONUS", "NorthDakota", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)
# CONUSrastlist14 <- list.files(path = here("Data", "DEM", "CONUS", "Nebraska", "nasadem-cog", "v001"), pattern='.tif$', all.files= T, full.names= T)

### converting all the files to spatraster
# CONUS_rasts <- lapply(c(CONUSrastlist, CONUSrastlist2, CONUSrastlist3, CONUSrastlist4, CONUSrastlist5, CONUSrastlist6, CONUSrastlist7, CONUSrastlist8, CONUSrastlist9, CONUSrastlist10, CONUSrastlist11, CONUSrastlist12, CONUSrastlist13, CONUSrastlist14), rast)

### making a spatraster collection
# CONUSdem_sprc <- sprc(CONUS_rasts)
### mosaicing spatraster collection
# CONUSdem_rast <- mosaic(CONUSdem_sprc)

### writing CONUSdem_rast to a tif file
# writeRaster(CONUSdem_rast, here("Data", "DEM", "CONUS", "CONUSDEMMosaic.tif"))

### DEM mosaicing end time
TotalEndTime <- Sys.time()
TotalTime <- TotalEndTime - TotalStartTime
TotalTime
