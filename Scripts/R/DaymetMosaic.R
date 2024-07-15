##### Script for Mosaicing Daymet data #####
library(pacman)
p_load(here, tidyverse, sf, terra, ncdf4, parallel, future)
future::plan("multisession")

##### hold on I'm trying some bullshit #####
poopypants <- rast(here("Data", "Daymet", "Alaska", "prcp_1992_13329.nc"))
poopypants_trim <- poopypants[[275:nlyr(poopypants)]]
names(poopypants_trim) <- 1:nlyr(poopypants_trim)
poopypants2 <- rast(here("Data", "Daymet", "Alaska", "prcp_1993_13329.nc"))
poopypants2_trim <- poopypants2[[1:273]]
names(poopypants2_trim) <- (nlyr(poopypants_trim)+1):(nlyr(poopypants_trim) + nlyr(poopypants2_trim))
poopypants_trim
poopypants2_trim

### leap years to non-leap years: 275 of prev year to 273 of water year
### non-leap year to non-leap year: 274 of PY to 273 of WY
### non-leap year to leap year: 274 of PY to 274 of WY

poopypants3 <- rast(here("Data", "Daymet", "Alaska", "prcp_1995_13329.nc"))
poopypants3_trim <- poopypants3[[274:nlyr(poopypants3)]]
names(poopypants3_trim) <- 1:nlyr(poopypants3_trim)
poopypants4 <- rast(here("Data", "Daymet", "Alaska", "prcp_1996_13329.nc"))
poopypants4_trim <- poopypants4[[1:274]]

poopypants3_trim
poopypants4_trim

stringvect1 <- c("tile1")
stringvect2 <- c("tile4")
stringvect3 <- c(stringvect1, stringvect2)



poopypants_merge <- c(poopypants_trim, poopypants2_trim)
poopypants_merge
plot(poopypants_merge$"276")

poopypants_mean <- app(poopypants_merge, fun = mean)
poopypants_mean
plot(poopypants_mean)

##### Calculating tmean #####
for (daymetyear in 1992:2023){
  currentyear <- as.character(daymetyear)
  print(currentyear)
  ### identifying tiles that are the correct variable and in the same year
  tminfiles_temp <- list.files(path = here("Data", "Daymet", "Alaska"), pattern = paste0("^", "tmin", "_", currentyear, "_", "*"), all.files= T, full.names= T)
  tmaxfiles_temp <- list.files(path = here("Data", "Daymet", "Alaska"), pattern = paste0("^", "tmax", "_", currentyear, "_", "*"), all.files= T, full.names= T)
  for (i in 1:length(tminfiles_temp)){
    ### grabbing the tile id
    ## only need to do this once, it's for when I save the tmean netCDF
    tmin_filename_temp <- tminfiles_temp[i]
    tmin_fileextension <- str_split_1(tmin_filename_temp, pattern = paste0("tmin_", currentyear, "_"))[[2]]
    tileid <- str_split_1(tmin_fileextension, pattern = ".nc")[[1]]
    ### reading in netCDF files
    tmin_temp <- rast(tminfiles_temp[i])
    tmax_temp <- rast(tmaxfiles_temp[i])
    tmean_temp <- mean(tmin_temp, tmax_temp)
    varnames(tmean_temp) <- "tmean (daily average of min and max temps)"
    writeCDF(tmean_temp, here("Data", "Daymet", "Alaska", paste0("tmean_", currentyear, "_", tileid, ".nc")))
  }
}



##### Aggregating Daymet data so it is only one layer #####





### Daymet mosaicing start time
DaymetStartTime <- Sys.time()
##### Daymet Mosaicing #####
### Mosaicing daymet tiles by variable for Alaska
# for (daymetyear in 1992:2023){
#   currentyear <- as.character(daymetyear)
#   print(currentyear)
#   for (variable in c("prcp", "tmin", "tmax", "srad")){
#     ### identifying tiles that are the correct variable and in the same year
#     files_temp <- list.files(path = here("Data", "Daymet", "Alaska"), pattern = paste0("^", variable, "_", currentyear, "_", "*"), all.files= T, full.names= T)
#     ### reading in tiles as spatrasters, making into spatraster collection, writing mosaiced netCDF file to new folder for easier access
#     nc_rasttemp <- lapply(files_temp, rast)
#     nc_sprctemp <- sprc(nc_rasttemp)
#     nc_mosaictemp <- mosaic(nc_sprctemp, overwrite = TRUE)
#     writeCDF(nc_mosaictemp, here("Data", "Daymet", "Alaska", "Mosaics", paste0(variable, "_", currentyear, "_Mosaic.nc")))
#   }
# }


### Mosaicing daymet tiles by variable for CONUS
# for (daymetyear in 1992:2023){
#   currentyear <- as.character(daymetyear)
#   print(currentyear)
#   for (variable in c("prcp", "tmin", "tmax", "srad")){
#     ### identifying tiles that are the correct variable and in the same year
#     files_temp <- list.files(path = here("Data", "Daymet", "CONUS"), pattern = paste0("^", variable, "_", currentyear, "_", "*"), all.files= T, full.names= T)
#     ### reading in tiles as spatrasters, making into spatraster collection, writing mosaiced netCDF file to new folder for easier access
#     nc_rasttemp <- lapply(files_temp, rast)
#     nc_sprctemp <- sprc(nc_rasttemp)
#     nc_mosaictemp <- mosaic(nc_sprctemp)
#     writeCDF(nc_mosaictemp, here("Data", "Daymet", "CONUS", "Mosaics", paste0(variable, "_", currentyear, "_Mosaic.nc")))
#   }
# }



### Daymet mosaicing end time
DaymetEndTime <- Sys.time()
DaymetMosaicTime <- DaymetEndTime - DaymetStartTime
DaymetMosaicTime