library(enerscape)
library(data.table)
library(terra)
library(tidyverse)
library(mapview)
library("furrr")

moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #Mollweide metric whatever 


##### Water
esa_water_r <- rast("data/spatial_data/covariates/raster/esa_wc_water_2021_10m.tif")

data_type_water <- terra::datatype(esa_water_r)

water_trans_r <- project(esa_water_r, 
                       y = moll_crs,
                       method = "max", 
                       filename= "data/spatial_data/covariates/raster/esa_wc_water_2021_10m_mollweide.tif",
                       threads = 5, 
                       datatype = data_type_water)

water_trans_r <- rast("data/spatial_data/covariates/raster/esa_wc_water_2021_10m_mollweide.tif")

water_ext <- ext(water_trans_r)

tile_template <- rast(ext = water_ext, resolution = 500000, crs = moll_crs) #500 km tiles

water_tiles <- getTileExtents(water_trans_r, tile_template, buffer=2)

plan(multisession, workers = 10)

water_res <- future_map(1:nrow(water_tiles), function(i){
  
  water_trans_r <- terra::rast("data/spatial_data/covariates/raster/esa_wc_water_2021_10m_mollweide.tif")
  data_type_water <- terra::datatype(water_trans_r)
  
  
  water_tile <- terra::crop(water_trans_r, terra::ext(water_tiles[i,])) 
  
  
  water_binary_r <- ifel(water_tile == 1, 1, NA, 
                         filename = paste0("data/spatial_data/raw_tiles/water_tile_", i, ".tif"),
                         overwrite = TRUE,
                         datatype = data_type_water)
  plot(water_binary_r)
 
  
  print(paste0(i, "/", nrow(water_tiles), " done"))
  
  dt_res <- data.frame(done = i)
  return(dt_res)
}, .progress = TRUE, .options = furrr_options(seed = TRUE))


## Combine water
water_files <- list.files("data/spatial_data/raw_tiles", full.names = T, pattern = "water")

water_raster_list <- lapply(water_files, rast)

water_r <- merge(sprc(water_raster_list), 
              filename = "data/spatial_data/covariates/raster/esa_wc_water_2021_10m_1_or_NA.tif", 
              overwrite = TRUE, 
              datatype = data_type_water)
plot(water_r)
file.remove(water_files)


#### WSF
wsf_r <- rast("data/spatial_data/covariates/raster/world_settlement_footprint_2015_10m.tif")

data_type_wsf <- terra::datatype(wsf_r)



wsf_100m_r <- terra::aggregate(wsf_r, 
                               fact = 10, 
                               fun = "max", 
                               na.rm = TRUE,
                               filename = "data/spatial_data/covariates/raster/world_settlement_footprint_2015_100m.tif", 
                               overwrite = TRUE)



wsf_trans_r <- terra::project(x = wsf_100m_r, 
                         moll_crs,
                         method = "max", 
                         tempdir = "data/spatial_data/terra_temp_dir", 
                         filename= "data/spatial_data/covariates/raster/world_settlement_footprint_2015_100m_mollweide.tif",
                         threads = 5, 
                         memfrac = 0.4)

#wsf_trans_r <- rast("data/spatial_data/covariates/raster/world_settlement_footprint_2015_10m_mollweide.tif")

wsf_ext <- ext(wsf_trans_r)

tile_template <- rast(ext = wsf_ext, resolution = 500000, crs = moll_crs) #500 km tiles

wsf_tiles <- getTileExtents(wsf_trans_r, tile_template, buffer=2)

plan(multisession, workers = 10)

wsf_res <- future_map(1:nrow(wsf_tiles), function(i){
  
  wsf_trans_r <- terra::rast("data/spatial_data/covariates/raster/world_settlement_footprint_2015_100m_mollweide.tif")
  data_type_wsf <- terra::datatype(wsf_trans_r)
  
  
  wsf_tile <- terra::crop(wsf_trans_r, terra::ext(wsf_tiles[i,])) 
  
  
  wsf_binary_r <- ifel(wsf_tile == 1, 1, NA, 
                         filename = paste0("data/spatial_data/raw_tiles/wsf_tile_", i, ".tif"),
                         overwrite = TRUE,
                         datatype = data_type_wsf)
  plot(wsf_binary_r)
  
  
  print(paste0(i, "/", nrow(wsf_tiles), " done"))
  
  dt_res <- data.frame(done = i)
  return(dt_res)
}, .progress = TRUE, .options = furrr_options(seed = TRUE))


## Combine wsf
wsf_files <- list.files("data/spatial_data/raw_tiles", full.names = T, pattern = "wsf")

wsf_raster_list <- lapply(wsf_files, rast)

wsf_r <- merge(sprc(wsf_raster_list), 
                 filename = "data/spatial_data/covariates/raster/world_settlement_footprint_2015_100m_1_or_NA.tif", 
                 overwrite = TRUE, 
                 tempdir = "data/spatial_data/terra_temp_dir", 
                 datatype = data_type_wsf)
plot(wsf_r)
file.remove(wsf_files)
