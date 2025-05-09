library(enerscape)
library(data.table)
library(terra)
library(tidyverse)
library(mapview)
library("furrr")

moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #Mollweide metric whatever 

###### Start 

dem_r <- rast("data/spatial_data/covariates/raster/nasa_dem_30m.tif")

dem_trans_r <- project(dem_r, 
                       y = moll_crs,
                       method = "bilinear", 
                       filename= "data/spatial_data/covariates/raster/nasa_dem_30m_mollweide.tif",
                       threads = 5)

dem_trans_r <- rast("data/spatial_data/covariates/raster/nasa_dem_30m_mollweide.tif")

dem_90m_r <- terra::aggregate(dem_trans_r, 
                                 fact = 3, 
                                 fun = "mean", 
                                 na.rm = TRUE,
                                 filename = "data/spatial_data/covariates/raster/nasa_dem_90m.tif", 
                                 overwrite = TRUE)

ext <- ext(dem_90m_r)

tile_template <- rast(ext = ext, resolution = 1000000, crs = moll_crs) #1000 km tiles

tiles <- getTileExtents(dem_90m_r, tile_template, buffer=2)

plan(multisession, workers = 10)

dt_res <- future_map(1:nrow(tiles), function(i){
  
  dem_90m_r <- terra::rast("data/spatial_data/covariates/raster/nasa_dem_90m.tif")

  dem_tile <- terra::crop(dem_90m_r, terra::ext(tiles[i,])) 
  
  ## Enerscape 
  en_tile <- enerscape::enerscape(dem = dem_tile, 
                m = 4400, 
                unit = "kcal")
  plot(en_tile)

  en_filename <- paste0("data/spatial_data/raw_tiles/energyscape_kcal_tile_", i, ".tif")
  terra::writeRaster(en_tile, filename = en_filename, overwrite = TRUE)
  
  ## Slope 
  slope_tile <- terra::terrain(dem_tile,
                            filename = paste0("data/spatial_data/raw_tiles/slope_tile_", i, ".tif"),  
                            v = "slope")
  plot(slope_tile)

  print(paste0(i, "/", nrow(tiles), " done"))
  
  dt_res <- data.frame(done = i)
  return(dt_res)
}, .progress = TRUE, .options = furrr_options(seed = TRUE))


## Combine energy lanscape scape
en_files <- list.files("data/spatial_data/raw_tiles", full.names = T, pattern = "energyscape")

en_raster_list <- lapply(en_files, rast)

en_r <- merge(sprc(en_raster_list), 
               filename = "data/spatial_data/covariates/raster/energyscape_kcal.tif", 
               overwrite = TRUE, 
              progress = TRUE, 
              tempdir = "data/spatial_data/terra_temp_dir",
              memfrac = 0.4)
plot(en_r)
plot(log(en_r))
file.remove(en_files)

## Combine slope
slope_files <- list.files("data/spatial_data/raw_tiles", full.names = T, pattern = "slope")

slope_raster_list <- lapply(slope_files, rast)

slope_r <- merge(sprc(slope_raster_list), 
              filename = "data/spatial_data/covariates/raster/slope_degree.tif",
              overwrite = TRUE, 
              progress = TRUE, 
              tempdir = "data/spatial_data/terra_temp_dir",
              memfrac = 0.4)
plot(slope_r)
file.remove(slope_files)

