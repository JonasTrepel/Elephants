library(enerscape)
library(data.table)
library(terra)
library(tidyverse)
library(mapview)
library(starsExtra)

dem_r <- rast("data/spatial_data/covariates/raster/nasa_dem_30m.tif")

#transform to metric crs
target_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #Mollweide metric whatever 
dem_trans <- project(dem_r, 
                     y = target_crs,
                     method = "bilinear", 
                     threads = 4)

#get energy landscape 
en <- enerscape(dem = dem_trans, 
                m = 4400, 
                unit = "kcal")
plot(en)
writeRaster(en, "data/spatial_data/covariates/raster/energyscape_kcal.tif", overwrite = TRUE)

#get slope 
slope <- terra::terrain(dem_trans,
                        v = "slope",
                        filename = "data/spatial_data/covariates/raster/slope_degree.tif", 
                        overwrite = TRUE)
plot(slope)

