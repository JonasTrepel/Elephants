
library(terra)

moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #Mollweide metric whatever 

##### Distance to Water --------------------
r_water <- rast("data/spatial_data/covariates/raster/esa_wc_water_2021_10m_1_or_NA.tif")

plot(r_water)

r_water_moll <- project(r_water, 
                        y = moll_crs,
                        method = "mode", 
                        threads = 4)
plot(r_water_moll)

r_dist_water <- distance(r_water_moll,
                         gdal = c("BIGTIFF=YES"))/1000 #get distance in km

plot(r_dist_water)

writeRaster(r_dist_water,
            "data/spatial_data/covariates/raster/distance_to_water_km.tif", 
            overwrite = TRUE)

##### Distance to Settlement --------------------
r_wsf <- rast("data/spatial_data/covariates/raster/...")

plot(r_wsf)

r_wsf_moll <- project(r_wsf, 
                        y = moll_crs,
                        method = "mode", 
                        threads = 4)
plot(r_wsf_moll)

r_dist_settlement <- distance(r_wsf_moll,
                         gdal = c("BIGTIFF=YES"))/1000 #get distance in km

plot(r_dist_settlement)

writeRaster(r_dist_settlement,
            "data/spatial_data/covariates/raster/distance_to_settlement_km.tif", 
            overwrite = TRUE)


