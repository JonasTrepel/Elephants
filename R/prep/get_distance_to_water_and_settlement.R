
library(terra)


##### Distance to Water --------------------


water_trans_r <- rast("data/spatial_data/covariates/raster/esa_wc_water_2021_10m_1_or_NA.tif")

plot(water_trans_r)

water_100m_r <- terra::aggregate(water_trans_r, 
                            fact = 10, 
                            fun = "max", 
                            na.rm = TRUE,
                            filename = "data/spatial_data/covariates/raster/esa_wc_water_2021_100m.tif", 
                            overwrite = TRUE)

r_dist_water <- distance(water_100m_r,
                         gdal = c("BIGTIFF=YES"))/1000 #get distance in km

plot(r_dist_water)

writeRaster(r_dist_water,
            "data/spatial_data/covariates/raster/distance_to_water_km.tif", 
            overwrite = TRUE)

##### Distance to Settlement --------------------



wsf_100m_r <- rast("data/spatial_data/covariates/raster/world_settlement_footprint_2015_100m_1_or_NA.tif")

plot(wsf_100m_r)


r_dist_wsf <- distance(wsf_100m_r,
                         gdal = c("BIGTIFF=YES"))/1000 #get distance in km

plot(r_dist_wsf)

writeRaster(r_dist_wsf,
            "data/spatial_data/covariates/raster/distance_to_settlement_km.tif", 
            overwrite = TRUE)

