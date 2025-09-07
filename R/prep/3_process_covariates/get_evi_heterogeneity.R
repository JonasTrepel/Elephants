library(terra)
library(future)
library(furrr)

evi_files <- list.files("data/spatial_data/time_series",
                        full.names = TRUE, pattern = "hsl_clamped_evi")
evi_files <- evi_files[grepl("30m", evi_files)]

terraOptions(memfrac = 0.5)


plan(multisession, workers = 6)

future_walk(1:length(evi_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- evi_files[i]
              og_r <- rast(file)
              #plot(og_r)
              
              data_type <- terra::datatype(og_r)
              
              years <- gsub("data/spatial_data/time_series/hsl_clamped_evi_", "", file)
              years <- gsub("_30m.tif", "", years)
              
              #aggregate at 90 
              evi_mean_90m <- terra::aggregate(og_r, 
                                               fact = 3, 
                                               fun = "mean", 
                                               filename = paste0(
                                                 "data/spatial_data/time_series/hsl_clamped_evi_",
                                                 years, "_90m.tif"), 
                                               cores = 1, 
                                               overwrite = T)
              
              evi_sd_90m <- terra::aggregate(og_r, 
                                               fact = 3, 
                                               fun = "sd", 
                                               filename = paste0(
                                                 "data/spatial_data/time_series/hsl_sd_clamped_evi_",
                                                 years, "_90m.tif"), 
                                               cores = 1, 
                                               overwrite = T)
            
              
              evi_sd_900m <- terra::aggregate(evi_mean_90m, 
                                             fact = 10, 
                                             fun = "sd", 
                                             filename = paste0(
                                               "data/spatial_data/time_series/hsl_sd_clamped_evi_",
                                               years, "_900m.tif"), 
                                             cores = 1, 
                                             overwrite = T)
              #plot(fraction_mode_100m_r)
            })
plant(sequential)