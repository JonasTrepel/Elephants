library(terra)
library(data.table)
library(tidyverse)
library(furrr)
library(future)

        
        

#get our time series (response) variables at multiple scales 

#1. Veg covers  -------
  #a. Grass cover 
  #b. Grass and crop cover 
  #c. Shrub cover 
  #d. Tree cover 
  #e. Bare Ground cover

grass_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                                           pattern = "grass_cover", 
                                                                           full.names = TRUE), 
                                                     filename = list.files("data/spatial_data/time_series/",
                                                                           pattern = "grass_cover", 
                                                                           full.names = FALSE)
) %>% 
  filter(grepl("100m", filename)) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename, 
         years = gsub("grass_cover_", "", filename), 
         new_filename = paste0("data/spatial_data/time_series/grass_cover_", years, "_1000m.tif"))


gr_n_cr_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                                           pattern = "gr_n_cr_cover", 
                                                                           full.names = TRUE), 
                                                     filename = list.files("data/spatial_data/time_series/",
                                                                           pattern = "gr_n_cr_cover", 
                                                                           full.names = FALSE)
) %>% 
  filter(grepl("100m", filename)) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename, 
         years = gsub("gr_n_cr_cover", "", filename), 
         new_filename = paste0("data/spatial_data/time_series/gr_n_cr_cover", years, "_1000m.tif"))


shrub_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                                             pattern = "shrub_cover", 
                                                                             full.names = TRUE), 
                                                       filename = list.files("data/spatial_data/time_series/",
                                                                             pattern = "shrub_cover", 
                                                                             full.names = FALSE)
) %>% 
  filter(grepl("100m", filename)) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename, 
         years = gsub("shrub_cover", "", filename), 
         new_filename = paste0("data/spatial_data/time_series/shrub_cover", years, "_1000m.tif"))


tree_cover_files <-  data.table(filepath = list.files("data/spatial_data/time_series/",
                                                                           pattern = "tree_cover", 
                                                                           full.names = TRUE), 
                                                     filename = list.files("data/spatial_data/time_series/",
                                                                           pattern = "tree_cover", 
                                                                           full.names = FALSE)
) %>% 
  filter(grepl("100m", filename)) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename, 
         years = gsub("tree_cover", "", filename), 
         new_filename = paste0("data/spatial_data/time_series/tree_cover", years, "_1000m.tif"))


bare_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                                          pattern = "bare_cover", 
                                                                          full.names = TRUE), 
                                                    filename = list.files("data/spatial_data/time_series/",
                                                                          pattern = "bare_cover", 
                                                                          full.names = FALSE)
) %>% 
  filter(grepl("100m", filename)) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename, 
         years = gsub("bare_cover", "", filename), 
         new_filename = paste0("data/spatial_data/time_series/bare_cover", years, "_1000m.tif"))


veg_cover_files <- rbind(grass_cover_files, 
                         gr_n_cr_cover_files, 
                         shrub_cover_files, 
                         tree_cover_files, 
                         bare_cover_files)

plan(multisession, workers = 5)

future_walk(1:nrow(veg_cover_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- veg_cover_files[i, ]$filepath
              new_file <- veg_cover_files[i, ]$new_filename
              og_r <- rast(file)
              #plot(og_r)
              
              data_type_og_r <- terra::datatype(og_r)
              
              years <- veg_cover_files[i, ]$years
              
              #aggregate at 1000m 
              agg_1000m_r <- terra::aggregate(og_r,
                                              fact = 10,
                                              fun = "mean", 
                                              na.rm=TRUE,
                                              filename = new_file, 
                                              cores = 1, 
                                              overwrite = T)
            })
plan(sequential)


#6. Evi ------------------------------------


evi_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                              pattern = "hsl_clamped_evi_", 
                                              full.names = TRUE), 
                        filename = list.files("data/spatial_data/time_series/",
                                              pattern = "hsl_clamped_evi_", 
                                              full.names = FALSE)) %>% 
  filter(grepl("90m", filename)) %>% 
  mutate(filename = gsub("_90m.tif", "", filename),
         colname =  gsub("hsl_clamped_evi_", "evi_", filename), 
         years = gsub("hsl_clamped_evi", "", filename), 
         new_filename = paste0("data/spatial_data/time_series/hls_clamped_evi", years, "_900m.tif"))


plan(multisession, workers = 6)

future_walk(1:nrow(evi_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- evi_files[i, ]$filepath
              new_file <- evi_files[i, ]$new_filename
              og_r <- rast(file)
              #plot(og_r)
              
              data_type_og_r <- terra::datatype(og_r)
              
              years <- evi_files[i, ]$years
              
              #aggregate at 900m 
              agg_900m_r <- terra::aggregate(og_r,
                                              fact = 10,
                                              fun = "mean", 
                                              na.rm=TRUE,
                                              filename = new_file, 
                                              cores = 1, 
                                              overwrite = T)
             # plot(agg_900m_r)
            })
plan(sequential)

#7. Canopy height ------------------


canopy_height_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                              pattern = "canopy_height", 
                                              full.names = TRUE), 
                        filename = list.files("data/spatial_data/time_series/",
                                              pattern = "canopy_height", 
                                              full.names = FALSE)) %>% 
  filter(grepl("30m", filename)) %>% 
  filter(!grepl("sd", filename)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("_30m", "", filename), 
         years = gsub("canopy_height_", "", colname)) #%>% filter(years %in% c("2002", "2005", "2008", "2013", "2016", "2019", "2022"))


plan(multisession, workers = 6)

furrr::future_walk(1:nrow(canopy_height_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- canopy_height_files[i, ]$filepath
              og_r <- rast(file)
              #plot(og_r)
              
              data_type_og_r <- terra::datatype(og_r)
              
              year <- canopy_height_files[i, ]$years
              
              #aggregate at 90m 
              agg_90m_r <- terra::aggregate(og_r,
                                             fact = 3,
                                             fun = "median", 
                                             na.rm=TRUE,
                                             filename = paste0(
                                               "data/spatial_data/time_series/canopy_height_", 
                                               year, "_90m.tif"),  
                                             cores = 1, 
                                             overwrite = T)
              #plot(agg_90m_r)
              
              #aggregate at 900m
              agg_900m_r <- terra::aggregate(og_r,
                                            fact = 30,
                                            fun = "median", 
                                            na.rm=TRUE,
                                            filename = paste0(
                                              "data/spatial_data/time_series/canopy_height_", 
                                              year, "_900m.tif"),                                              cores = 1, 
                                            overwrite = T)
              #plot(agg_900m_r)
            })
plan(sequential)

#7.2 Tree cover from canopy height ------------------



canopy_height_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                        pattern = "canopy_height", 
                                                        full.names = TRUE), 
                                  filename = list.files("data/spatial_data/time_series/",
                                                        pattern = "canopy_height", 
                                                        full.names = FALSE)) %>% 
  filter(grepl("30m", filename)) %>% 
  filter(!grepl("sd", filename)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("_30m", "", filename), 
         years = gsub("canopy_height_", "", colname)) #%>% filter(years %in% c("2002", "2005", "2008", "2013", "2016", "2019", "2022"))


ssa_ext <- ext(-17.5, 51.0, -35.0, 16.0)
ssa_vect <- as.polygons(ssa_ext, crs = "EPSG:4326")

plan(multisession, workers = 6)

future_walk(1:nrow(canopy_height_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- canopy_height_files[i, ]$filepath
              og_r <- rast(file)
              
              ssa_ext <- ext(-17.5, 51.0, -35.0, 16.0)
              og_r <- crop(og_r, ssa_ext)
              #plot(og_r)
              
              data_type_og_r <- terra::datatype(og_r)
              
              year <- canopy_height_files[i, ]$years
              
              tree_r <- (og_r > 2)
              
              rm(og_r)
              
              #aggregate at 90m 
              agg_300m_r <- terra::aggregate(tree_r,
                                            fact = 10,
                                            fun = "mean", 
                                            na.rm=TRUE,
                                            filename = paste0(
                                              "data/spatial_data/time_series/hunter_woody_cover_", 
                                              year, "_300m.tif"),  
                                            cores = 1, 
                                            overwrite = T)
              #plot(agg_300m_r)
              rm(agg_300m_r)
              
              #aggregate at 900m
              agg_900m_r <- terra::aggregate(tree_r,
                                             fact = 30,
                                             fun = "mean", 
                                             na.rm=TRUE,
                                             filename = paste0(
                                               "data/spatial_data/time_series/hunter_woody_cover_", 
                                               year, "_900m.tif"),                                              cores = 1, 
                                             overwrite = T)
              #plot(agg_900m_r)
              rm(agg_900m_r)
              gc()
              
            })
plan(sequential)


hunter_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                 pattern = "hunter", 
                                                 full.names = TRUE), 
                           filename = list.files("data/spatial_data/time_series/",
                                                 pattern = "hunter", 
                                                 full.names = FALSE)) 

for(i in 1:nrow(hunter_files)){
  r <- rast(hunter_files[i,]$filepath)
  plot(r, main = paste0(hunter_files[i,]$filename))
  Sys.sleep(1)
  
}



#8. Canopy height SD 90m ------------------


canopy_height_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                        pattern = "canopy_height", 
                                                        full.names = TRUE), 
                                  filename = list.files("data/spatial_data/time_series/",
                                                        pattern = "canopy_height", 
                                                        full.names = FALSE)) %>% 
  filter(grepl("30m", filename)) %>% 
  filter(!grepl("sd", filename)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("_30m", "", filename), 
         years = gsub("canopy_height_", "", colname)) #%>% filter(years %in% c("2002", "2005", "2008", "2013", "2016", "2019", "2022"))


plan(multisession, workers = 6)

future_walk(1:nrow(canopy_height_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- canopy_height_files[i, ]$filepath
              og_r <- rast(file)
              #plot(og_r)
              
              data_type_og_r <- terra::datatype(og_r)
              
              year <- canopy_height_files[i, ]$years
              
              #aggregate at 90m 
              agg_90m_r <- terra::aggregate(og_r,
                                            fact = 3,
                                            fun = "sd", 
                                            na.rm=TRUE,
                                            filename = paste0(
                                              "data/spatial_data/time_series/canopy_height_sd_", 
                                              year, "_90m.tif"),  
                                            cores = 1, 
                                            overwrite = T)
              #plot(agg_90m_r)
              rm(agg_90m_r)
              
            })
plan(sequential)


#9. Canopy height SD 900m ------------------


canopy_height_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                        pattern = "canopy_height", 
                                                        full.names = TRUE), 
                                  filename = list.files("data/spatial_data/time_series/",
                                                        pattern = "canopy_height", 
                                                        full.names = FALSE)) %>% 
  filter(grepl("90m", filename)) %>% 
  filter(!grepl("sd", filename)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("_90m", "", filename), 
         years = gsub("canopy_height_", "", colname)) #%>% filter(years %in% c("2002", "2005", "2008", "2013", "2016", "2019", "2022"))


plan(multisession, workers = 6)

future_walk(1:nrow(canopy_height_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- canopy_height_files[i, ]$filepath
              og_r <- rast(file)
              #plot(og_r)
              
              data_type_og_r <- terra::datatype(og_r)
              
              year <- canopy_height_files[i, ]$years
              
              #aggregate at 90m 
              agg_90m_r <- terra::aggregate(og_r,
                                            fact = 10,
                                            fun = "sd", 
                                            na.rm=TRUE,
                                            filename = paste0(
                                              "data/spatial_data/time_series/canopy_height_sd_", 
                                              year, "_900m.tif"),  
                                            cores = 1, 
                                            overwrite = T)
              #plot(agg_90m_r)
              rm(agg_90m_r)
              
            })
plan(sequential)