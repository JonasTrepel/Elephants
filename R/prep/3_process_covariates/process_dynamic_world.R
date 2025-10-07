library(data.table)
library(terra)
library(tidyverse)
library(mapview)
library("furrr")
library(vegan)



# Get percentage cover of the different habitat types 
dynamic_world_files <- list.files("data/spatial_data/time_series",
                                  full.names = T, pattern = "dynamic_world")

terraOptions(memfrac = 0.5)

#dynamic_world_files <- dynamic_world_files[7:10]

plan(multisession, workers = 5)

future_walk(1:length(dynamic_world_files),
            .progress = TRUE,
            function(i){
            
#for(file in unique(dynamic_world_files)){
  
file <- dynamic_world_files[i]
  dw_r <- rast(file)
  plot(dw_r)
  
  data_type_dw <- terra::datatype(dw_r)
  
  years <- gsub("data/spatial_data/time_series/dynamic_world_", "", file)
  years <- gsub("_10m.tif", "", years)
  
  #1. --Grass----------
  
  grass_r <- (dw_r == 2)
  plot(grass_r)
  
  writeRaster(
    grass_r,
    filename = paste0("data/spatial_data/time_series/grass_", years,"_10m.tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  grass_cover <- aggregate(
    grass_r, 
    fact = 10, 
    fun = "mean",
    cores = 1, 
    filename = paste0("data/spatial_data/time_series/grass_cover_", years,"_100m.tif"),
    overwrite = TRUE
  )

  print(paste0("Grass finished for ", years, ". Time: ", Sys.time()))
  
  # 2. --Grass and Crops----------
  gr_n_cr_r <- (dw_r == 2 | dw_r == 4)
  plot(gr_n_cr_r)
  
  writeRaster(
    gr_n_cr_r,
    filename = paste0("data/spatial_data/time_series/gr_n_cr_", years, "_10m.tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  gr_n_cr_cover <- aggregate(
    gr_n_cr_r,
    fact = 10,
    fun = "mean",
    cores = 1,
    filename = paste0("data/spatial_data/time_series/gr_n_cr_cover_", years, "_100m.tif"),
    overwrite = TRUE
  )
  
  print(paste0("Grass and crops finished for ", years, ". Time: ", Sys.time()))
  
  
  # 3. --Shrub----------
  shrub_r <- (dw_r == 5)
  plot(shrub_r)
  
  writeRaster(
    shrub_r,
    filename = paste0("data/spatial_data/time_series/shrub_", years, "_10m.tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  shrub_cover <- aggregate(
    shrub_r,
    fact = 10,
    fun = "mean",
    cores = 1,
    filename = paste0("data/spatial_data/time_series/shrub_cover_", years, "_100m.tif"),
    overwrite = TRUE
  )
  
  print(paste0("Shrubs finished for ", years, ". Time: ", Sys.time()))
  
  # 4. --Tree----------
  tree_r <- (dw_r == 1)
  plot(tree_r)
  
  writeRaster(
    tree_r,
    filename = paste0("data/spatial_data/time_series/tree_", years, "_10m.tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  tree_cover <- aggregate(
    tree_r,
    fact = 10,
    fun = "mean",
    cores = 1,
    filename = paste0("data/spatial_data/time_series/tree_cover_", years, "_100m.tif"),
    overwrite = TRUE
  )
  
  print(paste0("Trees finished for ", years, ". Time: ", Sys.time()))
  
  # 5. --Crop----------
  crop_r <- (dw_r == 4)
  plot(crop_r)
  
  writeRaster(
    crop_r,
    filename = paste0("data/spatial_data/time_series/crops_", years, "_10m.tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  crop_cover <- aggregate(
    crop_r,
    fact = 10,
    fun = "mean",
    cores = 1,
    filename = paste0("data/spatial_data/time_series/crops_cover_", years, "_100m.tif"),
    overwrite = TRUE
  )
  
  print(paste0("Crops finished for ", years, ". Time: ", Sys.time()))
  
  
  # 6. --Bare----------
  bare_r <- (dw_r == 7)
  plot(bare_r)
  
  writeRaster(
    bare_r,
    filename = paste0("data/spatial_data/time_series/bare_", years, "_10m.tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  bare_cover <- aggregate(
    bare_r,
    fact = 10,
    fun = "mean",
    cores = 1,
    filename = paste0("data/spatial_data/time_series/bare_cover_", years, "_100m.tif"),
    overwrite = TRUE
  )
  
  print(paste0("Bare ground finished for ", years, ". Time: ", Sys.time()))
  
  # 7 Vegetation types 
  vt_r <- classify(dw_r, rcl = matrix(c(
    1, 1,
    2, 2,
    3, 3,
    5, 5,
    7, 7
  ), ncol = 2, byrow = TRUE),
  others = NA,
  filename = paste0("data/spatial_data/time_series/vegetation_types_", years, "_10m.tif"),
  overwrite = TRUE,
  datatype = data_type_dw)
  
  print(paste0("Vegetation types finished for ", years, ". Time: ", Sys.time()))
  
  # 8. Vegetation Types with Grass + Crops grouped

    vt_r_inc_cr <- classify(dw_r, rcl = matrix(c(
      1, 1,
      2, 2,
      4, 2,
      3, 3,
      5, 5,
      7, 7
    ), ncol = 2, byrow = TRUE),
    others = NA,
    filename = paste0("data/spatial_data/time_series/veg_types_grass_and_crops_joined_", years, "_10m.tif"),
    overwrite = TRUE,
    datatype = data_type_dw)
   
  
   print(paste0("Vegetation types grass and crops grouped finished for ", years, ". Time: ", Sys.time()))
  

print(paste0(years, " done. Time: ", Sys.time()))

rm(dw_r, grass_r, gr_n_cr_r, shrub_r, tree_r, crop_r, bare_r, vt_r, vt_inc_cr_r); gc()

})
print(paste0("All done. Time: ", Sys.time()))

plan(sequential)


#### Tree cover SD ---------------------

tc_files <- list.files("data/spatial_data/time_series", pattern = "tree_2", full.names = T)

terraOptions(memfrac = 0.5)


plan(multisession, workers = 5)

future_walk(1:length(tc_files),
            .progress = TRUE,
            function(i){
              
              file <- tc_files[i]
              dw_r <- rast(file)
              plot(dw_r)
              
              years <- gsub("data/spatial_data/time_series/tree_", "", file)
              years <- gsub("_10m.tif", "", years)

tree_cover_sd <- aggregate(
  dw_r,
  fact = 10,
  fun = "sd",
  cores = 1,
  filename = paste0("data/spatial_data/time_series/tree_cover_sd_", years, "_100m.tif"),
  overwrite = TRUE
)

})

print(paste0("all done ", Sys.time()))
plan(sequential)

#Tree cover SD 1000m

tc_files <- list.files("data/spatial_data/time_series", pattern = "tree_cover_2", full.names = T)
tc_files <- tc_files[grepl("100m", tc_files)]

terraOptions(memfrac = 0.5)


plan(multisession, workers = 5)

future_walk(1:length(tc_files),
            .progress = TRUE,
            function(i){
              
              file <- tc_files[i]
              dw_r <- rast(file)
              plot(dw_r)
              
              years <- gsub("data/spatial_data/time_series/tree_cover_", "", file)
              years <- gsub("_100m.tif", "", years)
              
              tree_cover_sd <- aggregate(
                dw_r,
                fact = 10,
                fun = "sd",
                cores = 1,
                filename = paste0("data/spatial_data/time_series/tree_cover_sd_", years, "_1000m.tif"),
                overwrite = TRUE
              )
              
              #plot(tree_cover_sd)
              
            })

print(paste0("all done ", Sys.time()))
plan(sequential)



#### Habitat diversity -------------------------

vegetation_type_files <- list.files("data/spatial_data/time_series",
                                  full.names = T, pattern = "vegetation_type")

terraOptions(memfrac = 0.5)

# vegetation_type_files <- vegetation_type_files[
#   grepl("2018_2019", vegetation_type_files) |
#     grepl("2019_2020", vegetation_type_files)  | 
#     grepl("2024_2025", vegetation_type_files)]

plan(multisession, workers = 3)

future_walk(1:length(vegetation_type_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- vegetation_type_files[i]
              dw_r <- rast(file)
              plot(dw_r)
              
              data_type_dw <- terra::datatype(dw_r)
              
              years <- gsub("data/spatial_data/time_series/vegetation_types_", "", file)
              years <- gsub("_10m.tif", "", years)
    
    #aggregate at 100m 
    div_100m_r <- aggregate(dw_r, 
                       fact = 10, 
                       fun = function(x){ 
                         #remove NAs
                         x <- x[!is.na(x)]
                         if(length(x) == 0){return(NA)}
                         
                         # calculate shannon diversity 
                         abundances <- table(x)
                         
                         community_matrix <- matrix(abundances, nrow = 1)
                         colnames(community_matrix) <- names(abundances) #probably unnecceary, but just for the record
                         
                         sh_div <- vegan::diversity(community_matrix, index = "shannon")
                         return(sh_div)}, 
                       filename = paste0(
                         "data/spatial_data/time_series/shannon_diversity_habitat_vegetation_types_100m_",
                         years, ".tif"), 
                       cores = 1)
    
    #aggregate at the 1 km scale 
    div_1000m_r <- aggregate(dw_r, 
                            fact = 100, 
                            fun = function(x){ 
                              #remove NAs
                              x <- x[!is.na(x)]
                              if(length(x) == 0){return(NA)}
                              
                              # calculate shannon diversity 
                              abundances <- table(x)
                              
                              community_matrix <- matrix(abundances, nrow = 1)
                              colnames(community_matrix) <- names(abundances) #probably unnecessary, but just for the record
                              
                              sh_div <- vegan::diversity(community_matrix, index = "shannon")
                              return(sh_div)}, 
                            filename = paste0(
                              "data/spatial_data/time_series/shannon_diversity_habitat_vegetation_types_1000m_",
                              years, ".tif"), 
                            cores = 1)
     
    
})
print(paste0("All done. Time: ", Sys.time()))

plan(sequential) 


#### Habitat diversity Grasses and crops joined -------------------------

veg_type_files <- list.files("data/spatial_data/time_series",
                                    full.names = T, pattern = "veg_types")

terraOptions(memfrac = 0.5)

# veg_type_files <- veg_type_files[
#   grepl("2018_2019", veg_type_files) |
#     grepl("2019_2020", veg_type_files)  | 
#     grepl("2024_2025", veg_type_files)]

plan(multisession, workers = 3)

future_walk(1:length(veg_type_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- veg_type_files[i]
              dw_r <- rast(file)
              plot(dw_r)
              
              data_type_dw <- terra::datatype(dw_r)
              
              years <- gsub("data/spatial_data/time_series/veg_types_grass_and_crops_joined_", "", file)
              years <- gsub("_10m.tif", "", years)
              
              #aggregate at 100m 
              div_100m_r <- aggregate(dw_r, 
                                      fact = 10, 
                                      fun = function(x){ 
                                        #remove NAs
                                        x <- x[!is.na(x)]
                                        if(length(x) == 0){return(NA)}
                                        
                                        # calculate shannon diversity 
                                        abundances <- table(x)
                                        
                                        community_matrix <- matrix(abundances, nrow = 1)
                                        colnames(community_matrix) <- names(abundances) #probably unnecceary, but just for the record
                                        
                                        sh_div <- vegan::diversity(community_matrix, index = "shannon")
                                        return(sh_div)}, 
                                      filename = paste0(
                                        "data/spatial_data/time_series/shannon_div_habitat_veg_types_gr_n_cr_100m_",
                                        years, ".tif"), 
                                      cores = 1)
              
              #aggregate at the 1 km scale 
              div_1000m_r <- aggregate(dw_r, 
                                       fact = 100, 
                                       fun = function(x){ 
                                         #remove NAs
                                         x <- x[!is.na(x)]
                                         if(length(x) == 0){return(NA)}
                                         
                                         # calculate shannon diversity 
                                         abundances <- table(x)
                                         
                                         community_matrix <- matrix(abundances, nrow = 1)
                                         colnames(community_matrix) <- names(abundances) #probably unnecessary, but just for the record
                                         
                                         sh_div <- vegan::diversity(community_matrix, index = "shannon")
                                         return(sh_div)}, 
                                       filename = paste0(
                                         "data/spatial_data/time_series/shannon_div_habitat_veg_types_gr_n_cr_1000m_",
                                         years, ".tif"), 
                                       cores = 1)
              
              
            })
print(paste0("All done. Time: ", Sys.time()))

plan(sequential) 


### aggregate quality layer -----------


fraction_mode_files <- list.files("data/spatial_data/time_series",
                             full.names = T, pattern = "dw_fraction_mode_")

terraOptions(memfrac = 0.5)

# veg_type_files <- veg_type_files[
#   grepl("2018_2019", veg_type_files) |
#     grepl("2019_2020", veg_type_files)  | 
#     grepl("2024_2025", veg_type_files)]

#minimum quality ---------------------
plan(multisession, workers = 5)

future_walk(1:length(fraction_mode_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- fraction_mode_files[i]
              dw_r <- rast(file)
              plot(dw_r)
              
              data_type_dw <- terra::datatype(dw_r)
              
              years <- gsub("data/spatial_data/time_series/dw_fraction_mode_", "", file)
              years <- gsub("_10m.tif", "", years)
              
              #aggregate at 100m 
              fraction_mode_100m_r <- terra::aggregate(dw_r, 
                                      fact = 10, 
                                      fun = "min", 
                                      filename = paste0(
                                        "data/spatial_data/time_series/dw_mode_fraction_100m_",
                                        years, ".tif"), 
                                      cores = 1, 
                                      overwrite = T)
              #plot(fraction_mode_100m_r)
})
plan(sequential)

#### now get the minimum...

fraction_mode_100m_files <- list.files("data/spatial_data/time_series",
                                  full.names = T, pattern = "dw_mode_fraction_100m_")


r_stack <- rast(fraction_mode_100m_files)

r_min_mode_fraction <-  min(r_stack, na.rm = TRUE)
plot(r_min_mode_fraction)
datatype(r_min_mode_fraction)
writeRaster(r_min_mode_fraction, 
            filename = "data/spatial_data/covariates/raster/dw_min_mode_fraction_100m.tif", 
            overwrite = T)

r <- rast("data/spatial_data/covariates/raster/dw_min_mode_fraction_100m.tif")
datatype(r)


####### MEDIAN QUALITY ------

### aggregate quality layer -----------


fraction_mode_files <- list.files("data/spatial_data/time_series",
                                  full.names = T, pattern = "dw_fraction_mode_")

terraOptions(memfrac = 0.5)

# veg_type_files <- veg_type_files[
#   grepl("2018_2019", veg_type_files) |
#     grepl("2019_2020", veg_type_files)  | 
#     grepl("2024_2025", veg_type_files)]

#median quality ---------------------
plan(multisession, workers = 5)

future_walk(1:length(fraction_mode_files),
            .progress = TRUE,
            function(i){
              
              #for(file in unique(dynamic_world_files)){
              
              file <- fraction_mode_files[i]
              dw_r <- rast(file)
             # plot(dw_r)
              
              data_type_dw <- terra::datatype(dw_r)
              
              years <- gsub("data/spatial_data/time_series/dw_fraction_mode_", "", file)
              years <- gsub("_10m.tif", "", years)
              
              #aggregate at 100m 
              fraction_mode_100m_r <- terra::aggregate(dw_r, 
                                                       fact = 10, 
                                                       fun = "median", 
                                                       filename = paste0(
                                                         "data/spatial_data/time_series/dw_median_mode_fraction_100m_",
                                                         years, ".tif"), 
                                                       cores = 1, 
                                                       overwrite = T)
              #plot(fraction_mode_100m_r)
            })
plan(sequential)

#### now get the minimum & median...

fraction_median_mode_100m_files <- list.files("data/spatial_data/time_series",
                                       full.names = T, pattern = "dw_median_mode_fraction_100m_")


r_stack_median <- rast(fraction_median_mode_100m_files)


#minimum
r_min_median_mode_fraction <-  min(r_stack_median, na.rm = TRUE)
plot(r_min_median_mode_fraction)
datatype(r_min_median_mode_fraction)
writeRaster(r_min_median_mode_fraction, 
            filename = "data/spatial_data/covariates/raster/dw_min_median_mode_fraction_100m.tif", 
            overwrite = T)

r_min_median <- rast("data/spatial_data/covariates/raster/dw_min_median_mode_fraction_100m.tif")
plot(r_min_median)


#### now get the minimum & median...

r_median_median_mode_fraction <-  median(r_stack_median, na.rm = TRUE)
plot(r_median_median_mode_fraction)
datatype(r_median_median_mode_fraction)
writeRaster(r_median_median_mode_fraction, 
            filename = "data/spatial_data/covariates/raster/dw_median_median_mode_fraction_100m.tif", 
            overwrite = T)

r_median_median <- rast("data/spatial_data/covariates/raster/dw_median_median_mode_fraction_100m.tif")
plot(r_median_median)
plot(r_min_median)
