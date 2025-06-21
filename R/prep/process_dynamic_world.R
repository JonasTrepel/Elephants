library(data.table)
library(terra)
library(tidyverse)
library(mapview)
library("furrr")


dynamic_world_files <- list.files("data/spatial_data/time_series",
                                  full.names = T, pattern = "dynamic_world")

terraOptions(memfrac = 0.5)

#dynamic_world_files <- dynamic_world_files[2:3]

for(file in unique(dynamic_world_files)){
  
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

}
