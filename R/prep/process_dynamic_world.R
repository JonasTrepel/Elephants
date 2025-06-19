library(data.table)
library(terra)
library(tidyverse)
library(mapview)
library("furrr")


dynamic_world_files <- list.files("data/spatial_data/time_series",
                                  full.names = T, pattern = "dynamic_world")

for(file in unique(dynamic_world_files)){
  
  dw_r <- rast(file)
  plot(dw_r)
  
  data_type_dw <- terra::datatype(dw_r)
  
  years <- gsub("data/spatial_data/time_series/dynamic_world_", "", file)
  years <- gsub("_10m.tif", "", years)

  
  dw_ext <- ext(dw_r)
  
  tile_template <- rast(ext = dw_ext, resolution = 4, crs = "EPSG:4326") #4 degree tiles
  
  dw_tiles <- getTileExtents(dw_r, tile_template, buffer=2)

plan(multisession, workers = 30)


dw_res <- future_map(1:nrow(dw_tiles),
                     .progress = TRUE,
                     function(i){

  dw_r <- rast(file)
  data_type_dw <- terra::datatype(dw_r)
  
  
  tile_r <- terra::crop(dw_r, terra::ext(dw_tiles[i,])) 
  plot(tile_r)
  
  grass_r <- (tile_r == 2) * 1
  plot(grass_r, col = c("white", "#88B053"), main = "Grass")
  
  gr_n_cr_r <- (tile_r == 2 | tile_r == 4) * 1
  plot(gr_n_cr_r, col = c("white", "#88B053"), main = "Grass and Crop")
  
  shrub_r <- (tile_r == 5) * 1
  plot(shrub_r, col = c("white", "#DFC35A"), main = "Shrub")
  
  tree_r <- (tile_r == 1) * 1
  plot(tree_r, col = c("white", "#397D49"), main = "Trees")
  
  crop_r <- (tile_r == 4) * 1
  plot(grass_r, col = c("white", "#E49635"), main = "Crop")
  
  bare_r <- (tile_r == 7) * 1
  plot(bare_r, col = c("white", "#A59B8F"), main = "Bare")
  
  # Create heterogeneity classification image
  vt_r <- classify(tile_r,
                   rcl = matrix(c(
                     1, 1,   # class 1 to 1
                     2, 2,   # class 2 -> 2
                     3, 3,   # class 3 -> 3
                     5, 5,   # class 5 > 5
                     7, 7    # class 7 .> 7
                   ), ncol = 2, byrow = TRUE))
  
  # Mask out classes not mapped (i.e., all others become NA)
  mask_classes <- tile_r %in% c(1, 2, 3, 5, 7)
  vt_r <- mask(vt_r, mask_classes, maskvalues = FALSE)
  
  # Visualize with approximate DW class colors
  colors <- c("#397D49", "#88B053", "#7A87C6", "#DFC35A", "#A59B8F")
  plot(vt_r, col = colors, main = "Vegetation Types")
  
  
  vt_r_inc_cr <- classify(tile_r,
                   rcl = matrix(c(
                     1, 1,   # class 1 to 1
                     2, 2,   # class 2 -> 2
                     4, 2,   # class 4 -> 2
                     3, 3,   # class 3 -> 3
                     5, 5,   # class 5 > 5
                     7, 7    # class 7 .> 7
                   ), ncol = 2, byrow = TRUE))
  
  # Mask out classes not mapped (i.e., all others become NA)
  mask_classes_inc_cr <- tile_r %in% c(1, 2, 3, 4, 5, 7)
  vt_r_inc_cr <- mask(vt_r_inc_cr, mask_classes_inc_cr, maskvalues = FALSE)
  
  # Visualize with approximate DW class colors
  colors <- c("#397D49", "#88B053", "#7A87C6", "#DFC35A", "#DFC35A", "#A59B8F")
  plot(vt_r_inc_cr, col = colors, main = "Vegetation Types")
  
  writeRaster(
    grass_r, 
    filename =paste0("data/spatial_data/raw_tiles/grass_", years,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  writeRaster(
    gr_n_cr_r, 
    filename =paste0("data/spatial_data/raw_tiles/gr_n_cr_", years,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  writeRaster(
    shrub_r, 
    filename =paste0("data/spatial_data/raw_tiles/shrub_", years,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  writeRaster(
    tree_r, 
    filename =paste0("data/spatial_data/raw_tiles/tree_", years,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  writeRaster(
    crop_r, 
    filename =paste0("data/spatial_data/raw_tiles/crops_", years,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  writeRaster(
    bare_r, 
    filename =paste0("data/spatial_data/raw_tiles/bare_", years,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  writeRaster(
    vt_r, 
    filename =paste0("data/spatial_data/raw_tiles/vegetation_types_", years,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  writeRaster(
    vt_r_inc_cr, 
    filename =paste0("data/spatial_data/raw_tiles/veg_types_grass_n_crops_joined_", years,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dw
  )
  
  print(paste0(i, " of ", nrow(dw_tiles), " done. Time: ", Sys.time()))
  
  dt_res <- data.frame(done = i)
  return(dt_res)
  
})

# Merge all subsets too 

#Grass
grass_files <- list.files("data/spatial_data/raw_tiles",
                          full.names = T, pattern = "grass")

grass_raster_list <- lapply(grass_files, rast)

grass_file_name_merge <- paste0("data/spatial_data/time_series/grass_", years,"_10m.tif")

data_type_grass <- terra::datatype(grass_raster_list[[1]])

grass_r <- merge(sprc(grass_raster_list),
                 filename = grass_file_name_merge,
                 overwrite = TRUE,
                 datatype = data_type_grass)
plot(grass_r)

#Grass n Crops
gr_n_cr_files <- list.files("data/spatial_data/raw_tiles",
                            full.names = T, pattern = "gr_n_cr")

gr_n_cr_raster_list <- lapply(gr_n_cr_files, rast)

gr_n_cr_file_name_merge <- paste0("data/spatial_data/time_series/gr_n_cr_", years,"_10m.tif")

data_type_gr_n_cr <- terra::datatype(gr_n_cr_raster_list[[1]])

gr_n_cr_r <- merge(sprc(gr_n_cr_raster_list),
                   filename = gr_n_cr_file_name_merge,
                   overwrite = TRUE,
                   datatype = data_type_gr_n_cr)
plot(gr_n_cr_r)

#Shrubs

shrub_files <- list.files("data/spatial_data/raw_tiles",
                          full.names = T, pattern = "shrub")

shrub_raster_list <- lapply(shrub_files, rast)

shrub_file_name_merge <- paste0("data/spatial_data/time_series/shrub_", years,"_10m.tif")

data_type_shrub <- terra::datatype(shrub_raster_list[[1]])

shrub_r <- merge(sprc(shrub_raster_list),
                 filename = shrub_file_name_merge,
                 overwrite = TRUE,
                 datatype = data_type_shrub)
plot(shrub_r)

# Trees 
tree_files <- list.files("data/spatial_data/raw_tiles",
                         full.names = T, pattern = "tree")

tree_raster_list <- lapply(tree_files, rast)

tree_file_name_merge <- paste0("data/spatial_data/time_series/tree_", years,"_10m.tif")

data_type_tree <- terra::datatype(tree_raster_list[[1]])

tree_r <- merge(sprc(tree_raster_list),
                filename = tree_file_name_merge,
                overwrite = TRUE,
                datatype = data_type_tree)
plot(tree_r)

# Crop 
crop_files <- list.files("data/spatial_data/raw_tiles",
                         full.names = T, pattern = "crop")

crop_raster_list <- lapply(crop_files, rast)

crop_file_name_merge <- paste0("data/spatial_data/time_series/crop_", years,"_10m.tif")

data_type_crop <- terra::datatype(crop_raster_list[[1]])

crop_r <- merge(sprc(crop_raster_list),
                filename = crop_file_name_merge,
                overwrite = TRUE,
                datatype = data_type_crop)
plot(crop_r)

# Bare 
bare_files <- list.files("data/spatial_data/raw_tiles",
                         full.names = T, pattern = "bare")

bare_raster_list <- lapply(bare_files, rast)

bare_file_name_merge <- paste0("data/spatial_data/time_series/bare_", years,"_10m.tif")

data_type_bare <- terra::datatype(bare_raster_list[[1]])

bare_r <- merge(sprc(bare_raster_list),
                filename = bare_file_name_merge,
                overwrite = TRUE,
                datatype = data_type_bare)
plot(bare_r)

#Vegetation types

veg_types_files <- list.files("data/spatial_data/raw_tiles",
                              full.names = T, pattern = "vegetation_types")

veg_types_raster_list <- lapply(veg_types_files, rast)

veg_types_file_name_merge <- paste0("data/spatial_data/time_series/vegetation_types_", years,"_10m.tif")

data_type_veg_types <- terra::datatype(veg_types_raster_list[[1]])

veg_types_r <- merge(sprc(veg_types_raster_list),
                     filename = veg_types_file_name_merge,
                     overwrite = TRUE,
                     datatype = data_type_veg_types)
plot(veg_types_r)

#Vegetation types

veg_types_incl_crops_files <- list.files("data/spatial_data/raw_tiles",
                              full.names = T, pattern = "veg_types")

veg_types_incl_crops_raster_list <- lapply(veg_types_incl_crops_files, rast)

veg_types_incl_crops_file_name_merge <- paste0("data/spatial_data/time_series/veg_types_grass_and_crops_joined_", years,"_10m.tif")

data_type_veg_types_incl_crops <- terra::datatype(veg_types_incl_crops_raster_list[[1]])

veg_types_incl_crops_r <- merge(sprc(veg_types_incl_crops_raster_list),
                     filename = veg_types_incl_crops_file_name_merge,
                     overwrite = TRUE,
                     datatype = data_type_veg_types_incl_crops)
plot(veg_types_incl_crops_r)

file.remove(grass_files)
file.remove(gr_n_cr_files)
file.remove(shrub_files)
file.remove(tree_files)
file.remove(crop_files)
file.remove(bare_files)
file.remove(veg_types_files)
file.remove(veg_types_incl_crops_r)

print(paste0(years, " done. Time: ", Sys.time()))


}

## aggregate here 