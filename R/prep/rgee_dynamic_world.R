library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)

monitor_gee_task <- function(pattern = NA, path = "rgee_backup", last_sleep_time = 10) {
  drive_auth(email = "jonas.trepel@gmail.com")
  
  for (i in 1:1000) {
    drive_files <- drive_ls(path = path, pattern = pattern) %>%
      dplyr::select(name)
    
    # Check if the folder is empty
    if (n_distinct(drive_files) == 0) {
      Sys.sleep(30)
      print(paste0("Attempt ", i, ": Drive still empty"))
    } else {
      print("Files found:")
      print(drive_files)
      
      if (n_distinct(drive_files) < 8) {
        Sys.sleep(150) # to make sure all tiles are there
        drive_files <- drive_ls(path = path, pattern = pattern) %>%
          dplyr::select(name)
      }
      # check again
      if (n_distinct(drive_files) < 8) {
        Sys.sleep(last_sleep_time) # to make sure all tiles are there
      }
      drive_files <- drive_ls(path = path, pattern = pattern) %>% dplyr::select(name)
      print(drive_files)
      
      break #
    }
  }
}
# 
# rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
# reticulate::use_python(rgee_env_dir, required=T)
ee_clean_user_credentials()
ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
ee$String('Hello from the Earth Engine servers!')$getInfo()


#define area of interest
aoi <- ee$Geometry$Rectangle(
  #coords = c(7.5, -35.0, 45.0, 5.0), # xmin, ymin, xmax, ymax
  coords = c(27.68, -24.18, 27.91, -23.99), #Kaingo
  proj = "EPSG:4326",
  geodesic = FALSE
)

Map$addLayer(aoi)
# Define years and dates for Landsat image collection

years <- c(2016:2024)

for(year in years){ 

start_date <- paste0(year, "-01-01")
end_date <- paste0(year, "-12-31")


dw_ic <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterDate(start_date, end_date)$
  filterBounds(aoi)$
  select("label")

# Compute mode across time
dynamic_world_img <- dw_ic$reduce(ee$Reducer$mode())

# Display - not smart when doing all over subsaharan africa
# Map$centerObject(aoi)
# Map$addLayer(
#   annual_img,
#   list(min = 0, max = 8, palette = c(
#     "#419BDF", # Water
#     "#397D49", # Trees
#     "#88B053", # Grass
#     "#7A87C6", # Flooded vegetation
#     "#E49635", # Crops
#     "#DFC35A", # Shrub & Scrub
#     "#C4281B", # Built-up
#     "#A59B8F", # Bare ground
#     "#B39FE1"  # Snow/Ice
#   )),
#   paste("DW", year)
# )


export_dynamic_world <- ee_image_to_drive(
  image = dynamic_world_img,
  region = aoi,
  folder = "rgee_backup_dynamic_world",
  description = "dynamic_world",
  scale = 10,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_dynamic_world$start()


Sys.sleep(60)
monitor_gee_task(pattern = "dynamic_world", path = "rgee_backup_dynamic_world")

Sys.sleep(120)
(dynamic_world_drive_files <- drive_ls(path = "rgee_backup_dynamic_world",
                                       pattern = "dynamic_world") %>%
    dplyr::select(name) %>% 
    unique())

for(filename in unique(dynamic_world_drive_files$name)){
  
  path_name = paste0("data/spatial_data/raw_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}


dynamic_world_files <- list.files("data/spatial_data/raw_tiles",
                                  full.names = T, pattern = "dynamic_world")

dynamic_world_raster_list <- lapply(dynamic_world_files, rast)

dynamic_world_file_name_merge <- paste0("data/spatial_data/time_series/dynamic_world_", year,"_10m.tif")

data_type_dynamic_world <- terra::datatype(dynamic_world_raster_list[[1]])

dynamic_world_r <- merge(sprc(dynamic_world_raster_list),
                         filename = dynamic_world_file_name_merge,
                         overwrite = TRUE,
                         datatype = data_type_dynamic_world)
plot(dynamic_world_r)

writeRaster(dynamic_world_r, 
            filename = dynamic_world_file_name_merge,
            overwrite = TRUE,
            datatype = data_type_dynamic_world)
# loop through the raster list 

for(i in lengths(dynamic_world_raster_list)){
  
  tile_r <- dynamic_world_raster_list[[i]]
  plot(tile_r)
  
  grass_r <- (tile_r == 2 | tile_r == 4) * 1
  plot(grass_r, col = c("white", "#88B053"), main = "Grass")
  
  shrub_r <- (tile_r == 5) * 1
  plot(shrub_r, col = c("white", "#DFC35A"), main = "Shrub")
  
  tree_r <- (tile_r == 1) * 1
  plot(tree_r, col = c("white", "#397D49"), main = "Trees")
  
  # Create heterogeneity classification image
  vt_r <- classify(tile_r,
                      rcl = matrix(c(
                        1, 1,   # class 1 → 1
                        2, 2,   # class 2 → 2
                        4, 2,   # class 4 → 2
                        3, 3,   # class 3 → 3
                        5, 5,   # class 5 → 5
                        7, 7    # class 7 → 7
                      ), ncol = 2, byrow = TRUE))
  
  # Mask out classes not mapped (i.e., all others become NA)
  mask_classes <- tile_r %in% c(1, 2, 3, 4, 5, 7)
  vt_r <- mask(vt_r, mask_classes, maskvalues = FALSE)
  
  # Visualize with approximate DW class colors
  colors <- c("#397D49", "#88B053", "#7A87C6", "#DFC35A", "#DFC35A", "#A59B8F")
  plot(vt_r, col = colors, main = "Heterogeneity Map")
  
  writeRaster(
    grass_r, 
    filename =paste0("data/spatial_data/raw_tiles/grass_", year,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dynamic_world
  )
  
  writeRaster(
    shrub_r, 
    filename =paste0("data/spatial_data/raw_tiles/shrub_", year,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dynamic_world
  )
  
  writeRaster(
    tree_r, 
    filename =paste0("data/spatial_data/raw_tiles/tree_", year,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dynamic_world
  )
  
  writeRaster(
    vt_r, 
    filename =paste0("data/spatial_data/raw_tiles/vegetation_types_", year,"_10m_tile_", i, ".tif"),
    overwrite = TRUE,
    datatype = data_type_dynamic_world
  )
  
  print(paste0(i, " of ", lengths(dynamic_world_raster_list), " done. Time: ", Sys.time()))
}

# Merge all subsets too 

#Grass
grass_files <- list.files("data/spatial_data/raw_tiles",
                                  full.names = T, pattern = "grass")

grass_raster_list <- lapply(grass_files, rast)

grass_file_name_merge <- paste0("data/spatial_data/time_series/grass_", year,"_10m.tif")

data_type_grass <- terra::datatype(grass_raster_list[[1]])

grass_r <- merge(sprc(grass_raster_list),
                         filename = grass_file_name_merge,
                         overwrite = TRUE,
                         datatype = data_type_grass)
plot(grass_r)

#Shrubs

shrub_files <- list.files("data/spatial_data/raw_tiles",
                          full.names = T, pattern = "shrub")

shrub_raster_list <- lapply(shrub_files, rast)

shrub_file_name_merge <- paste0("data/spatial_data/time_series/shrub_", year,"_10m.tif")

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

tree_file_name_merge <- paste0("data/spatial_data/time_series/tree_", year,"_10m.tif")

data_type_tree <- terra::datatype(tree_raster_list[[1]])

tree_r <- merge(sprc(tree_raster_list),
                 filename = tree_file_name_merge,
                 overwrite = TRUE,
                 datatype = data_type_tree)
plot(tree_r)

#Vegetation types

veg_types_files <- list.files("data/spatial_data/raw_tiles",
                          full.names = T, pattern = "vegetation_types")

veg_types_raster_list <- lapply(veg_types_files, rast)

veg_types_file_name_merge <- paste0("data/spatial_data/time_series/veg_types_", year,"_10m.tif")

data_type_veg_types <- terra::datatype(veg_types_raster_list[[1]])

veg_types_r <- merge(sprc(veg_types_raster_list),
                 filename = veg_types_file_name_merge,
                 overwrite = TRUE,
                 datatype = data_type_veg_types)
plot(veg_types_r)

file.remove(dynamic_world_files)
file.remove(grass_files)
file.remove(shrub_files)
file.remove(tree_files)
file.remove(veg_types_files)

googledrive::drive_rm(unique(dynamic_world_drive_files$name))
googledrive::drive_rm("rgee_backup_dynamic_world")

print(paste0(year, " done"))

}

###### Reclassify ------------------------

dynamic_world_files <- list.files("data/spatial_data/time_series",
                                  full.names = T, pattern = "dynamic_world")

for(file in dynamic_world_files){
  
  dw_r <- rast(file)
  
  
  
}

grass_img <- annual_img$
  eq(2)$Or(annual_img$eq(4))$
  multiply(1)

Map$addLayer(grass_img, list(min = 0, max = 1, palette = c("white", "#88B053")))
  
shrub_img <- annual_img$
  eq(5)$
  multiply(1)

Map$addLayer(shrub_img, list(min = 0, max = 1, palette = c("white", "#DFC35A")))

tree_img <- annual_img$
  eq(1)$
  multiply(1)

Map$addLayer(tree_img, list(min = 0, max = 1, palette = c("white", "#397D49")))


het_img <- annual_img$where(annual_img$eq(1), 1)$
  where(annual_img$eq(2)$Or(annual_img$eq(4)), 2)$
  where(annual_img$eq(3), 3)$
  where(annual_img$eq(5), 5)$
  where(annual_img$eq(7), 7)


mask_values <- annual_img$eq(1)$Or(
  annual_img$eq(2)$Or(
    annual_img$eq(3)$Or(
      annual_img$eq(4)$Or(
        annual_img$eq(5)$Or(
          annual_img$eq(7))))))

# Apply the mask
het_img <- het_img$updateMask(mask_values)
Map$addLayer(het_img, list(min = 1, max = 7, palette = c("#397D49", "#88B053", "#7A87C6","#DFC35A", "#DFC35A", "#A59B8F")), "Heterogeneity Map")



