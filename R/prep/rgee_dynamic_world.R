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

file.remove(dynamic_world_files)
googledrive::drive_rm(unique(dynamic_world_drive_files$name))
googledrive::drive_rm("rgee_backup_dynamic_world")

print(paste0(year, " done"))

}

###### Reclassify ------------------------

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



