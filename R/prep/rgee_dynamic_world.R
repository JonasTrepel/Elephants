library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)

source("R/functions/monitor_gee_task.R")
#  
rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
reticulate::use_python(rgee_env_dir, required=T)
#ee_clean_user_credentials()
#ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "ee-jonastrepel")
drive_auth(email = "jonas.trepel@bio.au.dk")
ee$String('Hello from the Earth Engine servers!')$getInfo()


#define area of interest
aoi <- ee$Geometry$Rectangle(
  coords = c(7.5, -35.0, 45.0, 5.0), # xmin, ymin, xmax, ymax
  #coords = c(27.68, -24.18, 27.91, -23.99), #Kaingo
  proj = "EPSG:4326",
  geodesic = FALSE
)

Map$addLayer(aoi)
# Define years and dates for Landsat image collection

years <- c(2015:2024)

for(year in years){ 
  
year_1 = year+1

start_date <- paste0(year, "-07-01")
end_date <- paste0(year_1, "-6-30")


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
monitor_gee_task(pattern = "dynamic_world", path = "rgee_backup_dynamic_world",
                 last_sleep_time = 600)

Sys.sleep(2400)
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

dynamic_world_file_name_merge <- paste0("data/spatial_data/time_series/dynamic_world_", year,"_",year_1,"_10m.tif")

data_type_dynamic_world <- terra::datatype(dynamic_world_raster_list[[1]])

dynamic_world_r <- merge(sprc(dynamic_world_raster_list),
                         filename = dynamic_world_file_name_merge,
                         overwrite = TRUE,
                         datatype = data_type_dynamic_world)
plot(dynamic_world_r)

# writeRaster(dynamic_world_r, 
#             filename = dynamic_world_file_name_merge,
#             overwrite = TRUE,
#             datatype = data_type_dynamic_world)



file.remove(dynamic_world_files)
googledrive::drive_rm(unique(dynamic_world_drive_files$name))
googledrive::drive_rm("rgee_backup_dynamic_world")

print(paste0(year, " done. Time: ", Sys.time()))

}


