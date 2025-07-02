
reticulate::use_python("C:/Users/au713983/.conda/envs/rgee_env/python.exe", required = TRUE)


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
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
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

years <- c(2019:2024)


for(year in years){ 
  
  print(paste0("starting now with year: ", year, ". Time: ", Sys.time()))
        
  year_1 = year+1
  
  start_date <- paste0(year, "-07-01")
  end_date <- paste0(year_1, "-6-30")
  
  
  dw_ic <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
    filterDate(start_date, end_date)$
    filterBounds(aoi)$
    select("label")
  
  #1. get annual mode
  dw_mode <- dw_ic$reduce(ee$Reducer$mode())
  
  #2. For each image in collection, check if image equals the mode, 
  # if yes it should have the value 1, ig not 0 or NA
  dw_eq_mode_ic <- dw_ic$map(function(img) {
    img$eq(dw_mode)$rename("match")$updateMask(img$mask())
  })
  
  #number of images equal to mode
  dw_n_eq_mode <- dw_eq_mode_ic$select("match")$sum()$toInt16()
  
  
  #3. Get the total number of valid images for a given year
  dw_valid_count <- dw_ic$map(function(img) {
    img$updateMask(img$mask())$ # keep only valid pixels
      multiply(0)$add(1)$        # convert valid pixels to 1
      rename("valid")           
  })$select("valid")$sum()$int16() 
  
  #4. Get the fraction of images which correspond to the mode. If high --> probably we can be more certain
  dw_fraction_mode <- dw_n_eq_mode$divide(dw_valid_count)$multiply(100)$round()$toInt16() #multiply by 100 to get percentage (e.g., integer)
  
  
  # Viz (commented out when working on the full extent )
  # Map$centerObject(aoi)
  # 
  # Map$addLayer(dw_fraction_mode,
  #   visParams = list(min = 0, max = 100, palette = c("blue", "lightblue", "white", "pink", "red")),
  #   name = "Fraction (percentage) = Mode"
  # )
  #
  # Map$addLayer(dw_valid_count,
  #             visParams = list(min = 0, max = 100, palette = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154")),
  #               name = "Valid obs"
  # )
  # 
  # Map$addLayer(dw_fraction_mode,
  #              visParams = list(min = 0, max = 100, palette = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154")),
  #              name = "Valid obs"
  # )
  

  ### EXPORT AND PROCESS fraction_mode IMAGE
  export_dw_fraction_mode <- ee_image_to_drive(
    image = dw_fraction_mode,
    region = aoi,
    folder = "rgee_backup_dw_fraction_mode",
    description = "dw_fraction_mode",
    scale = 10,
    timePrefix = FALSE,
    maxPixels = 1e13
  )
  export_dw_fraction_mode$start()
  
  export_dw_valid_image_count <- ee_image_to_drive(
    image = dw_valid_count,
    region = aoi,
    folder = "rgee_backup_dw_valid_image_count",
    description = "dw_valid_image_count",
    scale = 10,
    timePrefix = FALSE,
    maxPixels = 1e13
  )
  export_dw_valid_image_count$start()
  

  ### Monitor and process image count
  
  Sys.sleep(60)
  monitor_gee_task(pattern = "dw_valid_image_count", path = "rgee_backup_dw_valid_image_count",
                   last_sleep_time = 600, mail = "jonas.trepel@gmail.com")
  
  Sys.sleep(5400)
  (dw_valid_image_count_drive_files <- drive_ls(path = "rgee_backup_dw_valid_image_count",
                                            pattern = "dw_valid_image_count") %>%
      dplyr::select(name) %>% 
      unique())
  
  for(filename in unique(dw_valid_image_count_drive_files$name)){
    
    path_name = paste0("data/spatial_data/raw_tiles/", filename)
    drive_download(file = filename, path = path_name, overwrite = TRUE)
  }
  
  
  dw_valid_image_count_files <- list.files("data/spatial_data/raw_tiles",
                                       full.names = T, pattern = "dw_valid_image_count")
  
  dw_valid_image_count_raster_list <- lapply(dw_valid_image_count_files, rast)
  
  dw_valid_image_count_file_name_merge <- paste0("data/spatial_data/time_series/dw_valid_image_count_", year,"_",year_1,"_10m.tif")
  
  data_type_dw_valid_image_count <- terra::datatype(dw_valid_image_count_raster_list[[1]])
  
  dw_valid_image_count_r <- merge(sprc(dw_valid_image_count_raster_list),
                              filename = dw_valid_image_count_file_name_merge,
                              overwrite = TRUE,
                              datatype = data_type_dw_valid_image_count)
  plot(dw_valid_image_count_r)
  
  
  file.remove(dw_valid_image_count_files)
  googledrive::drive_rm(unique(dw_valid_image_count_drive_files$name))
  googledrive::drive_rm("rgee_backup_dw_valid_image_count")
  print(paste0("Number of valid images for ", year, " done. Time: ", Sys.time()))
  
  
  ### Monitor and process fraction_modeal image 
  Sys.sleep(60)
  monitor_gee_task(pattern = "dw_fraction_mode", path = "rgee_backup_dw_fraction_mode",
                   last_sleep_time = 600, mail = "jonas.trepel@gmail.com")
  
  Sys.sleep(3600)
  (dw_fraction_mode_drive_files <- drive_ls(path = "rgee_backup_dw_fraction_mode",
                                            pattern = "dw_fraction_mode") %>%
      dplyr::select(name) %>% 
      unique())
  
  for(filename in unique(dw_fraction_mode_drive_files$name)){
    
    path_name = paste0("data/spatial_data/raw_tiles/", filename)
    drive_download(file = filename, path = path_name, overwrite = TRUE)
  }
  
  
  dw_fraction_mode_files <- list.files("data/spatial_data/raw_tiles",
                                       full.names = T, pattern = "dw_fraction_mode")
  
  dw_fraction_mode_raster_list <- lapply(dw_fraction_mode_files, rast)
  
  dw_fraction_mode_file_name_merge <- paste0("data/spatial_data/time_series/dw_fraction_mode_", year,"_",year_1,"_10m.tif")
  
  data_type_dw_fraction_mode <- terra::datatype(dw_fraction_mode_raster_list[[1]])
  
  dw_fraction_mode_r <- merge(sprc(dw_fraction_mode_raster_list),
                              filename = dw_fraction_mode_file_name_merge,
                              overwrite = TRUE,
                              datatype = data_type_dw_fraction_mode)
  plot(dw_fraction_mode_r)
  
  
  file.remove(dw_fraction_mode_files)
  googledrive::drive_rm(unique(dw_fraction_mode_drive_files$name))
  googledrive::drive_rm("rgee_backup_dw_fraction_mode")
  
  print(paste0("fraction_mode of images equal to mode done for ", year, " done. Time: ", Sys.time()))
  
}
