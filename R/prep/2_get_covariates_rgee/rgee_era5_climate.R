##### Load all rasters ##### 

library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)

#  
rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
reticulate::use_python(rgee_env_dir, required=T)
#ee_clean_user_credentials()
#ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
ee$String('Hello from the Earth Engine servers!')$getInfo()


monitor_gee_task <- function(pattern = NA, path = "rgee_backup", last_sleep_time = 600, 
                             mail = "jonas.trepel@gmail.com") {
  
  drive_auth(email = mail)
  
  for (i in 1:10000) {
    drive_files <- drive_ls(path = path, pattern = pattern) %>%
      dplyr::select(name)
    
    # Check if the folder is empty
    if (n_distinct(drive_files) == 0) {
      Sys.sleep(10)
      print(paste0("Attempt ", i, ": Drive still empty"))
    } else {
      print("Files found:")
      print(drive_files)
      
      if (n_distinct(drive_files) < 8) {
        Sys.sleep(10) # to make sure all tiles are there
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


years <- c(2001:2024)


for(year in years){
  
  print(paste0("Starting with: ", year))
  
  start_date <- paste0(year, "-07-01")
  end_date <- paste0(year+1, "-06-30")
  
  
  aoi <- ee$Geometry$Rectangle(
    coords = c(7.5, -35.0, 45.0, 5.0), # xmin, ymin, xmax, ymax
    #coords = c(27.68, -24.18, 27.91, -23.99), #Kaingo
    proj = "EPSG:4326",
    geodesic = FALSE
  )
  
  Map$addLayer(aoi)
  
  # MAT
  annual_temp <- ee$
    ImageCollection('ECMWF/ERA5_LAND/MONTHLY_AGGR')$
    select('temperature_2m')$
    filterDate(start_date, end_date)$
    mean()$subtract(273.15)
  
  Map$addLayer(annual_temp)

  
  export_task <- ee_image_to_drive(image = annual_temp,
                                   region = aoi,
                                   folder = "rgee_backup_mat",
                                   description = "annual_temp",
                                   scale = 11132, 
                                   timePrefix = FALSE, 
                                   maxPixels = 1e13
  )
  export_task$start()
  
  Sys.sleep(30)
  monitor_gee_task(pattern = "annual_temp", path = "rgee_backup_mat",
                   last_sleep_time = 10)
  
  (drive_files <- drive_ls(path = "rgee_backup_mat",
                           pattern = "annual_temp") %>%
      dplyr::select(name) %>% 
      unique())
  
  path_name <- paste0("data/spatial_data/time_series/mat_", year, ".tif")
  
  drive_download(file = drive_files$name, path = path_name, overwrite = TRUE)
  
  googledrive::drive_rm(unique(drive_files$name))
  googledrive::drive_empty_trash()
  
  r_mat <- rast(path_name)
  plot(r_mat, main = paste0("MAT ", year))
  
  #Precipitation
  annual_prec <- ee$
    ImageCollection('ECMWF/ERA5_LAND/MONTHLY_AGGR')$
    select('total_precipitation_sum')$
    filterDate(start_date, end_date)$
    sum()$multiply(1000)
  
  Map$addLayer(annual_prec)

  export_task <- ee_image_to_drive(image = annual_prec,
                                   region = aoi,
                                   folder = "rgee_backup_prec",
                                   description = "annual_prec",
                                   scale = 11132, 
                                   timePrefix = FALSE, 
                                   maxPixels = 1e13
  )
  export_task$start()
  
  Sys.sleep(30)
  monitor_gee_task(pattern = "annual_prec", path = "rgee_backup_prec",
                   last_sleep_time = 10)
  
  (drive_files <- drive_ls(path = "rgee_backup_prec",
                           pattern = "annual_prec") %>%
      dplyr::select(name) %>% 
      unique())
  
  
  path_name <- paste0("data/spatial_data/time_series/precipitation_sum_", year, ".tif")
  
  
  drive_download(file = drive_files$name, path = path_name, overwrite = TRUE)
  
  
  googledrive::drive_rm(unique(drive_files$name))
  googledrive::drive_empty_trash()
  
  r_prec <- rast(path_name)
  plot(r_prec, main = paste0("Precipitation ", year))
  
  print(paste0(year, " done"))
  
}