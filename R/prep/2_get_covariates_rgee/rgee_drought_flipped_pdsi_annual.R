rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
reticulate::use_python(rgee_env_dir, required=T)

library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)
library(exactextractr)

############################# HOUSEKEEPING #############################

source("R/functions/monitor_gee_task.R")

#ee_clean_user_credentials()
ee$Authenticate(auth_mode='notebook')
#when on GIS04
#ee$Initialize(project = "ee-jonastrepel")
#drive_auth(email = "jonas.trepel@bio.au.dk")
#mail <- "jonas.trepel@bio.au.dk"
#when GIS07
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
mail <- "jonas.trepel@gmail.com"

ee$String('Hello from the Earth Engine servers!')$getInfo()

### Get sub-saharan Africa extent
aoi <- ee$Geometry$Rectangle(
  coords = c(7.5, -35.0, 45.0, 5.0), # xmin, ymin, xmax, ymax
  proj = "EPSG:4326",
  geodesic = FALSE
)


years <- c(2001:2024)

for(year in years){
  
  print(paste0("Starting with: ", year))
  
  start_date <- paste0(year, "-07-01")
  end_date <- paste0(year+1, "-06-30")
  
  pdsi_mean <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
    filterDate(start_date, end_date)$
    filterBounds(aoi)$ 
    select("pdsi")$
    mean()$
    divide(1000)$
    multiply(-1) #flippin it so that higher values, more severe drought
  
  
  pdsi_min <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
    filterDate(start_date, end_date)$
    filterBounds(aoi)$ 
    select("pdsi")$
    min()$
    divide(1000)$
    multiply(-1) #flippin it so that higher values, more severe drought
  
  # Map$centerObject(aoi)
  # Map$addLayer(
  #   pdsi_mean,
  #   list(min = -5, max = 5, palette = c("blue", "white", "red")),
  #   "Mean PDSI"
  # )
  # 
  # Map$centerObject(aoi)
  # Map$addLayer(
  #   pdsi_min,
  #   list(min = -5, max = 5, palette = c("blue", "white", "red")),
  #   "Max (Min) PDSI"
  # )
  # 
  
  export_mean_pdsi <- ee_image_to_drive(
    image = pdsi_mean,
    region = aoi,
    folder = "rgee_backup_mean_pdsi",
    description = "mean_pdsi",
    scale = 4638.3,
    timePrefix = FALSE,
    maxPixels = 1e13
  )
  export_mean_pdsi$start()
  
  export_min_pdsi <- ee_image_to_drive(
    image = pdsi_min,
    region = aoi,
    folder = "rgee_backup_min_pdsi",
    description = "min_pdsi",
    scale = 4638.3,
    timePrefix = FALSE,
    maxPixels = 1e13
  )
  export_min_pdsi$start()
  
  Sys.sleep(120)
  monitor_gee_task(pattern = "mean_pdsi", path = "rgee_backup_mean_pdsi",
                   mail = mail, last_sleep_time = 10)
  
  drive_files_mean_pdsi <- drive_ls(path = "rgee_backup_mean_pdsi", pattern = "mean_pdsi") %>%
    dplyr::select(name) %>% 
    unique()
  
  monitor_gee_task(pattern = "min_pdsi", path = "rgee_backup_min_pdsi",
                   mail = mail, last_sleep_time = 10)
  
  drive_files_min_pdsi <- drive_ls(path = "rgee_backup_min_pdsi", pattern = "min_pdsi") %>%
    dplyr::select(name) %>% 
    unique()
  
  
  # save mean PDSI
  filename_mean_pdsi <- unique(drive_files_mean_pdsi$name)
  filepath_mean_pdsi <- paste0("data/spatial_data/time_series/mean_drought_flipped_pdsi_", year, ".tif")
  
  drive_download(file = filename_mean_pdsi, path = filepath_mean_pdsi, overwrite = TRUE)
  
  googledrive::drive_rm(unique(drive_files_mean_pdsi$name))
  googledrive::drive_rm("rgee_backup_mean_pdsi")
  
  mean_pdsi_r <- rast(filepath_mean_pdsi)
  
  plot(mean_pdsi_r, main = paste0("Mean Flipped PDSI ", year))
  
  #save min (now max I guess??)
  filename_min_pdsi <- unique(drive_files_min_pdsi$name)
  filepath_min_pdsi <- paste0("data/spatial_data/time_series/max_drought_flipped_pdsi_", year, ".tif")
  
  drive_download(file = filename_min_pdsi, path = filepath_min_pdsi, overwrite = TRUE)
  
  googledrive::drive_rm(unique(drive_files_min_pdsi$name))
  googledrive::drive_rm("rgee_backup_min_pdsi")
  
  min_pdsi_r <- rast(filepath_min_pdsi)
  
  plot(min_pdsi_r, main = paste0("Max Flipped PDSI ", year))
  
  #file.remove(files)
  
  print(paste0(year, " PDSI done"))
  
}
