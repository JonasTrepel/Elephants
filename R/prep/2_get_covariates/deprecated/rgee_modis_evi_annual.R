##### Load all EVI rasters ##### 
rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
reticulate::use_python(rgee_env_dir, required=T)

library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)

source("R/functions/monitor_gee_task.R")


#ee_clean_user_credentials()
#ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "ee-jonastrepel")
drive_auth(email = "jonas.trepel@bio.au.dk")


years <- c(2001:2024)

for(year in years){
  
  print(paste0("Starting with: ", year))
  
  start_date <- paste0(year, "-07-01")
  end_date <- paste0(year+1, "-06-30")
  
  annual_img <- ee$
    ImageCollection('MODIS/061/MOD13A1')$
    map(function(img) {
      qa <- img$select("SummaryQA")
      img$updateMask(qa$eq(0))})$ ## select only high quality data 
    select('EVI')$
    filterDate(start_date, end_date)$
    mean()
  
  Map$addLayer(annual_img)
  
  # ee_print(annual_img)
  
  ### Get sub-saharan Africa extent
  ssa_ext <- ee$Geometry$Rectangle(
    coords = c(7.5, -35.0, 45.0, 5.0), # xmin, ymin, xmax, ymax
    proj = "EPSG:4326",
    geodesic = FALSE
  )
  
  export_evi <- ee_image_to_drive(
    image = annual_img,
    region = ssa_ext,
    folder = "rgee_backup_evi",
    description = "evi",
    scale = 500,
    timePrefix = FALSE,
    maxPixels = 1e13
  )
  export_evi$start()
  
  Sys.sleep(60)
  monitor_gee_task(pattern = "evi", path = "rgee_backup_evi", 
                   last_sleep_time = 10)
  
  drive_files_evi <- drive_ls(path = "rgee_backup_evi", pattern = "evi") %>%
    dplyr::select(name) %>% 
    unique()
  
  # since it's only one tile we can save it directly 
  filename_evi <- unique(drive_files_evi$name)
  filepath_evi <- paste0("data/spatial_data/time_series/evi_mean_500m_", year, ".tif")
  
  drive_download(file = filename_evi, path = filepath_evi, overwrite = TRUE)
 
  googledrive::drive_rm(unique(drive_files_evi$name))
  googledrive::drive_rm("rgee_backup_evi")
  
  evi_r <- rast(filepath_evi)

  plot(evi_r, main = paste0(year))
  
  #file.remove(files)
  
  print(paste0(year, " EVI mean done"))
  
}
