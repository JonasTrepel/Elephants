library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library("enerscape")
library(terra)
library(exactextractr)

############################# HOUSEKEEPING #############################

source("R/functions/monitor_gee_task.R")

rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
reticulate::use_python(rgee_env_dir, required=T)

#ee_clean_user_credentials()
#ee$Authenticate(auth_mode='notebook')
#ee_Authenticate(auth_mode='notebook')
ee$Initialize(project = "ee-jonastrepel")
drive_auth(email = "jonas.trepel@bio.au.dk")
ee$String('Hello from the Earth Engine servers!')$getInfo()

### Get sub-saharan Africa extent
ssa_ext <- ee$Geometry$Rectangle(
  coords = c(7.5, -35.0, 45.0, 5.0), # xmin, ymin, xmax, ymax
  proj = "EPSG:4326",
  geodesic = FALSE
)



############################# COVARIATES #############################


##### EVI Mean ------------------------

evi_img <- ee$
  ImageCollection("MODIS/061/MOD13A1")$
  map(function(img) {
  qa <- img$select("SummaryQA")
  img$updateMask(qa$eq(0))
})$ ## select only high quality data
  select("EVI")$
  filterDate("2001-01-01", "2024-12-31")$
  mean()

export_evi <- ee_image_to_drive(
  image = evi_img,
  region = ssa_ext,
  folder = "rgee_backup_evi",
  description = "evi",
  scale = 500,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_evi$start()

Sys.sleep(60)
monitor_gee_task(pattern = "evi", path = "rgee_backup_evi")

drive_files_evi <- drive_ls(path = "rgee_backup_evi", pattern = "evi") %>%
  dplyr::select(name) %>% 
  unique()

# since it's only one tile we can save it directly 
filename_evi <- unique(drive_files_evi$name)
drive_download(file = filename_evi, path = "data/spatial_data/covariates/raster/mean_evi_2001_2024.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_evi$name))
googledrive::drive_rm("rgee_backup_evi")

evi_r <- rast("data/spatial_data/covariates/raster/mean_evi_2001_2024.tif")
plot(evi_r)


##### EVI Dry Season (May - September) ---------------------------------
#https://www.cpc.ncep.noaa.gov/products/assessments/assess_96/safr.html

evi_dry <- ee$ImageCollection("MODIS/061/MOD13A1")$
  filterDate("2001-01-01", "2024-12-31")$
  filter(ee$Filter$calendarRange(5, 9, 'month'))$  #Filter for May to September
  map(function(img) {
    qa <- img$select("SummaryQA")
    img$updateMask(qa$eq(0))$select("EVI")
  })$
  mean()

export_evi_dry <- ee_image_to_drive(
  image = evi_dry,
  region = ssa_ext,
  folder = "rgee_backup_evi_dry",
  description = "evi_dry",
  scale = 500,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_evi_dry$start()
Sys.sleep(60)
monitor_gee_task(pattern = "evi_dry", path = "rgee_backup_evi_dry")

(drive_files_evi_dry <- drive_ls(path = "rgee_backup_evi_dry", pattern = "evi_dry") %>%
  dplyr::select(name) %>% 
  unique())

# since it's only one tile we can save it directly 
filename_evi_dry <- unique(drive_files_evi_dry$name)
drive_download(file = filename_evi_dry, path = "data/spatial_data/covariates/raster/dry_season_mean_evi_2001_2024.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_evi_dry$name))
googledrive::drive_rm("rgee_backup_evi_dry")

evi_dry_r <- rast("data/spatial_data/covariates/raster/dry_season_mean_evi_2001_2024.tif")
plot(evi_dry_r)


##### EVI Wet Season (October - April) --------------------------------------------------
#https://www.cpc.ncep.noaa.gov/products/assessments/assess_96/safr.html


evi_wet <- ee$ImageCollection("MODIS/061/MOD13A1")$
  filterDate("2001-01-01", "2024-12-31")$
  filter(ee$Filter$calendarRange(10, 4, 'month'))$  #Filter for October to April
  map(function(img) {
    qa <- img$select("SummaryQA")
    img$updateMask(qa$eq(0))$select("EVI")
  })$
  mean()

export_evi_wet <- ee_image_to_drive(
  image = evi_wet,
  region = ssa_ext,
  folder = "rgee_backup_evi_wet",
  description = "evi_wet",
  scale = 500,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_evi_wet$start()
Sys.sleep(60)
monitor_gee_task(pattern = "evi_wet", path = "rgee_backup_evi_wet")

drive_files_evi_wet <- drive_ls(path = "rgee_backup_evi_wet", pattern = "evi_wet") %>%
  dplyr::select(name) %>% 
  unique()

# since it's only one tile we can save it directly 
filename_evi_wet <- unique(drive_files_evi_wet$name)
drive_download(file = filename_evi_wet, path = "data/spatial_data/covariates/raster/wet_season_mean_evi_2001_2024.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_evi_wet$name))
googledrive::drive_rm("rgee_backup_evi_wet")

evi_wet_r <- rast("data/spatial_data/covariates/raster/wet_season_mean_evi_2001_2024.tif")
plot(evi_wet_r)



##### ESA World Cover -------------------------------------------------
esa_wc_img <- ee$ImageCollection("ESA/WorldCover/v200")$
  select("Map")$
  first()$
  toInt16()

Map$addLayer(esa_wc_img)

export_esa_wc <- ee_image_to_drive(
  image = esa_wc_img,
  region = ssa_ext,
  folder = "rgee_backup_esa_wc",
  description = "esa_world_cover",
  scale = 10,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_esa_wc$start()


Sys.sleep(60)
monitor_gee_task(pattern = "esa_world_cover", path = "rgee_backup_esa_wc", 
                 last_sleep_time = 3600)

Sys.sleep(600)
(esa_wc_drive_files <- drive_ls(path = "rgee_backup_esa_wc", pattern = "esa_world_cover") %>%
  dplyr::select(name) %>% 
  unique())

for(filename in unique(esa_wc_drive_files$name)){
  
  path_name = paste0("data/spatial_data/raw_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}


esa_wc_files <- list.files("data/spatial_data/raw_tiles", full.names = T, pattern = "esa_world_cover")

esa_wc_raster_list <- lapply(esa_wc_files, rast)

esa_wc_file_name_merge <- paste0("data/spatial_data/covariates/raster/esa_world_cover_2021_10m.tif")

data_type_esa_wc <- terra::datatype(esa_wc_raster_list[[1]])

esa_wc_r <- merge(sprc(esa_wc_raster_list),
                  filename = esa_wc_file_name_merge,
                  overwrite = TRUE,
                  datatype = data_type_esa_wc)
plot(esa_wc_r)

googledrive::drive_rm(unique(esa_wc_drive_files$name))
googledrive::drive_rm("rgee_backup_esa_wc")
file.remove(esa_wc_files)

esa_wc_r <- rast("data/spatial_data/covariates/raster/esa_world_cover_2021_10m.tif")

wc_50m_r <- terra::aggregate(esa_wc_r, 
                              fact = 5, 
                              fun = "modal", 
                              filename = "data/spatial_data/covariates/raster/esa_world_cover_2021_50m.tif", 
                              overwrite = TRUE, 
                              cores = 20)
plot(wc_50m_r)

##### ESA Worldcover 100m


# Load and select the ESA WorldCover image
esa_wc_img <- ee$ImageCollection("ESA/WorldCover/v200")$
  select("Map")$
  first()

# Define the target projection: 100m resolution, using the native EPSG
esa_proj <- esa_wc_img$projection()$atScale(100)

# Reduce resolution using mode (most common class) and reproject to 100m
esa_wc_img_100m <- esa_wc_img$
  reduceResolution(
    reducer = ee$Reducer$mode(),
    maxPixels = 1024
  )$
  reproject(crs = esa_proj, scale = 100)

# Optional: visualize the layer (Map viewer is limited to 5000x5000px)
Map$centerObject(esa_wc_img)
Map$addLayer(esa_wc_img_100m, list(min = 10, max = 100), "ESA WC 100m (mode)")

# Export to Google Drive
export_esa_wc <- ee_image_to_drive(
  image = esa_wc_img_100m,
  region = ssa_ext,  # Make sure ssa_ext is defined (an ee$Geometry)
  folder = "rgee_backup_esa_wc",
  description = "esa_world_cover_100m_mode",
  scale = 100,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_esa_wc$start()


##### Water ESA worldcover -----------------------------

esa_img <- ee$ImageCollection("ESA/WorldCover/v200")$
  select("Map")$
  first()

esa_water <- esa_img$eq(80)$rename("water_binary") #1 oe NA
Map$addLayer(esa_water, list(min = 0, max = 1), "ESA Water")

export_esa_water <- ee_image_to_drive(
  image = esa_water,
  region = ssa_ext,
  folder = "rgee_backup_esa_water",
  description = "esa_water",
  scale = 10,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_esa_water$start()


Sys.sleep(60)
monitor_gee_task(pattern = "esa_water", path = "rgee_backup_esa_water")

Sys.sleep(600)
(esa_water_drive_files <- drive_ls(path = "rgee_backup_esa_water", pattern = "esa_water") %>%
  dplyr::select(name) %>% 
  unique())

for(filename in unique(esa_water_drive_files$name)){
  
  path_name = paste0("data/spatial_data/raw_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}

googledrive::drive_rm(unique(esa_water_drive_files$name))
googledrive::drive_rm("rgee_backup_esa_water")

esa_water_files <- list.files("data/spatial_data/raw_tiles", full.names = T, pattern = "esa_water")

esa_water_raster_list <- lapply(esa_water_files, rast)

data_type_esa_water <- terra::datatype(esa_water_raster_list[[1]])

esa_water_file_name_merge <- paste0("data/spatial_data/covariates/raster/esa_wc_water_2021_10m.tif")

esa_water_r <- merge(sprc(esa_water_raster_list),
            filename = esa_water_file_name_merge,
            overwrite = TRUE,
            datatype = data_type_esa_water)
plot(esa_water_r)
 
file.remove(esa_water_files)


##### DEM -------------------------------------------

dem_img <- ee$Image("NASA/NASADEM_HGT/001")$
  select("elevation")

dem_img$getInfo()

export_dem <- ee_image_to_drive(
  image = dem_img,
  region = ssa_ext,
  folder = "rgee_backup_dem",
  description = "dem",
  scale = 30,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_dem$start()

Sys.sleep(60)
monitor_gee_task(pattern = "dem", path = "rgee_backup_dem")

Sys.sleep(600)
(drive_files_dem <- drive_ls(path = "rgee_backup_dem", pattern = "dem") %>%
  dplyr::select(name) %>% 
  unique())

for(filename in unique(drive_files_dem$name)){
  
  path_name = paste0("data/spatial_data/raw_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}

googledrive::drive_rm(unique(drive_files_dem$name))
googledrive::drive_rm("rgee_backup_dem")

dem_files <- list.files("data/spatial_data/raw_tiles", full.names = T, pattern = "dem")

dem_raster_list <- lapply(dem_files, rast)

data_type_dem <-  terra::datatype(dem_raster_list[[1]])

dem_file_name_merge <- paste0("data/spatial_data/covariates/raster/nasa_dem_30m.tif")

dem_r <- merge(sprc(dem_raster_list), 
               filename = dem_file_name_merge,
               overwrite = TRUE,
               datatype = data_type_dem)
plot(dem_r)
file.remove(dem_files)


##### World Settlement Footprint 2015  -----------------------------------------------
# https://www.nature.com/articles/s41597-020-00580-5

wsf_img <- ee$Image("DLR/WSF/WSF2015/v1")$
  select("WSF")

# Reclassify: 255 becomes 1, others become NA (mask)
wsf_binary <- ee$Image(0)$where(wsf_img$eq(255), 1)$toUint8()
Map$addLayer(wsf_binary, list(min = 0, max = 1), "WSF 10m")


export_wsf <- ee_image_to_drive(
  image = wsf_binary,
  region = ssa_ext,
  folder = "rgee_backup_wsf",
  description = "wsf",
  scale = 10,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_wsf$start()

Sys.sleep(60)
monitor_gee_task(pattern = "wsf", path = "rgee_backup_wsf")

Sys.sleep(600)
(drive_files_wsf <- drive_ls(path = "rgee_backup_wsf", pattern = "wsf") %>%
  dplyr::select(name) %>% 
  unique())

for(filename in unique(drive_files_wsf$name)){
  
  path_name = paste0("data/spatial_data/raw_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}

googledrive::drive_rm(unique(drive_files_wsf$name))
googledrive::drive_rm("rgee_backup_wsf")

wsf_files <- list.files("data/spatial_data/raw_tiles", full.names = T, pattern = "wsf")

wsf_raster_list <- lapply(wsf_files, rast)


plot(wsf_raster_list[[1]])
terra::hasValues(wsf_raster_list[[1]])


data_type_wsf <- datatype(wsf_raster_list[[1]])

wsf_file_name_merge <- paste0("data/spatial_data/covariates/raster/world_settlement_footprint_2015_10m.tif")

wsf_r <- merge(sprc(wsf_raster_list), 
               filename = wsf_file_name_merge, 
               overwrite = TRUE, 
               datatype = data_type_wsf, #potentially change datatype?
               tempdir = "data/spatial_data/terra_temp_dir", 
               todisk = TRUE, 
               memfrac = 0.4
               )
plot(wsf_r)
file.remove(wsf_files)



##### Roads -> not available at GEE, but there's a vector layer: 
#https://www.globio.info/download-grip-dataset




