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



############################# COVARIATES #############################

### Drought ------

start_date <- paste0(2015, "-01-01")
end_date <- paste0(2022, "-12-31")

spei_mean <- ee$ImageCollection("CSIC/SPEI/2_10")$
  filterDate(start_date, end_date)$
  filterBounds(aoi)$ 
  select("SPEI_12_month")$
  mean() 

Map$centerObject(aoi)
Map$addLayer(
  spei_mean,
  list(min = -2.5, max = 2.5, palette = c("red", "white", "blue")),
  "Mean SPEI"
)

export_mean_spei <- ee_image_to_drive(
  image = spei_mean,
  region = aoi,
  folder = "rgee_backup_mean_spei",
  description = "mean_spei",
  scale = 55660,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_mean_spei$start()

Sys.sleep(60)
monitor_gee_task(pattern = "mean_spei", path = "rgee_backup_mean_spei",
                 mail = mail, last_sleep_time = 10)

drive_files_mean_spei <- drive_ls(path = "rgee_backup_mean_spei", pattern = "mean_spei") %>%
  dplyr::select(name) %>% 
  unique()

# since it's only one tile we can save it directly 
filename_mean_spei <- unique(drive_files_mean_spei$name)
drive_download(file = filename_mean_spei, path = "data/spatial_data/covariates/raster/mean_spei_2000_2023.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_mean_spei$name))
googledrive::drive_rm("rgee_backup_spei_mean")

mean_spei_r <- rast("data/spatial_data/covariates/raster/mean_spei_2000_2023.tif")
plot(mean_spei_r)

#Numer of months with severe drought 

spei_ic <- ee$ImageCollection("CSIC/SPEI/2_10")$
  filterDate(start_date, end_date)$
  filterBounds(aoi)$ 
  select("SPEI_12_month") 

spei_binary_severe <- spei_ic$map(function(img) {
  img$lte(-1.6)$rename("binary")$copyProperties(img, img$propertyNames()) #values lower than 1.6 indicate severe droughts 
})

spei_sum_severe2 <- spei_binary_severe$sum()$toDouble()

Map$centerObject(aoi)
Map$addLayer(
  spei_sum_severe,
  list(min = 0, max = 100, palette = c("white", "yellow", "orange", "red")),
  "N droughts SPEI"
)

export_spei_severe <- ee_image_to_drive(
  image = spei_sum_severe,
  region = aoi,
  folder = "rgee_backup_spei_severe",
  description = "spei_severe",
  scale = 55660,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_spei_severe$start()

Sys.sleep(60)
monitor_gee_task(pattern = "spei_severe", path = "rgee_backup_spei_severe",
                 mail = mail, last_sleep_time = 10)

drive_files_spei_severe <- drive_ls(path = "rgee_backup_spei_severe", pattern = "spei_severe") %>%
  dplyr::select(name) %>% 
  unique()

# since it's only one tile we can save it directly 
filename_spei_severe <- unique(drive_files_spei_severe$name)
drive_download(file = filename_spei_severe, path = "data/spatial_data/covariates/raster/spei_months_severe_drought_2000_2023.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_spei_severe$name))
googledrive::drive_rm("rgee_backup_spei_severe")

spei_severe_r <- rast("data/spatial_data/covariates/raster/spei_months_severe_drought_2000_2023.tif")
plot(spei_severe_r)


#Numer of months with extreme drought 

spei_ic <- ee$ImageCollection("CSIC/SPEI/2_10")$
  filterDate(start_date, end_date)$
  filterBounds(aoi)$ 
  select("SPEI_12_month") 

spei_binary_extreme <- spei_ic$map(function(img) {
  img$lte(-2)$rename("binary")$copyProperties(img, img$propertyNames()) #values lower than 1.6 indicate extreme droughts 
})

spei_sum_extreme <- spei_binary_extreme$sum()$toDouble()

Map$centerObject(aoi)
Map$addLayer(
  spei_sum_extreme,
  list(min = 0, max = 100, palette = c("white", "yellow", "orange", "red")),
  "N droughts SPEI"
)

export_spei_extreme <- ee_image_to_drive(
  image = spei_sum_extreme,
  region = aoi,
  folder = "rgee_backup_spei_extreme",
  description = "spei_extreme",
  scale = 55660,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_spei_extreme$start()

Sys.sleep(60)
monitor_gee_task(pattern = "spei_extreme", path = "rgee_backup_spei_extreme",
                 mail = mail, last_sleep_time = 10)

drive_files_spei_extreme <- drive_ls(path = "rgee_backup_spei_extreme", pattern = "spei_extreme") %>%
  dplyr::select(name) %>% 
  unique()

# since it's only one tile we can save it directly 
filename_spei_extreme <- unique(drive_files_spei_extreme$name)
drive_download(file = filename_spei_extreme, path = "data/spatial_data/covariates/raster/spei_months_extreme_drought_2000_2023.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_spei_extreme$name))
googledrive::drive_rm("rgee_backup_spei_extreme")

spei_extreme_r <- rast("data/spatial_data/covariates/raster/spei_months_extreme_drought_2000_2023.tif")
plot(spei_extreme_r)

### PDSI 
start_date <- paste0(2000, "-07-01")
end_date <- paste0(2025, "-6-30")

pdsi_mean <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate(start_date, end_date)$
  filterBounds(aoi)$ 
  select("pdsi")$
  mean()$
  divide(1000)$
  multiply(-1) #flippin it so that higher values, more severe drought

Map$centerObject(aoi)
Map$addLayer(
  pdsi_mean,
  list(min = -5, max = 5, palette = c("blue", "white", "red")),
  "Mean PDSI"
)


export_pdsi <- ee_image_to_drive(
  image = pdsi_mean,
  region = aoi,
  folder = "rgee_backup_pdsi",
  description = "pdsi",
  scale = 500,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_pdsi$start()

Sys.sleep(60)
monitor_gee_task(pattern = "pdsi", path = "rgee_backup_pdsi",
                 mail = mail, last_sleep_time = 10)

drive_files_pdsi <- drive_ls(path = "rgee_backup_pdsi", pattern = "pdsi") %>%
  dplyr::select(name) %>% 
  unique()

# since it's only one tile we can save it directly 
filename_pdsi <- unique(drive_files_pdsi$name)
drive_download(file = filename_pdsi, path = "data/spatial_data/covariates/raster/mean_flipped_pdsi_2000_2025.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_pdsi$name))
googledrive::drive_rm("rgee_backup_pdsi_mean")

pdsi_r <- rast("data/spatial_data/covariates/raster/mean_flipped_pdsi_2000_2025.tif")
plot(pdsi_r)

###### HLS EVI MEan -----------

start_date <- paste0(2013, "-07-01")
end_date <- paste0(2025, "-6-30")


hls_ic <- ee$ImageCollection("NASA/HLS/HLSL30/v002")$
  filterDate(start_date, end_date)$
  filterBounds(aoi)

# Mask out low quality pixels 
mask_fmask <- function(img) {
  fmask <- img$select("Fmask")
  
  # Bits:
  # bit 1 = cloud
  no_cloud <- fmask$bitwiseAnd(2)$eq(0)
  
  # bit 2 = adjacent to cloud
  no_cloud_adj <- fmask$bitwiseAnd(4)$eq(0)
  
  # bit 3 = cloud shadow
  no_shadow <- fmask$bitwiseAnd(8)$eq(0)
  
  # bit 4 = snow/ice
  no_snow <- fmask$bitwiseAnd(16)$eq(0)
  
  #bit 5 water
  no_water <- fmask$bitwiseAnd(32)$eq(0)
  
  # Keep only pixels with all these conditions = 1 (good)
  mask <- no_cloud$And(no_cloud_adj)$And(no_shadow)$And(no_snow)$And(no_water)
  
  return(img$updateMask(mask))
}

hls_masked <- hls_ic$map(mask_fmask)


hls_mean_bands <- hls_masked$
  select(c("B5", "B4", "B2"))$ # NIR, RED, BLUE
  mean()

# Calculate EVI on the mean bands
hls_evi_mean_bands <- hls_mean_bands$expression(
  "2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))",
  list(
    NIR = hls_mean_bands$select("B5"),
    RED = hls_mean_bands$select("B4"),
    BLUE = hls_mean_bands$select("B2")
  )
)$rename("EVI")

# Scale to integer if you like
hls_evi_mean <- hls_evi_mean_bands$multiply(10000)$round()

export_evi <- ee_image_to_drive(
  image = hls_evi_mean,
  region = aoi,
  folder = "rgee_backup_evi_mean",
  description = "evi_mean",
  scale = 30,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_evi$start()


Sys.sleep(60)
monitor_gee_task(pattern = "evi_mean", path = "rgee_backup_evi_mean",
                 last_sleep_time = 600, mail = "jonas.trepel@gmail.com")

Sys.sleep(2400)
(evi_drive_files <- drive_ls(path = "rgee_backup_evi_mean",
                             pattern = "evi_mean") %>%
    dplyr::select(name) %>% 
    unique())

for(filename in unique(evi_drive_files$name)){
  
  path_name = paste0("data/spatial_data/raw_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}


evi_files <- list.files("data/spatial_data/raw_tiles",
                        full.names = T, pattern = "evi_mean")

evi_raster_list <- lapply(evi_files, rast)

evi_file_name_merge <- paste0("data/spatial_data/covariates/raster/hls_mean_evi_2013_2025.tif")

data_type_evi <- terra::datatype(evi_raster_list[[1]])

evi_r <- merge(sprc(evi_raster_list),
               filename = evi_file_name_merge,
               overwrite = TRUE,
               datatype = data_type_evi)
plot(evi_r)


file.remove(evi_files)
googledrive::drive_rm(unique(evi_drive_files$name))
googledrive::drive_rm("rgee_backup_evi_mean")

print(paste0("HLS mean Evi done. Time: ", Sys.time()))

#clamp

r <- rast("data/spatial_data/covariates/raster/hls_mean_evi_2013_2025.tif")
plot(r)
r_clamped <- clamp(r, 
                  lower = 0, 
                  upper =  10000,
                  values = FALSE,
                  filename = "data/spatial_data/covariates/raster/hls_clamped_mean_evi_2013_2025.tif", 
                  overwrite = T)
plot(r_clamped)


#aggregate 

r_clamped <- rast("data/spatial_data/covariates/raster/hls_clamped_mean_evi_2013_2025.tif")
r_agg <- aggregate(r_clamped, 
                   fact = 3, 
                   fun = "mean", 
                   filename = "data/spatial_data/covariates/raster/hls_clamped_mean_evi_2013_2025_90m.tif", 
                   overwrite = T, 
                   cores = 20)
plot(r_agg)
##### EVI Mean ------------------------

evi_img <- ee$
  ImageCollection("MODIS/061/MOD13A1")$
  map(function(img) {
  qa <- img$select("SummaryQA")
  img$updateMask(qa$eq(0))
})$ ## select only high quality data
  select("EVI")$
  filterDate("2001-07-01", "2025-06-30")$
  mean()

export_evi <- ee_image_to_drive(
  image = evi_img,
  region = aoi,
  folder = "rgee_backup_evi_mean",
  description = "evi",
  scale = 500,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_evi$start()

Sys.sleep(60)
monitor_gee_task(pattern = "evi", path = "rgee_backup_evi_mean",
                 mail = mail, last_sleep_time = 10)

drive_files_evi <- drive_ls(path = "rgee_backup_evi_mean", pattern = "evi") %>%
  dplyr::select(name) %>% 
  unique()

# since it's only one tile we can save it directly 
filename_evi <- unique(drive_files_evi$name)
drive_download(file = filename_evi, path = "data/spatial_data/covariates/raster/mean_evi_2001_2024.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_evi$name))
googledrive::drive_rm("rgee_backup_evi_mean")

evi_r <- rast("data/spatial_data/covariates/raster/mean_evi_2001_2024.tif")
plot(evi_r)


##### EVI Dry Season (May - September) ---------------------------------
#https://www.cpc.ncep.noaa.gov/products/assessments/assess_96/safr.html

evi_dry <- ee$ImageCollection("MODIS/061/MOD13A1")$
  filterDate("2001-07-01", "2025-06-30")$
  filter(ee$Filter$calendarRange(5, 9, 'month'))$  #Filter for May to September
  map(function(img) {
    qa <- img$select("SummaryQA")
    img$updateMask(qa$eq(0))$select("EVI")
  })$
  mean()

export_evi_dry <- ee_image_to_drive(
  image = evi_dry,
  region = aoi,
  folder = "rgee_backup_evi_dry",
  description = "evi_dry",
  scale = 500,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_evi_dry$start()
Sys.sleep(60)
monitor_gee_task(pattern = "evi_dry", path = "rgee_backup_evi_dry", 
                 mail = mail, last_sleep_time = 10)

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
  filterDate("2001-07-01", "2025-06-30")$
  filter(ee$Filter$calendarRange(10, 4, 'month'))$  #Filter for October to April
  map(function(img) {
    qa <- img$select("SummaryQA")
    img$updateMask(qa$eq(0))$select("EVI")
  })$
  mean()

export_evi_wet <- ee_image_to_drive(
  image = evi_wet,
  region = aoi,
  folder = "rgee_backup_evi_wet",
  description = "evi_wet",
  scale = 500,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_evi_wet$start()
Sys.sleep(60)
monitor_gee_task(pattern = "evi_wet", path = "rgee_backup_evi_wet", 
                 mail = mail, last_sleep_time = 10)

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
  region = aoi,
  folder = "rgee_backup_esa_wc",
  description = "esa_world_cover",
  scale = 10,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_esa_wc$start()


Sys.sleep(60)
monitor_gee_task(pattern = "esa_world_cover", path = "rgee_backup_esa_wc", 
                 last_sleep_time = 3600, mail = mail)

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



##### Water ESA worldcover -----------------------------

esa_img <- ee$ImageCollection("ESA/WorldCover/v200")$
  select("Map")$
  first()

esa_water <- esa_img$eq(80)$rename("water_binary") #1 oe NA
Map$addLayer(esa_water, list(min = 0, max = 1), "ESA Water")

export_esa_water <- ee_image_to_drive(
  image = esa_water,
  region = aoi,
  folder = "rgee_backup_esa_water",
  description = "esa_water",
  scale = 10,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_esa_water$start()


Sys.sleep(60)
monitor_gee_task(pattern = "esa_water", path = "rgee_backup_esa_water", 
                 mail = mail)

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
  region = aoi,
  folder = "rgee_backup_dem",
  description = "dem",
  scale = 30,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_dem$start()

Sys.sleep(60)
monitor_gee_task(pattern = "dem", path = "rgee_backup_dem", 
                 mail = mail)

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
  region = aoi,
  folder = "rgee_backup_wsf",
  description = "wsf",
  scale = 10,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_wsf$start()

Sys.sleep(60)
monitor_gee_task(pattern = "wsf", path = "rgee_backup_wsf", 
                 mail = mail)

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



##### Fire frequency #####

## important - here a year ranges from July of a year to June of year + 1 
## should probably be changed to Jan-Dec of year when used in nothern hemisphere

year_list <- ee$List$sequence(2001, 2024)
n_years <- 2024-2001

modis_burndate <- ee$ImageCollection("MODIS/061/MCD64A1")$
  select("BurnDate")

modis_qa <- ee$ImageCollection("MODIS/061/MCD64A1")$
  select("QA")

# Bitwise extraction function (bit 0: valid burn)
bitwise_extract <- function(input, from_bit, to_bit) {
  mask_size <- ee$Number(1)$add(to_bit)$subtract(from_bit)
  mask <- ee$Number(1)$leftShift(mask_size)$subtract(1)
  input$rightShift(from_bit)$bitwiseAnd(mask)
}

# Function to get annual binary burn map (1 = burned, 0 = unburned)
get_annual_binary <- function(year) {
  year <- ee$Number(year)
  next_year <- year$add(1)
  
  # BurnDate and QA filtered by July 1 - June 30
  start_date <- ee$Date$fromYMD(year, 7, 1)
  end_date <- ee$Date$fromYMD(next_year, 6, 30)
  
  burn_img <- modis_burndate$
    filterDate(start_date, end_date)$
    select("BurnDate")$
    max()
  
  qa_img <- modis_qa$
    filterDate(start_date, end_date)$
    select("QA")$
    max()
  
  # Mask: burn pixels with good quality
  mask <- bitwise_extract(qa_img, 0, 0)$eq(1)
  
  # Burned = 1, Unburned = 0
  burned_bin <- burn_img$
    where(burn_img$neq(0), 1)$
    unmask(0)$
    updateMask(mask)$
    rename("Burned")$
    set("system:time_start", start_date)
  
  return(burned_bin)
}

# Build the annual binary image collection
burned_col <- ee$ImageCollection$fromImages(
  year_list$map(ee_utils_pyfunc(function(yr) {
    get_annual_binary(yr)
  }))
)

# Sum the collection to get fire frequency
fire_frequency <- burned_col$sum()$clip(aoi)$divide(n_years)

# Visualization
vis_params <- list(
  min = 0,
  max = 1,
  palette = c("#ffffff", "#ffffb2", "#fd8d3c", "#e31a1c", "#b10026")  # white to dark red
)
Map$centerObject(aoi, 6)
Map$addLayer(fire_frequency, vis_params, "Fire Frequency")


export_fire_frequency <- ee_image_to_drive(
  image = fire_frequency,
  region = aoi,
  folder = "rgee_backup_fire_frequency",
  description = "fire_frequency",
  scale = 500,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_fire_frequency$start()

Sys.sleep(60)
monitor_gee_task(pattern = "fire_frequency", path = "rgee_backup_fire_frequency",
                 mail = mail, last_sleep_time = 10)

drive_files_fire_frequency <- drive_ls(path = "rgee_backup_fire_frequency", pattern = "fire_frequency") %>%
  dplyr::select(name) %>% 
  unique()

# since it's only one tile we can save it directly 
filename_fire_frequency <- unique(drive_files_fire_frequency$name)
drive_download(file = filename_fire_frequency, path = "data/spatial_data/covariates/raster/fire_frequency_500m_2001_2024.tif", overwrite = TRUE)
googledrive::drive_rm(unique(drive_files_fire_frequency$name))
googledrive::drive_rm("rgee_backup_fire_frequency")

fire_frequency_r <- rast("data/spatial_data/covariates/raster/fire_frequency_500m_2001_2024.tif")
plot(fire_frequency_r)

