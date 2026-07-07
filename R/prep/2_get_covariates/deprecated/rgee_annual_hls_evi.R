library(rgee)
library(tidyverse)
library(sf)
library(terra)
library(tidylog)
library(googledrive)

source("R/functions/monitor_gee_task.R")
#  
rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
reticulate::use_python(rgee_env_dir, required=T)
#ee_clean_user_credentials()
#ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
ee$String('Hello from the Earth Engine servers!')$getInfo()

#nice

#define area of interest
aoi <- ee$Geometry$Rectangle(
  coords = c(7.5, -35.0, 45.0, 5.0), # sub saharan africa xmin, ymin, xmax, ymax
  #coords = c(27.68, -24.18, 27.91, -23.99), #Kaingo
  proj = "EPSG:4326",
  geodesic = FALSE
)

Map$addLayer(aoi)

years <- c(2013:2024)

for(year in years){ 
  
year_1 = year+1

start_date <- paste0(year, "-07-01")
end_date <- paste0(year_1, "-6-30")


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

# Option 1: calculate EVI for each timestep, then average

calc_evi <- function(img) {
  evi <- img$expression(
    "2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))",
    list(
      NIR = img$select("B5"), 
      RED = img$select("B4"),   
      BLUE = img$select("B2") 
    )
  )$rename("EVI")
  
  return(img$addBands(evi))
}

hls_evi_ic <- hls_masked$map(calc_evi)

# Mean EVI composite
hls_evi_mean <- hls_evi_ic$select("EVI")$mean()$multiply(10000)$round()

# Visualization parameters
vis_params_evi <- list(
  bands = c("EVI"),
  min = 0,
  max = 7000,
  palette = c("red", "orange", "darkgreen")
)

Map$centerObject(aoi)
Map$addLayer(hls_evi_mean, vis_params_evi, "evi")


export_evi <- ee_image_to_drive(
  image = hls_evi_mean,
  region = aoi,
  folder = "rgee_backup_evi_annual",
  description = "hls_evi_annual",
  scale = 30,
  timePrefix = FALSE,
  maxPixels = 1e13
)
export_evi$start()


Sys.sleep(60)
monitor_gee_task(pattern = "hls_evi_annual", path = "rgee_backup_evi_annual",
                 last_sleep_time = 600, mail = "jonas.trepel@gmail.com")

Sys.sleep(2400)
(evi_drive_files <- drive_ls(path = "rgee_backup_evi_annual",
                                       pattern = "hls_evi_annual") %>%
    dplyr::select(name) %>% 
    unique())

for(filename in unique(evi_drive_files$name)){
  
  path_name = paste0("data/spatial_data/raw_tiles/", filename)
  drive_download(file = filename, path = path_name, overwrite = TRUE)
}


evi_files <- list.files("data/spatial_data/raw_tiles",
                                  full.names = T, pattern = "hls_evi_annual")

evi_raster_list <- lapply(evi_files, rast)

evi_file_name_merge <- paste0("data/spatial_data/time_series/hls_evi_", year,"_",year_1,"_30m.tif")

data_type_evi <- terra::datatype(evi_raster_list[[1]])

evi_r <- merge(sprc(evi_raster_list),
                         filename = evi_file_name_merge,
                         overwrite = TRUE,
                         datatype = data_type_evi)
plot(evi_r)

# writeRaster(evi_r, 
#             filename = evi_file_name_merge,
#             overwrite = TRUE,
#             datatype = data_type_evi)



file.remove(evi_files)
googledrive::drive_rm(unique(evi_drive_files$name))
googledrive::drive_rm("rgee_backup_evi_annual")

print(paste0(year, " done. Time: ", Sys.time()))

}


### Clamp to range -------------------------- 
files <- list.files("data/spatial_data/time_series/", pattern = "hls_evi")

library(furrr)
library(future)
library(terra)
plan(multisession, workers = 7)
future_walk(unique(files),
            .progress = TRUE,
            .options = furrr_options(seed = TRUE),
            function(file){


#for(file in unique(files)){
 
  r <- rast(paste0("data/spatial_data/time_series/", file))
  plot(r)
  
  new_file = gsub("hls_evi", "hsl_clamped_evi", file)
  
  r_clamped <- clamp(r, 
                     lower = 0, 
                     upper =  10000,
                     values = FALSE,
                     filename = paste0("data/spatial_data/time_series/", new_file), 
                     overwrite = T)
  plot(r_clamped) 
  
  rm(r_clamped)
  rm(r)
  
  
})
plan(sequential)


#just to archive it ----------------------
## Option 2: calculate mean reflectance first 
# 
# 
# hls_mean <- hls_masked$mean()
# 
# band_names <- hls_mean$bandNames()$getInfo()
# print(band_names)
# 
# calc_evi <- function(img) {
#   nir <- img$select("B5")
#   red <- img$select("B4")
#   blue <- img$select("B2")
#   
#   evi <- nir$subtract(red)$
#     divide(
#       nir$add(red$multiply(6))$
#         subtract(blue$multiply(7.5))$
#         add(1)
#     )$
#     multiply(2.5)$
#     rename("EVI")
#   
#   return(img$addBands(evi))
# }
# 
# hls_evi <- calc_evi(hls_mean)
# 
# hls_evi_expr <- hls_mean$expression(
#   "2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))",
#   list(
#     NIR = hls_mean$select("B5"),
#     RED = hls_mean$select("B4"),
#     BLUE = hls_mean$select("B2")
#   )
# )$rename("EVI")
# 
# 
# vis_params_evi <- list(
#   bands = c("EVI"),
#   min = 0,
#   max = 1,
#   palette = c("brown", "yellow", "green")
# )
# 
# Map$centerObject(aoi)
# Map$addLayer(hls_evi$select("EVI"), vis_params_evi, "EVI")
# 
# Map$centerObject(aoi)
# Map$addLayer(hls_evi_expr, vis_params_evi, "EVI (expression)")