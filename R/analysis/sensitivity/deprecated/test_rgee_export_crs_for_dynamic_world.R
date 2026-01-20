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

ee_clean_user_credentials()
ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
ee$String('Hello from the Earth Engine servers!')$getInfo()



#define area of interest
aoi <- ee$Geometry$Rectangle(
  coords = c(31.6775, -28.4404, 32.1707, -27.9972), #HiP
  proj = "EPSG:4326",
  geodesic = FALSE
)

Map$addLayer(aoi)

#test the resolution problem (or rather if it is a problem)
  
  start_date <- paste0(2015, "-07-01")
  end_date <- paste0(2025, "-6-30")
  
  
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
  
  #Default CRS. Should be the native image CRS, somehow is 4326
  export_dynamic_4326 <- ee_image_to_drive(
    image = dynamic_world_img,
    region = aoi,
    folder = "rgee_backup_dynamic_world",
    description = "dynamic_world_4326",
    scale = 10,
    timePrefix = FALSE,
    maxPixels = 1e13)
  export_dynamic_4326$start()
  
  #Local UTM zone
  export_dynamic_world_utm <- ee_image_to_drive(
    image = dynamic_world_img,
    region = aoi,
    folder = "rgee_backup_dynamic_world",
    description = "dynamic_world_utm",
    scale = 10,
    crs = "EPSG:32736",
    timePrefix = FALSE,
    maxPixels = 1e13)
  export_dynamic_world_utm$start()
  
#Can't find a nice equal area projection 
  # export_dynamic_world_ea <- ee_image_to_drive(
  #   image = dynamic_world_img,
  #   region = aoi,
  #   folder = "rgee_backup_dynamic_world",
  #   description = "dynamic_world_ea",
  #   scale = 10,
  #   crs = "ESRI:54034",
  #   timePrefix = FALSE,
  #   maxPixels = 1e13)
  # export_dynamic_world_ea$start()
  
  #All default
  export_dynamic_def <- ee_image_to_drive(
    image = dynamic_world_img,
    region = aoi,
    folder = "rgee_backup_dynamic_world",
    description = "dynamic_world_default",
    timePrefix = FALSE,
    maxPixels = 1e13)
  export_dynamic_def$start()

  
  Sys.sleep(60)
  monitor_gee_task(pattern = "dynamic_world", path = "rgee_backup_dynamic_world",
                   last_sleep_time = 900)
  
  Sys.sleep(600)
  (dynamic_world_drive_files <- drive_ls(path = "rgee_backup_dynamic_world",
                                         pattern = "dynamic_world") %>%
      dplyr::select(name) %>% 
      unique())
  
  for(filename in unique(dynamic_world_drive_files$name)){
    
    path_name = paste0("data/spatial_data/test_export_crs/", filename)
    drive_download(file = filename, path = path_name, overwrite = TRUE)
  }
  

  r <- rast("data/spatial_data/test_export_crs/dynamic_world_default.tif")
  plot(r) #well, that's nonsence (1 degree res)
  
  r_4326 <- rast("data/spatial_data/test_export_crs/dynamic_world_4326.tif")
  plot(r_4326)
  
  r_utm <- rast("data/spatial_data/test_export_crs/dynamic_world_utm.tif")
  plot(r_utm)
  
  library(sf)
  library(exactextractr)
  r_sf <- ext(r_4326) %>%
    as.polygons(crs = crs(r_4326)) %>% 
    st_as_sf()           # convert to sf object
  
  # Generate 1000 random points inside the extent polygon
  set.seed(161)  # for reproducibility
  sf_use_s2(TRUE)
  sf_rp <- st_sample(r_sf, size = 10000, type = "random") %>% 
    st_as_sf() %>% 
    mutate(point_id = paste0("point_", 1:nrow(.))) %>% 
    st_buffer(50)
  sf_use_s2(FALSE)
  
  mapview::mapview(sf_rp)
  
  #transform 
  sf_utm <- sf_rp %>% 
    st_transform(crs = st_crs(r_utm))
  
  #extract 
  extr_4326 <- exactextractr::exact_extract(r_4326, 
                                            sf_rp, 
                                       append_cols = c("point_id"),
                                       fun = "mode")
  
  extr_utm <- exactextractr::exact_extract(r_utm, 
                                           sf_utm, 
                                            append_cols = c("point_id"),
                                            fun = "mode")
  
  dt_extr <- extr_utm %>%
    rename(mode_utm = mode) %>%
    left_join(extr_4326) %>% 
    rename(mode_4326 = mode)
  
  cor.test(dt_extr$mode_utm, dt_extr$mode_4326) #0.98
  
  dt_extr %>% 
    ggplot() +
    geom_jitter(aes(mode_utm, mode_4326), alpha = 0.1, width = 0.5, height = 0.5) + 
    geom_smooth(aes(mode_utm, mode_4326), method = "lm") #ok, looks pretty decent 
 

