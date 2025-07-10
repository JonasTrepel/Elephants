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
  
  ## get burn date band 
  annual_img_col <- ee$ImageCollection("MODIS/061/MCD64A1")$
    filterDate(start_date, end_date)$
    select("BurnDate")
  
  Map$addLayer(annual_img_col$mean())
  
  # Define the image collection and select QA band
  QA <- ee$ImageCollection("MODIS/061/MCD64A1")$
    filterDate(start_date, end_date)$
    select("QA")
  
  #print(QA$getInfo())
  
  # Function to modify the BurnDate band
  set_non_zero_to_one <- function(image) {
    band <- image$select("BurnDate")
    modifiedBand <- band$where(band$neq(0), 1)
    image$addBands(modifiedBand, NULL, TRUE)$rename("BurnDateModified")
  }
  
  # Apply the function to each image in the collection
  
  annual_01 <- annual_img_col$map(set_non_zero_to_one)
  Map$addLayer(annual_01$max())
  
  bitwise_extract = function(input, fromBit, toBit) {
    maskSize = ee$Number(1)$add(toBit)$subtract(fromBit)
    mask = ee$Number(1)$leftShift(maskSize)$subtract(1)
    return(input$rightShift(fromBit)$bitwiseAnd(mask))
  }
  
  # Function to count the number of fires in a pixel per year
  yearly_fires <- function(year) {
    
    year_plus_one <- ee$Number(year)$add(1)
    
    fire_sub <- annual_01$
      filter(ee$Filter$calendarRange(year, year_plus_one, "year"))$
      max()
    
    qa_sub <-  QA$filter(ee$Filter$calendarRange(year, year_plus_one, 'year'))$max();
    
    mask <- bitwise_extract(qa_sub, 0, 0)$eq(1)
    
    fire_sub2 <- fire_sub$
      unmask(0)$
      updateMask(mask)$
      set("system:time_start", ee$Date$fromYMD(year, 1, 1))
    
    return(fire_sub2)
  }
  
  years_ls <- ee$List$sequence(year, year+1)  # 
  
  # Create an ImageCollection of annual burned area
  annual_col <- ee$ImageCollection$fromImages(
    years_ls$map(ee_utils_pyfunc(function(anno) {
      yearly_fires(anno)
    }))
  )
  
  # Function to reduce resolution of images and calculate fraction burned area
  burned_area_fun <- function(year) {
    
    annual_burned_sub <- annual_col$
      filter(ee$Filter$calendarRange(year, year, "year"))$
      max()$
      setDefaultProjection(
        crs = "SR-ORG:6974",
        crsTransform = c(463.3127165279165, 0, -20015109.354, 
                         0, -463.3127165279167, 7783653.6376640005)
      )$
      reduceResolution(
        reducer = ee$Reducer$mean(), # Fractional cover 
        maxPixels = 255
      )$
      reproject(
        scale = 5000,
        crs = "SR-ORG:6974"
      )$
      rename("BurnedFraction")
    
    annual_burned_sub$
      set("year", year)$
      set("system:time_start", ee$Date$fromYMD(year, 1, 1))
  }
  
  # Create an ImageCollection from the function applied to each year
  burned_area <- ee$ImageCollection$fromImages(
    years_ls$map(ee_utils_pyfunc(function(anno) {
      burned_area_fun(anno)
    }))
  )
  
  # Compute mean burned fraction and display
  mean_burned <- burned_area$reduce(ee$Reducer$mean())
  
  burned_vis_params <- list(
    min = 0, max = 1, palette = c("#ffffff", "#ffeda0", "#feb24c", "#f03b20", "#bd0026"))
  
  # Add mean burned fraction to the map
  Map$centerObject(aoi, zoom= 5)
  Map$addLayer(mean_burned, visParams = burned_vis_params, name = "Mean Burned Fraction")
  
  
  export_task_burned_area <- ee_image_to_drive(image = mean_burned,
                                   region = aoi,
                                   folder = "rgee_backup_burned_area",
                                   description = "annual_burned_area",
                                   scale = 5000, 
                                   timePrefix = FALSE, 
                                   maxPixels = 1e13
  )
  export_task_burned_area$start()
  
  Sys.sleep(60)
  monitor_gee_task(pattern = "annual_burned_area", path = "rgee_backup_burned_area", 
                   last_sleep_time = 10)
  
  drive_files_burned_area <- drive_ls(path = "rgee_backup_burned_area", pattern = "annual_burned_area") %>%
    dplyr::select(name) %>% 
    unique()
  
  # since it's only one tile we can save it directly 
  filename_burned_area <- unique(drive_files_burned_area$name)
  filepath_burned_area <- paste0("data/spatial_data/time_series/burned_area_5000m_", year, ".tif")
  
  drive_download(file = filename_burned_area, path = filepath_burned_area, overwrite = TRUE)
  
  googledrive::drive_rm(unique(drive_files_burned_area$name))
  googledrive::drive_rm("rgee_backup_evi")
  
  burned_area_r <- rast(filepath_burned_area)
  
  plot(burned_area_r, main = paste0(year))
  
  #file.remove(files)
  
  print(paste0(year, " burned area done"))
  
}
  