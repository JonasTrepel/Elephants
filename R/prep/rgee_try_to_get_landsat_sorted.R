#  ADAPTED FOR RGEE FROM XANDER VENTER 
#   * Code for calculating EVI or NDVI trends
# * Originated from code behind Venter et al 2020 
# * https://www.sciencedirect.com/science/article/pii/S1470160X20301436
# * Adjusted to cater for Landsat Collection 2
# * Adjusted to include 3 types of trend calculation
# * Adjusted to calculate trend on either annual composites, or raw image time series
# */



library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)

source("R/functions/monitor_gee_task.R")
# 
# rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
# reticulate::use_python(rgee_env_dir, required=T)
ee_clean_user_credentials()
ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
ee$String('Hello from the Earth Engine servers!')$getInfo()



years <- c(2001:2024)

#define area of interest
aoi <- ee$Geometry$Rectangle(
  #coords = c(7.5, -35.0, 45.0, 5.0), # xmin, ymin, xmax, ymax
  coords = c(27.6, -24.18, 27.95, -24), #Kaingo
  proj = "EPSG:4326",
  geodesic = FALSE
)

Map$addLayer(aoi)
# Define years and dates for Landsat image collection
start_year <- 2015
end_year <- 2015
start_day <- "01-01"  # start of date filter (month-day)
end_day <- "12-31"    # end of date filter (month-day)

# Choose mosaicking method: "median" or "medoid"
mosaic_method <- "medoid"


########################################################################################################
##### PROCESSING FUNCTIONS ##### 
########################################################################################################

#----- Landsat cloud mask -----
mask_landsat_clouds <- function(image) {
  #Develop masks for unwanted pixels (fill, cloud, cloud shadow).
  qa_mask <- image$select("QA_PIXEL")$bitwiseAnd(strtoi("11111", base = 2))$eq(0)
  saturation_mask <- image$select("QA_RADSAT")$eq(0)
  #Replace original bands with scaled bands and apply masks.
  image$updateMask(qa_mask)$updateMask(saturation_mask)
}

#----- scaling factors for Landsat collection 2 -----
apply_scale_factors <- function(image) {
  optical_bands <- image$select("SR_B.")$multiply(0.0000275)$add(-0.2)
  optical_bands <- optical_bands$multiply(10000)
  image$addBands(optical_bands, NULL, TRUE)
}

#----- L8 to L7 HARMONIZATION FUNCTION -----
#slope and intercept citation: Roy, D.P., Kovalskyy, V., Zhang, H.K., Vermote, E.F., Yan, L., Kumar, S.S, Egorov, A., 2016, Characterization of Landsat-7 to Landsat-8 reflective wavelength and normalized difference vegetation index continuity, Remote Sensing of Environment, 185, 57-70.(http://dx.doi.org/10.1016/j.rse.2015.12.024); Table 2 - reduced major axis (RMA) regression coefficients
harmonization_roy <- function(oli) {
  slopes <- ee$Image$constant(c(0.9785, 0.9542, 0.9825, 1.0073, 1.0171, 0.9949))
  intercepts <- ee$Image$constant(c(-0.0095, -0.0016, -0.0022, -0.0021, -0.0030, 0.0029))
  harmonized <- oli$
    select(
      c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7"),
      c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7")
    )$
    resample("bicubic")$
    subtract(intercepts$multiply(10000))$
    divide(slopes)$
    set("system:time_start", oli$get("system:time_start"))
  
  harmonized$toShort()
}

#----- RETRIEVE A SENSOR SR (surface reflection?) COLLECTION FUNCTION -----
get_sr_collection <- function(year, start_day, end_day, sensor, aoi) {
  sr_collection <- ee$ImageCollection(paste0("LANDSAT/", sensor, "/C02/T1_L2"))$
    filterBounds(aoi)$
    filterDate(paste0(year, "-", start_day), paste0(year, "-", end_day))
  
  sr_collection <- sr_collection$map(apply_scale_factors)
  
  sr_collection <- sr_collection$map(function(img) {
    dat <- ee$Image(
      ee$Algorithms$If(
        sensor == "LC08",
        harmonization_roy(img$unmask()),
        img$
          select(c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7"))$
          unmask()$
          resample("bicubic")$
          set("system:time_start", img$get("system:time_start"))
      )
    )$addBands(img$select(c("QA_PIXEL", "QA_RADSAT")))
    dat
  })
  
  sr_collection$map(mask_landsat_clouds)
}

#----- FUNCTION TO COMBINE LT05, LE07, & LC08 COLLECTIONS -----
get_combined_sr_collection <- function(year, start_day, end_day, aoi) {
  lt5 <- get_sr_collection(year, start_day, end_day, "LT05", aoi)
  le7 <- get_sr_collection(year, start_day, end_day, "LE07", aoi)
  lc8 <- get_sr_collection(year, start_day, end_day, "LC08", aoi)
  merged <- lt5$merge(le7)$merge(lc8)
  merged$map(function(img) img$int())$sort("system:time_start")
}

#----- All: RETRIEVE A SENSOR SR COLLECTION FUNCTION -----
get_sr_collection_all <- function(start_year, end_year, start_day, end_day, sensor, aoi) {
  sr_collection <- ee$ImageCollection(paste0("LANDSAT/", sensor, "/C02/T1_L2"))$
    filterBounds(aoi)$
    filterDate(paste0(start_year, "-", start_day), paste0(end_year, "-", end_day))
  
  sr_collection <- sr_collection$map(apply_scale_factors)
  
  sr_collection <- sr_collection$map(function(img) {
    dat <- ee$Image(
      ee$Algorithms$If(
        sensor == "LC08",
        harmonization_roy(img$unmask()),
        img$
          select(c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7"))$
          unmask()$
          resample("bicubic")$
          set("system:time_start", img$get("system:time_start"))
      )
    )$addBands(img$select(c("QA_PIXEL", "QA_RADSAT")))
    dat
  })
  
  sr_collection$map(mask_landsat_clouds)
}

#----- FUNCTION TO COMBINE LT05, LE07, & LC08 COLLECTIONS -----
get_combined_sr_collection_all <- function(start_year, end_year, start_day, end_day, aoi) {
  lt5 <- get_sr_collection_all(start_year, end_year, start_day, end_day, "LT05", aoi)
  le7 <- get_sr_collection_all(start_year, end_year, start_day, end_day, "LE07", aoi)
  lc8 <- get_sr_collection_all(start_year, end_year, start_day, end_day, "LC08", aoi)
  merged <- lt5$merge(le7)$merge(lc8)
  merged$map(function(img) img$int())$sort("system:time_start")
}

#----- Reduce to annual composite using median or medoid -----
make_mosaic <- function(in_collection, mosaic_method) {
  in_collection <- in_collection$select(c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7"))
  median <- in_collection$median()
  
  diff_from_median <- in_collection$map(function(img) {
    diff <- ee$Image(img)$subtract(median)$pow(ee$Image$constant(2))
    diff$reduce("sum")$addBands(img)
  })
  
  medoid <- ee$ImageCollection(diff_from_median)$reduce(ee$Reducer$min(7))$
    select(c(1, 2, 3, 4, 5, 6), c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7"))
  
  if (tolower(mosaic_method) == "median") {
    median$select(c(0, 1, 2, 3, 4, 5), c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B7"))
  } else {
    medoid
  }
}


#FUNCTION TO APPLY MEDOID COMPOSITING FUNCTION TO A COLLECTIO
build_mosaic <- function(year, start_day, end_day, aoi) {
  collection <- get_combined_sr_collection(year, start_day, end_day, aoi)
  img <- make_mosaic(collection, mosaic_method)$
    set("system:time_start", ee$Date$fromYMD(year, 8, 1)$millis()) ## DOUBLE CHECK!
  ee$Image(img)
}


# FUNCTION TO BUILD ANNUAL MOSAIC COLLECTION
build_mosaic_collection <- function(start_year, end_year, start_day, end_day, aoi) {
  imgs <- list()
  for (i in start_year:end_year) {
    tmp <- build_mosaic(i, start_day, end_day, aoi)$
      set("system:time_start", ee$Date$fromYMD(i, 1, 1)$millis())
    imgs <- append(imgs, list(tmp))
  }
  ee$ImageCollection(imgs)
}

# ADD EVI 

add_evi <- function(image) {
  evi <- image$expression(
    "2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))",
    list(
      NIR = image$select("SR_B4"),
      RED = image$select("SR_B3"),
      BLUE = image$select("SR_B1")
    )
  )$rename("evi")$
    set("system:time_start", image$get("system:time_start"))
  evi
}

#Clam EVI to a given range and then scale between 0 and 1 for ease of interpreting trend values
clamp_evi <- function(img) {
img$
  clamp(0, 2.5)$
  unitScale(0, 2.5)$
  set("system:time_start", img$get("system:time_start"))
}


########################################################################################################
##### BUILD COLLECTIONS AND VISUALIZE #####
########################################################################################################


get_annual_mosaic_evi <- function(year, start_day, end_day, aoi, mosaic_method = "medoid") {
  # Get SR collection for one year
  collection <- get_combined_sr_collection(year, start_day, end_day, aoi)
  
  # Mosaic the collection
  mosaic <- make_mosaic(collection, mosaic_method)
  
  # Compute and scale EVI
  evi <- add_evi(mosaic)
  evi_scaled <- clamp_evi(evi)
  
  # Rename and set metadata
  evi_scaled$rename("mean_evi")$
    set("system:time_start", ee$Date$fromYMD(year, 7, 1)$millis())
}

mosaic_evi <- get_annual_mosaic_evi(2020, "06-01", "09-30", aoi)
Map$addLayer(mosaic_evi, list(min=0, max=1, palette=c("white", "green")), "Mosaic EVI 2020")



# Function to calculate annual mean EVI for a single year
get_annual_mean_evi <- function(year, start_day, end_day, aoi) {
  # Get combined, cloud-masked, and harmonized SR image collection
  sr_collection <- get_combined_sr_collection(year, start_day, end_day, aoi)
  
  # Calculate EVI for each image
  evi_collection <- sr_collection$
    map(add_evi)$
    map(clamp_evi)
  
  # Take the mean of EVI band
  mean_evi <- evi_collection$
    select("evi")$
    mean()$
    rename("mean_evi")$
    set("system:time_start", ee$Date$fromYMD(year, 7, 1)$millis()) # or another reference date
  
  mean_evi
}

mean_evi_image <- get_annual_mean_evi(2020, "01-01", "12-31", aoi)
Map$centerObject(aoi)
Map$addLayer(mean_evi_image, list(min=0, max=1, palette=c("white", "green")), "Mean EVI 2020")

# Build annual mosaicked surface reflectance collection
annual_sr_collection <- build_mosaic(start_year, start_day, end_day, aoi)$
  map(add_evi)$
  map(clamp_evi)

Map$addLayer(annual_sr_collection$select("evi")$mean(), list(min = 0, max = 1), "annual SR collection", FALSE)


annual_mean_evi <- annual_sr_collection$
  select("evi")$
  mean()



palette <- c(
  "FFFFFF", "CE7E45", "DF923D", "F1B555", "FCD163", "99B718",
  "74A901", "66A000", "529400", "3E8601", "207401", "056201",
  "004C00", "023B01", "012E01", "011D01", "011301"
)

first_img <- annual_sr_collection$first()
ee_print(first_img)
Map$addLayer(first_img$select("evi"), list(min = 0, max = 1, palette = palette), "evi first", FALSE)
