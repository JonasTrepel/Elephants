library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)
library(sf)

#  
#rgee_env_dir <- c("C:\\Users\\au713983\\.conda\\envs\\rgee_env")
#reticulate::use_python(rgee_env_dir, required=T)
ee_clean_user_credentials()
ee$Authenticate(auth_mode='notebook')
ee$Initialize(project = "jonas-trepel")
drive_auth(email = "jonas.trepel@gmail.com")
ee$String('Hello from the Earth Engine servers!')$getInfo()


source("R/functions/monitor_gee_task.R")

sf_pa <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  st_transform(crs = 4326)

sf_pa_sub <- sf_pa %>% filter(grepl("Kaingo", NAME))

sf_bbox <- st_bbox(sf_pa_sub %>% st_buffer(dist = 100))

roi <- ee$Geometry$Rectangle(
  coords = c(sf_bbox), # xmin, ymin, xmax, ymax
  proj = "EPSG:4326",
  geodesic = FALSE
)

#### code adjusted from Sagang et al 2022
## https://code.earthengine.google.com/af058b015b4dda2e425497a731ab6b6c

Map$centerObject(roi, 11)
Map$addLayer(roi)

# ################################################################
# ### FUNCTIONS ###
# ################################################################
get_nbr_collection <- function(start_year, end_year) {
  
  # Rename L8 OLI bands to common names
  rename_oli <- function(img) {
    img$select(
      c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7", "QA_PIXEL"),
      c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2", "QA_PIXEL")
    )
  }
  
  # Convert Collection-2 scaled SR to surface reflectance
  #https://www.usgs.gov/landsat-missions/landsat-collection-2-surface-reflectance
 scale_to_refl <- function(img) {
    img$select(c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2"))$
      multiply(0.0000275)$
      add(-0.2)
  }
  
  # Mask clouds, cloud shadows and dilated clouds using QA_PIXEL
  mask_clouds <- function(img) {
    qa <- img$select("QA_PIXEL")
    cloud <- bitwShiftL(1, 3)  # 8
    cloud_shadow  <- bitwShiftL(1, 4)  # 16
    dilated_cloud <- bitwShiftL(1, 1)  # 2
    mask <- qa$bitwiseAnd(cloud)$eq(0)$
      And(qa$bitwiseAnd(cloud_shadow)$eq(0))$
      And(qa$bitwiseAnd(dilated_cloud)$eq(0))
    img$updateMask(mask)
  }
  
  # Calculate NBR binary mask: 1 = fire scar where ((nir - swir)/(nir + swir)) < 0.1
  calc_nbr <- function(img) {
    img$expression(
      "((nir - swir) / (nir + swir)) < 0.01",
      list(
        nir  = img$select("NIR"),
        swir = img$select("SWIR2")
      )
    )$rename("NBR")
  }
  
  # Prepare L8 images: rename, mask, scale to refl, compute NBR
  prep_oli <- function(img) {
    original <- img
    img <- rename_oli(img)
    img <- mask_clouds(img)
    refl <- scale_to_refl(img)
    img <- refl$addBands(img$select("QA_PIXEL"))
    img <- calc_nbr(img)
    ee$Image(img$copyProperties(original, original$propertyNames()))
  }
  
  ## APPLICATION ###

  # Landsat 8 Collection 2 Tier 1 Level-2
  oli_col <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")
  
  # Define a collection filter
  col_filter <- ee$Filter$And(
    ee$Filter$bounds(roi),
    ee$Filter$calendarRange(start_year, end_year, "Year"),
    ee$Filter$lt("CLOUD_COVER", 50),
    ee$Filter$lt("GEOMETRIC_RMSE_MODEL", 10),
    ee$Filter$eq("IMAGE_QUALITY_OLI", 9)
  )
  
  # Filter collection and prepare
  oli_col <- oli_col$filter(col_filter)$map(prep_oli)
  
  return(oli_col)
}

# ################################################################
# ### Build L8-only NBR collection ###
# ################################################################
# L8 launched in April 2013; adjust start_year if you need the full archive
start_year <- 2014
end_year   <- 2019

col <- get_nbr_collection(start_year, end_year)

# ################################################################
# ### Step 2: Mapping fire frequency ###
# ################################################################
# ################################################################
# ### Mapping fire frequency between 2014 and 2018 ###
# ################################################################
start_14_18 <- "2014-01-01"
end_14_18   <- "2025-12-31"

# Get L8 image collection for the target years
col_14_18 <- ee$ImageCollection(col)$
  filterDate(start_14_18, end_14_18)

print(col_14_18)

# Rename collection of interest
col_2 <- col_14_18

# Create a list of years
years <- ee$List$sequence(2014, 2018)

# Compute the NBR index and set the pixel value to 1 if the NBR > 0.1 (fire scar)
# and 0 if the NBR < 0.1 (for no fire scar) using the "calc_nbr" function.
# NBR threshold of 0.1 was chosen based on a visual interpretation of the NBR
# value of fire scars from Landsat images.
# Check the methodological section of the manuscript for more details.
# The result is an image collection with all the images available for the dry
# season for each year.
# Count the number of fire scars (1) for each pixel on a yearly basis;
# The result is an ImageCollection with one image for each year and the number
# of times fire scar was detected for a pixel within the respective year.
by_year <- ee$ImageCollection$fromImages(
  years$map(ee_utils_pyfunc(function(y) {
    col_2$
      select("NBR")$
      filter(ee$Filter$calendarRange(y, y, "year"))$
      reduce(ee$Reducer$sum())$
      toFloat()$
      set("year", y)
  }))
)

# Filter images with no bands
by_year_2 <- ee$ImageCollection(by_year)$map(function(image) {
  image$set("count", image$bandNames()$length())
})$
  filter(ee$Filter$eq("count", 1))

# Apply a threshold to the number of fire scars detected to validate or reject
# the hypothesis of fire occurrence. A year is validated with fire occurrence if
# at least two fire scars were detected within that respective year.
fire_scaling <- function(image) {
  image$gt(1)
}

collection_scale_14_18 <- by_year_2$map(fire_scaling)

# Generate fire frequency as the ratio between the numbers of years
# with a fire scar detection and the total number of years sampled
bai_freq_14_18 <- collection_scale_14_18$sum()
bai_freq_14_18 <- bai_freq_14_18$clip(roi)

# Display the fire frequency
Map$addLayer(
  bai_freq_14_18,
  list(
    min = 0,
    max = 10,
    palette = c("black", "darkgreen", "green", "lightgreen", "orange", "yellow", "red")
  ),
  "fire_freq_2014_2018"
)

