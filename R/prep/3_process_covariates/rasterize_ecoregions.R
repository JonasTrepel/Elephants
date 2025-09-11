library(terra)
library(exactextractr)
library(sf)
library(tidyverse)
library(mapview)

#https://ecoregions.appspot.com/
sf_eco <- st_read("data/spatial_data/covariates/vector/ecoregions/Ecoregions2017.shp") %>% 
  st_transform(crs = "ESRI:54009")
mapview(sf_eco, zcol = "ECO_NAME")


sf_eco %>% 
  as.data.frame() %>% 
  mutate(geometry = NULL) %>% 
  dplyr::select(ECO_ID, ECO_NAME) %>% 
  arrange(ECO_ID) %>%
  unique()

sf_eco %>% 
  as.data.frame() %>% 
  mutate(geometry = NULL) %>% 
  dplyr::select(BIOME_NUM, BIOME_NAME) %>% 
  arrange(BIOME_NUM) %>%
  unique()



### get raster of pa maps 

temp_r_id <- rast(ext(sf_eco), resolution = 1000, crs = "ESRI:54009")


get_mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


eco_ids_r <- rasterize_polygons(sf_eco,
                       temp_r_id)


eco_ids_r[] <- sf_eco$ECO_ID[eco_ids_r[]]

plot(eco_ids_r)

writeRaster(eco_ids_r, "data/spatial_data/covariates/raster/resolve_eco_regions_1km.tif")


#now the same for biomes 
biome_ids_r <- rasterize_polygons(sf_eco,
                                temp_r_id)

biome_ids_r[] <- sf_eco$BIOME_NUM[biome_ids_r[]]

plot(biome_ids_r)

writeRaster(biome_ids_r, "data/spatial_data/covariates/raster/resolve_biomes_1km.tif")
