#GET PA grids 

library(sf)
library(data.table)
library(tidyverse)
library(mapview)
library(exactextractr)
library(terra)

# 1. Load Data ---------------------------------
sf_pa <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  st_transform(., crs = "ESRI:54009")


# 2. Make Grid ---------------------------------

sf_grid_raw <- NULL
for(pa in unique(sf_pa$NAME)){
  
  print(paste0("starting with ", pa))
  
  
  sf_pa_sub <- sf_pa %>% filter(NAME == pa)
  
  sf_grid_sub <- st_make_grid(sf_pa_sub,
                              cellsize = 500,
                              square = TRUE, 
                              what = "centers") %>% #1km for now, but should be 500m for final analysis
    st_as_sf() %>% 
    filter(lengths(st_intersects(., sf_pa_sub)) > 0) %>% 
    mutate(park_id = unique(sf_pa_sub$NAME), 
           country_code_iso3 = unique(sf_pa_sub$ISO3),
           designation = unique(sf_pa_sub$DESIG_ENG),
           wdpa_pid = unique(sf_pa_sub$WDPA_PID),
           iucn_cat = unique(sf_pa_sub$IUCN_CAT)
    )
  
  if(is.null(sf_grid_raw)){
    
    sf_grid_raw <- sf_grid_sub
    
  }else{
    
    sf_grid_raw <- rbind(sf_grid_raw, sf_grid_sub)
    
  }
  
  print(paste0(pa, " done"))
  
}


sf_points <- sf_grid_raw %>% 
  mutate(unique_id = paste0("point_", 1:nrow(.)))
st_write(sf_points, "data/spatial_data/grid/empty_points_pas.gpkg", append = FALSE)
