library(sf)
library(data.table)
library(tidyverse)
library(mapview)
library(exactextractr)
library(terra)

# 1. Load Data ---------------------------------
sf_hr <- st_read("data/spatial_data/elephants/mcp_home_ranges.gpkg") %>% 
  st_transform(., crs = "ESRI:54009")


sf_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = "ESRI:54009") # we want to work in metric crs here (mollweide)

sf_loc %>% 
  sample_n(10000) %>% 
  ggplot() +
  geom_sf(size = 0.1)

###LOOOOOP 
sf_grid_all <- NULL
for(id in unique(sf_loc$individual_id)){
  
  sf_loc_sub <- sf_loc %>% filter(individual_id == id)
  sf_hr_sub <- sf_hr %>% filter(individual_id == id)

# 2. Make Grid ---------------------------------

sf_grid_raw <- st_make_grid(sf_loc_sub, cellsize = 500, square = TRUE) %>% #1km for now, but should be 500m for final analysis
  st_as_sf() %>% 
  filter(lengths(st_intersects(., sf_hr_sub)) > 0) %>% 
  mutate(grid_id = paste0("grid_",id, "_", 1:nrow(.)))

#st_write(sf_grid_raw, "data/spatial_data/grid/empty_grid.gpkg")
#sf_grid_raw <- st_read("data/spatial_data/grid/empty_grid.gpkg")

#3. Split relocation points in seasons --------------------

# All
#sf_loc

sf_loc_sub %>%
  as.data.frame() %>% 
  mutate(geometry = NULL) %>% 
  select(sex, individual_id) %>% 
  unique() %>% 
  dplyr::select(sex) %>% 
  pull() %>% 
  table()

# Dry Season
sf_loc_dry <- sf_loc_sub %>%
  filter(month %in% c(5,6,7,8,9)) 

# Wet Season 
sf_loc_wet <- sf_loc_sub %>%
  filter(!month %in% c(5,6,7,8,9))

# Get relative occurrence

sf_grid_rel <- sf_grid_raw %>%
  mutate(
    n_points = lengths(st_intersects(., sf_loc_sub)),
    rel_occ = (n_points / nrow(sf_loc_sub)) * 100, 
    n_points_dry = lengths(st_intersects(., sf_loc_dry)),
    rel_occ_dry = (n_points_dry / nrow(sf_loc_dry)) * 100,
    n_points_wet = lengths(st_intersects(., sf_loc_wet)),
    rel_occ_wet = (n_points_wet / nrow(sf_loc_wet)) * 100) %>% 
  dplyr::select(-n_points, -n_points_dry, -n_points_wet)

plot(sf_grid_rel$rel_occ_dry, sf_grid_rel$rel_occ_wet)
mapview(sf_grid_rel, zcol = "rel_occ_dry")
mapview(sf_grid_rel, zcol = "rel_occ_wet")
mapview(sf_grid_rel, zcol = "rel_occ")


# 8. Get Coordinates -------------------------

coords_moll <- sf_grid_raw %>% st_centroid() %>% st_coordinates()

coords_lat_lon <- sf_grid_raw %>% st_transform(4326) %>% st_centroid() %>% st_coordinates()


sf_grid_raw$x_mollweide <- coords_moll[,1]
sf_grid_raw$y_mollweide <- coords_moll[,2]

sf_grid_raw$lon <- coords_lat_lon[,1]
sf_grid_raw$lat <- coords_lat_lon[,2]


dt_coords <- sf_grid_raw %>% 
  as.data.frame() %>% 
  mutate(x = NULL) %>% 
  dplyr::select(grid_id, x_mollweide, y_mollweide, lon, lat)

# 10. Combine --------------------

dt_id_meta <- sf_loc_sub %>% 
  as.data.frame() %>% 
  mutate(geometry = NULL) %>% 
  select(individual_id, sex, source, start_date, start_year, end_date, end_year, median_interval_mins,
         park_id, wdpa_pid, hr_mcp_area_km2, hr_diameter_km, hr_locoh_area_km2) %>% 
  unique()

sf_grid_w_coords <- sf_grid_rel %>% 
  left_join(dt_coords) %>% 
  mutate(individual_id = id, 
         n_total = nrow(sf_loc_sub), 
         n_dry = nrow(sf_loc_dry), 
         n_wet = nrow(sf_loc_wet)) %>% 
  left_join(dt_id_meta)


if(is.null(sf_grid_all)){
  sf_grid_all <- sf_grid_w_coords
}else{
sf_grid_all <- rbind(sf_grid_w_coords, sf_grid_all)
}

print(paste0(id, " done"))
}

st_write(sf_grid_all, "data/spatial_data/grid/individual_grids_relative_occurance.gpkg")

