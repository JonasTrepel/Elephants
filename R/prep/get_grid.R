library(sf)
library(data.table)
library(tidyverse)
library(mapview)
library(exactextractr)
library(terra)

sf_hr <- st_read("data/spatial_data/elephants/mcp_home_ranges.gpkg") %>% 
  st_transform(., crs = "ESRI:54009")


sf_loc <- fread("data/clean_data/all_location_data.csv") %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = "ESRI:54009") # we want to work in metric crs here (mollweide)

sf_loc %>% 
  sample_n(10000) %>% 
  ggplot() +
  geom_sf(size = 0.1)


grid <- st_make_grid(sf_loc, cellsize = 5000, square = FALSE) %>% 
  st_as_sf()

grid_fin <- grid %>% 
  filter(lengths(st_intersects(., sf_hr)) > 0) %>% 
  mutate(grid_id = paste0("grid_", 1:nrow(.)))

mapview(grid_fin)

st_write(grid_fin, "data/spatial_data/empty_grid.gpkg")


# road density 

roads <- st_read("data/spatial_data/covariates/vector/africa_roads/GRIP4_region3.shp") %>% 
  st_transform(., crs = "ESRI:54009")

roads_in_grids <- roads %>% 
  filter(lengths(st_intersects(., grid_fin)) > 0)

mapview(roads_in_grids)

road_length_grid <- st_intersection(roads_in_grids, grid_fin) %>% 
  mutate(length_m = st_length(geometry)) %>% 
  as.data.frame() %>% 
  mutate(geometry = NULL) %>% 
  group_by(grid_id) %>% 
  summarize(road_length_km = as.numeric(sum(length_m)/1000))

grid_roads <-  grid_fin %>% 
  mutate(road_density = lengths(st_intersects(., roads))) %>% 
  left_join(road_length_grid)


mapview(grid_roads, zcol = "road_length_km") 


###### test getting distance 

r_water <- rast("data/spatial_data/covariates/raster/esa_wc_water_2021_10m_1_or_NA.tif")
plot(r_water)

#Kruger
test_extent <- ext(30.8, 32.0, -25.5, -22.0)  # xmin, xmax, ymin, ymax

r_water_sub <- crop(r_water, test_extent)

target_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #Mollweide metric whatever 

r_water_moll <- project(r_water_sub, 
                     y = target_crs,
                     method = "mode", 
                     threads = 4)
plot(r_water_moll)

r_dist_water <- distance(r_water_moll,
                         gdal = c("BIGTIFF=YES"))/1000 #get distance in km

plot(r_dist_water)




