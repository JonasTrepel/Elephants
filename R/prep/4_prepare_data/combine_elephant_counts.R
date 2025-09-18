## Combine elephant data 

library(data.table)
library(tidyverse)
library(sf)

ele_files <- list.files("data/spatial_data/elephants/knp_spatial_counts/", 
                        pattern = ".shp", full.names = T) 

ele_files <- ele_files[!grepl("xml", ele_files)]

dt_files <- data.frame(file_path = ele_files) %>%
  mutate(year = gsub("data/spatial_data/elephants/knp_spatial_counts//MegaHerbivore_census_", "", file_path),
         year = gsub("data/spatial_data/elephants/knp_spatial_counts//MegaHerbivore_ccensus_", "", year),
         year = gsub("data/spatial_data/elephants/knp_spatial_counts//Rhino_census_", "", year),
         year = gsub(".shp", "", year)) %>% 
  filter(year != "2010", year != "2013") %>% 
  mutate(year = ifelse(year == "2010_2011", 2010, year), 
         year = as.numeric(year))

for(i in 1:nrow(dt_files)){
  
  shp <- st_read(dt_files[i, ]$file_path) %>% 
    st_transform(crs = 4326)
  
  names(shp) <- tolower(names(shp))
  
  shp$year <- unique(dt_files[i, ]$year)
  
  shp$photo__ <- NULL
  shp$adult_f <- NULL
  shp$adult_m <- NULL
  shp$sub_ad_f <- NULL
  shp$sub_ad_m <- NULL

  if(!is.null(shp$date)){
    shp$date <- as_date(shp$date)
  }
  
  if(!is.null(shp$date)){
    shp$date <- as_date(shp$date)
  }
  
  if(!is.null(shp$calves)){
    shp$calves <- as.numeric(shp$calves)
  }
  
  if(!is.null(shp$estimate)){
    shp$estimate <- as.numeric(shp$estimate)
  }

  if(i == 1){
    sf_ele_raw <- shp
  }else{
    sf_ele_raw <- bind_rows(sf_ele_raw, shp)
    
  }

}

glimpse(sf_ele_raw)

sf_ele <- sf_ele_raw %>% 
  rename(lon = point_x, 
         lat = point_y) %>% 
  mutate(lon = ifelse(is.na(lon), x_coord, lon), 
         lat = ifelse(is.na(lat), y_coord, lat), 
         lon = ifelse(is.na(lon), longitude, lon), 
         lat = ifelse(is.na(lat), latitude, lat), 
         ) %>% 
  dplyr::select(year, species, total, calves, herd_no, sex, comments, lon, lat, geometry) %>% 
  filter(!total == 0) %>% 
  st_make_valid()

sf_ele$lon <- st_coordinates(sf_ele)[,1]
sf_ele$lat <- st_coordinates(sf_ele)[,2]


sf_ele %>% 
 # filter(lon != 0, lat != 0) %>% 
  ggplot() +
  geom_point(aes(x = lon, y = lat)) +
  #geom_sf(aes(color = total), size = 0.1) +
  facet_wrap(~as.factor(year))

fwrite(sf_ele %>% 
         as.data.frame() %>% 
         mutate(geometry = NULL), "data/spatial_data/elephants/elephant_count_points.csv")

st_write(sf_ele, "data/spatial_data/elephants/elephant_count_points.gpkg", append = FALSE)

file
