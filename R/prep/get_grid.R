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


# 2. Make Grid ---------------------------------

sf_grid_raw <- st_make_grid(sf_loc, cellsize = 5000, square = FALSE) %>% 
  st_as_sf() %>% 
  filter(lengths(st_intersects(., sf_hr)) > 0) %>% 
  mutate(grid_id = paste0("grid_", 1:nrow(.)))

st_write(sf_grid_raw, "data/spatial_data/grid/empty_grid.gpkg")


#3. Split relocation points --------------------

# All
#sf_loc

sf_loc %>%
  as.data.frame() %>% 
  mutate(geometry = NULL) %>% 
  select(sex, individual_id) %>% 
  unique() %>% 
  dplyr::select(sex) %>% 
  pull() %>% 
  table()
# F   M   U 
# 171  64  34 

# Dry Season
sf_loc_dry <- sf_loc %>%
  filter(month %in% c(5,6,7,8,9)) 

# Wet Season 
sf_loc_wet <- sf_loc %>%
  filter(!month %in% c(5,6,7,8,9))

# Males 
sf_loc_males <- sf_loc %>%
  filter(sex == "M")

# Females 
sf_loc_females <- sf_loc %>%
  filter(sex == "F")

# Dry Season Males 
sf_loc_dry_male <- sf_loc %>%
  filter(month %in% c(5,6,7,8,9) & sex == "M") 

# Dry Season Females  
sf_loc_dry_female <- sf_loc %>%
  filter(month %in% c(5,6,7,8,9) & sex == "F") 

# Wet Season Males 
sf_loc_wet_male <- sf_loc %>%
  filter(!month %in% c(5,6,7,8,9) & sex == "M") 

# Wet Season Females  
sf_loc_wet_female <- sf_loc %>%
  filter(!month %in% c(5,6,7,8,9) & sex == "F") 


# 4. Get relative occurrence for each individual elephant ------------------


subset_list <- list(
  all = sf_loc,
  dry = sf_loc_dry,
  wet = sf_loc_wet,
  males = sf_loc_males,
  females = sf_loc_females,
  dry_male = sf_loc_dry_male,
  dry_female = sf_loc_dry_female,
  wet_male = sf_loc_wet_male,
  wet_female = sf_loc_wet_female
)

# Loop through individuals

id = "ele11"
subset_name = "all"
sf_loc %>% filter(individual_id == id)

sf_grid_occ_loop <- sf_grid_raw
i = 0
for(id in unique(sf_loc$individual_id)){
  print(paste("Processing", id))
  
  for (subset_name in names(subset_list)) {
    # Filter the subset for the specific individual
    ind_points <- subset_list[[subset_name]] %>% filter(individual_id == id)
    
    # Skip if no data
    if (nrow(ind_points) == 0) next
    
    # Column names
   # colname_n <- paste0("n_points_", subset_name, "_", id)
    colname_rel <- paste0("rel_obs_", subset_name, "_", id)
    
    # Compute intersection and relative percentage
    sf_grid_occ_loop <- sf_grid_occ_loop %>%
      mutate(
        colname_n = lengths(st_intersects(., ind_points)),
        colname_rel = (colname_n / nrow(ind_points)) * 100) %>% 
      dplyr::select(-colname_n)
    
   # setnames(sf_grid_occ_loop, "colname_n", colname_n)
    setnames(sf_grid_occ_loop, "colname_rel", colname_rel)
    
  }
  
  i = i+1
  
  print(paste0(id, " done (", i, "/", n_distinct(sf_loc$individual_id), ")"))
}

# 5. Sum up relative ocurrence per cell  ----------------

categories <- c("rel_obs_all", "rel_obs_dry", "rel_obs_wet", 
                "rel_obs_males", "rel_obs_females", 
                "rel_obs_dry_male", "rel_obs_dry_female", 
                "rel_obs_wet_male", "rel_obs_wet_female")

dt_grid_rel <- sf_grid_occ_loop %>%
  as.data.frame() %>%
  mutate(x = NULL) %>% 
  select(grid_id)

dt_grid_occ <- sf_grid_occ_loop %>%
  as.data.frame() %>%
  mutate(x = NULL)


for (cat in categories) {
  
  rel_cols <- grep(cat, names(sf_grid_occ_loop), value = TRUE)
  
  if (length(rel_cols) == 0){next}
  
  dt_summary <- dt_grid_occ %>%
    as.data.frame() %>%
    rowwise() %>%
    mutate(
      sum_rel = sum(c_across(all_of(rel_cols)), na.rm = TRUE),
      n_indiv = sum(c_across(all_of(rel_cols)) > 0, na.rm = TRUE),
      mean_rel = ifelse(n_indiv > 0, sum_rel / n_indiv, 0)
    ) %>%
    ungroup() %>%
    dplyr::select(sum_rel, n_indiv, mean_rel)
  
  names(dt_summary) <- paste0(c("sum_", "n_indiv_", "mean_"), cat)
  
  dt_grid_rel <- cbind(dt_grid_rel, dt_summary)
  
  print(paste0(cat, " done"))
}

summary(dt_grid_rel)

# 6. Get road density --------------------------------------------------

#### Load and split roads 

sf_roads <- st_read("data/spatial_data/covariates/vector/africa_roads/GRIP4_region3.shp") %>% 
  st_transform(., crs = "ESRI:54009")

sf_roads_in_grids <- sf_roads %>% 
  filter(lengths(st_intersects(., sf_grid_raw)) > 0)

sf_major_roads <- sf_roads_in_grids %>% 
  filter(GP_RTP %in% c(1, 2, 3)) #highways, primary or secondary

sf_minor_roads <- sf_roads_in_grids %>% 
  filter(GP_RTP %in% c(4, 5)) #tertiary or local 


#### get road lengths

# all roads 
dt_road_length <- st_intersection(sf_roads_in_grids, sf_grid_raw) %>% 
  mutate(length_m = st_length(geometry)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL) %>% 
  group_by(grid_id) %>% 
  summarize(road_length_km = as.numeric(sum(length_m)/1000))

# major roads 
dt_major_road_length <- st_intersection(sf_major_roads, sf_grid_raw) %>% 
  mutate(length_m = st_length(geometry)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL) %>% 
  group_by(grid_id) %>% 
  summarize(major_road_length_km = as.numeric(sum(length_m)/1000))

# minor roads 
dt_minor_road_length <- st_intersection(sf_minor_roads, sf_grid_raw) %>% 
  mutate(length_m = st_length(geometry)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL) %>% 
  group_by(grid_id) %>% 
  summarize(minor_road_length_km = as.numeric(sum(length_m)/1000))

#### get road density
dt_road_density <-  sf_grid_raw %>% 
  mutate(road_density = lengths(st_intersects(., sf_roads_in_grids)), 
         major_road_density = lengths(st_intersects(., sf_major_roads)), 
         minor_road_density = lengths(st_intersects(., sf_minor_roads))) %>% 
  as.data.table() %>% 
  mutate(x = NULL)


dt_roads <- dt_road_density %>% 
  left_join(dt_road_length) %>% 
  left_join(dt_major_road_length) %>% 
  left_join(dt_minor_road_length)

# 7. Get distance to road --------------------------------

sf_grid_cent <- st_centroid(sf_grid_raw) #centroids 

# get index for the road
sf_grid_cent$nearest_road_index <- st_nearest_feature(sf_grid_cent, sf_roads)
sf_grid_cent$nearest_major_road_index <- st_nearest_feature(sf_grid_cent, sf_major_roads)
sf_grid_cent$nearest_minor_road_index <- st_nearest_feature(sf_grid_cent, sf_minor_roads)

# Extract nearest road geometries
nearest_road <- sf_roads[sf_grid_cent$nearest_road_index, ]
nearest_major_road <- sf_major_roads[sf_grid_cent$nearest_major_road_index, ]
nearest_minor_road <- sf_minor_roads[sf_grid_cent$nearest_minor_road_index, ]

mapview(nearest_major_road)

# get distances
sf_grid_cent$distance_to_road_km <- as.numeric(st_distance(sf_grid_cent, nearest_road, by_element = TRUE)/1000)
sf_grid_cent$distance_to_major_road_km <- as.numeric(st_distance(sf_grid_cent, nearest_major_road, by_element = TRUE)/1000)
sf_grid_cent$distance_to_minor_road_km <- as.numeric(st_distance(sf_grid_cent, nearest_minor_road, by_element = TRUE)/1000)

dt_road_distance <- sf_grid_cent %>% 
  as.data.table() %>% 
  mutate(x = NULL) %>% 
  dplyr::select(distance_to_road_km, distance_to_major_road_km, distance_to_minor_road_km, grid_id)

# 8. Combine --------------------

dt_comb <- dt_grid_rel %>% 
  left_join(dt_grid_rel) %>% 
  left_join(dt_road_density) %>% 
  left_join(dt_road_distance)
  
sf_grid_comb <- sf_grid_raw %>% 
  left_join(dt_comb)


fwrite(dt_comb, "data/clean_data/data_fragments/relative_occurance_and_roads.csv")
st_write(sf_grid_comb, "data/spatial_data/grid/grid_relative_occurance_and_roads.csv")
