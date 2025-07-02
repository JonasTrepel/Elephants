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


sf_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = "ESRI:54009") # we want to work in metric crs here (mollweide)


# 2. Make Grid ---------------------------------

sf_grid_raw <- NULL
for(pa in unique(sf_pa$NAME)){

  print(paste0("starting with ", pa))
  

sf_pa_sub <- sf_pa %>% filter(NAME == pa)
  
sf_grid_sub <- st_make_grid(sf_pa_sub, cellsize = 500, square = TRUE) %>% #1km for now, but should be 500m for final analysis
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


sf_grid_raw <- sf_grid_raw %>% 
  mutate(grid_id = paste0("grid_", 1:nrow(.)))

coords_moll <- sf_grid_raw %>% st_centroid() %>% st_coordinates()

coords_lat_lon <- sf_grid_raw %>% st_transform(4326) %>% st_centroid() %>% st_coordinates()


sf_grid_raw$x_mollweide <- coords_moll[,1]
sf_grid_raw$y_mollweide <- coords_moll[,2]

sf_grid_raw$lon <- coords_lat_lon[,1]
sf_grid_raw$lat <- coords_lat_lon[,2]

st_write(sf_grid_raw, "data/spatial_data/grid/empty_grid_pas.gpkg", append = FALSE)
sf_grid_raw <- st_read("data/spatial_data/grid/empty_grid_pas.gpkg")

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
  dry_both = sf_loc_dry,
  wet_both = sf_loc_wet,
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

categories <- c("rel_obs_all", "rel_obs_dry_both", "rel_obs_wet_both", 
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


# 9. Get cluster id ------------

# sf_clust <- st_read("data/spatial_data/protected_areas/pa_clusters.gpkg") %>% 
#   st_transform(crs = "ESRI:54009")
# 
# dt_cluster <- sf_grid_raw %>% 
#   st_intersection(., sf_clust) %>% 
#   as.data.frame() %>% 
#   mutate(x = NULL, geom = NULL, geometry = NULL)

# 10. Combine --------------------

dt_comb <- dt_grid_rel 

sf_grid_comb <- sf_grid_raw %>% 
  left_join(dt_comb)


fwrite(dt_comb, "data/processed_data/data_fragments/pa_grid_rel_occ.csv")
st_write(sf_grid_comb, "data/spatial_data/grid/pa_grid_rel_occ.gpkg", append = FALSE)
