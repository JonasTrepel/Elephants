library(data.table)
library(tidyverse)
library(sf)
library(rnaturalearth)

# Load data -------------------------------
# 
# # Minimum convex polygons home ranges 
# sf_mcp <- st_read("data/spatial_data/elephants/mcp_home_ranges.gpkg") %>% 
#   st_transform(., crs = 4326)
# 
# # Local convex hull home ranges 
# sf_locoh <- st_read("data/spatial_data/elephants/locohs_home_ranges.gpkg") %>% 
#   st_transform(., crs = 4326)

# Location points 
sf_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = 4326)


# PAs with location points 
sf_pas_ld <- st_read("data/spatial_data/protected_areas/pas_intersecting_with_locations_data.gpkg") %>% 
  st_transform(crs = 4326)

# PAs with population counts 
sf_pas_pc <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  st_transform(crs = 4326)

# World 
sf_world <- ne_countries(scale = "medium", returnclass = "sf")

# Africa 
sf_africa <- sf_world %>% filter(region_un == "Africa") %>% 
  filter(!name == "Madagascar") %>% 
  st_transform(., crs = 4326)

### Plots ------------------------------------------
p_pa_pc_loc <- sf_loc %>% 
  sample_n(250000) %>% 
  ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  geom_sf(data = sf_africa, fill = "grey99") +
  geom_sf(size = 0.1, alpha = 0.1) +
 # geom_sf(data = sf_clust, alpha = 0.1, fill = "yellow") + 
  geom_sf(data = sf_pas_pc, alpha = 0.5, fill = "orange") + 
  theme_void()
p_pa_pc_loc

p_pas <- sf_pas_pc %>% 
  ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  geom_sf(data = sf_africa, fill = "grey95") +
  geom_sf(alpha = 0.75, fill = "darkred") + 
  theme_minimal()
p_pas

ggsave(plot = p_pa_pc_loc, "builds/plots/exploratory/location_data_and_pas.png", dpi = 600)
ggsave(plot = p_pas, "builds/plots/exploratory/pas_with_population_counts.png", dpi = 600)

