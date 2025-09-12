library(data.table)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(scico)

# Load data -------------------------------
# 
#filter(cluster_id %in% c("chobe", "limpopo", "kzn", "luangwa")) %>% 


#population counts 

dt_pc <- fread("data/processed_data/clean_data/all_population_counts.csv") %>% 
  group_by(park_id) %>% 
  mutate(n = n()) %>% 
  filter(n() >= 3) %>% 
  ungroup()

# Location points 
sf_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = 4326)

#12hr estimates 

dt_est <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv")

# PAs with location points 
sf_pas_ld <- st_read("data/spatial_data/protected_areas/pas_intersecting_with_locations_data.gpkg") %>% 
  st_transform(crs = 4326)

# PAs with population counts 
sf_pas_pc <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  st_transform(crs = 4326)

sf_clust <- st_read("data/spatial_data/protected_areas/pa_clusters.gpkg") %>% 
  st_transform(crs = 4326) %>% 
  filter(cluster_id %in% c("limpopo", "kzn", "chobe", "luangwa")) %>% 
  mutate(cluster_id = case_when(
    cluster_id == "greater_kruger" ~ "Limpopo", 
    cluster_id == "greater_waterberg" ~ "Limpopo", 
    cluster_id == "limpopo" ~ "Limpopo", 
    cluster_id == "kzn" ~ "KZN", 
    cluster_id == "luangwa" ~ "Luangwa", 
    cluster_id == "chobe" ~ "Chobe", 
    cluster_id == "kafue" ~ "Kafue", 
    cluster_id == "zambezi" ~ "Zambezi"
  )) 

# World 
sf_world <- ne_countries(scale = "medium", returnclass = "sf")

# Africa 
sf_africa <- sf_world %>% filter(region_un == "Africa") %>% 
  filter(!name == "Madagascar") %>% 
  st_transform(., crs = 4326)

### Plots ------------------------------------------
p_pas <- sf_pas_pc %>% 
  filter(WDPA_PID %in% unique(dt_pc$wdpa_pid)) %>% 
  ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  geom_sf(data = sf_africa, fill = "linen") +
  geom_sf(data = sf_clust, aes(fill = cluster_id, color = cluster_id), alpha = 0.25,
          linetype = "dashed", 
          linewidth = 1.1)+
  scale_color_scico_d(palette = "batlow") +
  scale_fill_scico_d(palette = "batlow") +
  geom_sf(alpha = 0.75, fill = "black") + 
  theme_void() +
  theme(legend.position = "none")
p_pas

p_loc <- sf_loc %>% 
  filter(individual_id %in% unique(dt_est$individual_id)) %>%
  sample_n(500000) %>% 
  ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  geom_sf(data = sf_africa, fill = "linen") +
  geom_sf(data = sf_clust, aes(color = cluster_id), alpha = 0.25, size = 1.5,
          fill = "transparent", 
          linetype = "dashed", 
          linewidth = 1.1) +
  scale_color_scico_d(palette = "batlow") +
  geom_sf(size = 0.1, alpha = 0.1, color = "black") +
  theme_void()+
  theme(legend.position = "none")
p_loc

ggsave(plot = p_pas, "builds/plots/exploratory/pas_with_population_counts.png", dpi = 600)
ggsave(plot = p_loc, "builds/plots/exploratory/location_data_12hr_individuals.png", dpi = 600)
