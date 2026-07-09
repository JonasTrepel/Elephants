library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(broom)
library("sdmTMB")
library(future)
library(furrr)
library(groupdata2)
library(GGally)
library(glmmTMB)
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

#load data 
#sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_100m.csv") %>% 
  mutate(tree_cover_100m_coef = tree_cover_100m_coef*100)

setDT(dt)


# get dataframe with comlete and clean data fro mdoeling 

dt_mod <- dt %>% 
 # filter(dw_min_median_mode_fraction >= 50) %>% 
  filter(park_id == "Hluhluwe – iMfolozi Park") %>% 
  dplyr::select(
    #mean values /habitat characteristics 
    mean_tree_cover_100m, mean_canopy_height_90m, 
    
    #starting conditions
    tree_cover_100m_2015_2016, canopy_height_90m_2000,
    
    # environmental predictors
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, months_extreme_drought, mat_coef, prec_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2,# density_trend_estimate, density_trend_estimate,
    
    #Trends - Responses 
    tree_cover_100m_coef, canopy_height_90m_coef, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, 
    
    #identifieres 
    park_id, cluster_id, grid_id
  ) %>% 
  mutate(
    x_moll_km = x_mollweide/1000, 
    y_moll_km = y_mollweide/1000,
  ) 

library(scico)
p_ch <- dt_mod %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_90m_coef)) +
  scale_color_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) + 
  coord_equal() +
  labs(title = "Vegetation Height Change (2000-2022)", fill = "Vegetation\nHeight\nChange") +
  theme_void() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5))
p_ch


p_tc <- dt_mod %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_100m_coef)) +
  scale_color_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95) + 
  coord_equal() +
  labs(title = "Woody Cover Change (2015-2025)", fill = "Woody\nCover\nChange") +
  theme_void() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5))
p_tc

library(patchwork)
p = p_ch + p_tc
ggsave(plot = p, "builds/plots/supplement/hip_veg_change.png", dpi = 900, height = 6, width = 10)
