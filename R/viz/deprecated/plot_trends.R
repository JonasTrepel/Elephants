library(tidyverse)
library(data.table)
library(sf)
library(patchwork)
library(scico)
library(tidyverse)


dt_grid_hq <- fread("data/processed_data/data_fragments/pa_grid_with_habitat_quality.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))
dt_grid_trends <- fread("data/processed_data/data_fragments/pa_grid_with_trends.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))
sf_grid <- st_read("data/spatial_data/grid/empty_grid_pas.gpkg") %>% 
  left_join(dt_grid_hq[, -c("x_mollweide", "y_mollweide", "lon", "lat")]) %>% 
  left_join(dt_grid_trends[, -c("x_mollweide", "y_mollweide", "lon", "lat")])

for(park in unique(sf_grid$park_id)){
  
  sf_sub <- sf_grid %>% filter(park_id == park)
  
  p_grass <- sf_sub %>% 
    ggplot() +
    geom_sf(aes(fill = grass_cover_coef, color = grass_cover_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "Grass Cover Trend", fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_grass
  
  p_gr_n_cr <- sf_sub %>% 
    ggplot() +
    geom_sf(aes(fill = gr_n_cr_cover_coef, color = gr_n_cr_cover_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "Grass n Crop Cover Trend", fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_gr_n_cr
  
  p_shrub <- sf_sub %>% 
    ggplot() +
    geom_sf(aes(fill = shrub_cover_coef, color = shrub_cover_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "Shrub Cover Trend", fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_shrub
  
  p_tree <- sf_sub %>% 
    ggplot() +
    geom_sf(aes(fill = tree_cover_coef, color = tree_cover_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "Tree Cover Trend", fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_tree
  
  # p_bare <- sf_sub %>% 
  #   ggplot() +
  #   geom_sf(aes(fill = bare_cover_coef, color = bare_cover_coef)) +
  #   scale_color_scico(palette = "bam", midpoint = 0) +
  #   scale_fill_scico(palette = "bam", midpoint = 0) +
  #   labs(title = "Bare Cover Trend", fill = "", color = "") +
  #   theme_void() +
  #   theme(legend.position = "bottom")
  # p_bare
  
  p_evi <- sf_sub %>% 
    ggplot() +
    geom_sf(aes(fill = mean_evi_coef, color = mean_evi_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "EVI Trend", fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_evi

  p_h_div_100 <- sf_sub %>%
    ggplot() +
    geom_sf(aes(fill = habitat_diversity_100m_coef, color = habitat_diversity_100m_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "Hab. Div. 100m Trend", fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_h_div_100

  p_h_div_1000 <- sf_sub %>%
    ggplot() +
    geom_sf(aes(fill = habitat_diversity_1000m_coef, color = habitat_diversity_1000m_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "Hab. Div. 1000m Trend", fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_h_div_1000
  
  
  p_mat <- sf_sub %>%
    ggplot() +
    geom_sf(aes(fill = mat_coef, color = mat_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "MAT Trend",  fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_mat
  
  p_prec <- sf_sub %>%
    ggplot() +
    geom_sf(aes(fill = prec_coef, color = prec_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "Prec Trend",  fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_prec
  
  p_burned_area <- sf_sub %>%
    ggplot() +
    geom_sf(aes(fill = burned_area_coef, color = burned_area_coef)) +
    scale_color_scico(palette = "bam", midpoint = 0) +
    scale_fill_scico(palette = "bam", midpoint = 0) +
    labs(title = "Burned Area Trend",  fill = "", color = "") +
    theme_void() +
    theme(legend.position = "bottom")
  p_burned_area
  
  
  p <- (p_tree | p_shrub | p_grass | p_gr_n_cr | p_evi) /
    (p_h_div_1000 | p_h_div_100 | p_mat | p_prec | p_burned_area)
 # print(p)
  filename <- paste0("builds/plots/supplement/trend_maps/", gsub(" ", "_", tolower(park)), "_trends.png")
  ggsave(plot = p, filename = filename, dpi = 600, height = 8, width = 12)
  
  print(paste0(park, " done"))
  
}