library(tidyverse)
library(data.table)
library(scico)
#plots 1000m ---------------

dt_grid_1000m_hq_plot <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100)


for (park in unique(dt_grid_1000m_hq_plot$park_id)) {
  
  print(paste0("Start with ", park))
  
  # Filter data for the current park
  park_data <- dt_grid_1000m_hq_plot %>%
    filter(park_id == park)
  
  # Plot all variables in one row
  p_1 <- park_data %>% 
    ggplot() +
    # geom_sf(aes(fill = value_rescaled, color = value_rescaled)) +
    geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
    scico::scale_fill_scico(palette = "bamako") +
    theme_void() +
   # labs(fill = "Rescaled Value", color = "Rescaled Value") +
    labs(title = park, fill = "Habitat\nQuality") +
    theme(legend.position = "right", 
          strip.text = element_text(size = 10, face = "bold"), 
          plot.title = element_text(face = "bold", size = 14)) +
    coord_fixed()
  p_1
  
  print(p_1)
  

  # Save plot
  filename <- paste0("builds/plots/supplement/habitat_quality_maps/1000m_", gsub(" ", "_", tolower(park)), "_hq.png")
  ggsave(plot = p_1, filename = filename, dpi = 600)
}

#100m -------------------
dt_grid_100m_hq_plot <- fread("data/processed_data/clean_data/analysis_ready_grid_100m.csv") %>% 
  mutate(tree_cover_100m_coef = tree_cover_100m_coef*100)



for (park in unique(dt_grid_100m_hq_plot$park_id)) {
  
  print(paste0("Start with ", park))
  
  # Filter data for the current park
  park_data <- dt_grid_100m_hq_plot %>%
    filter(park_id == park)
  
  # Plot all variables in one row
  p_1 <- park_data %>% 
    ggplot() +
    # geom_sf(aes(fill = value_rescaled, color = value_rescaled)) +
    geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
    scico::scale_fill_scico(palette = "bamako") +
    theme_void() +
    # labs(fill = "Rescaled Value", color = "Rescaled Value") +
    labs(title = park, fill = "Habitat\nQuality") +
    theme(legend.position = "right", 
          strip.text = element_text(size = 10, face = "bold"), 
          plot.title = element_text(face = "bold", size = 14)) +
    coord_fixed()
  p_1
  
  print(p_1)
  
  
  # Save plot
  filename <- paste0("builds/plots/supplement/habitat_quality_maps/100m_", gsub(" ", "_", tolower(park)), "_hq.png")
  ggsave(plot = p_1, filename = filename, dpi = 600)
}

