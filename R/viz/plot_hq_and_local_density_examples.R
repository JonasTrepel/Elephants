library(data.table)
library(tidyverse)
library(scico)

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv")
unique(dt$park_id)

p_hwange <- dt %>% filter(park_id %in% c("Hwange")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scale_color_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  labs(title = "Hwange", fill = "Habitat\nQuality") +
  theme_void() + coord_fixed() + 
  theme(legend.position = "right")
p_hwange

p_chobe <- dt %>% filter(park_id %in% c("Chobe")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scale_color_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  theme_void() + coord_fixed() + 
  labs(title = "Chobe", fill = "Habitat\nQuality") +
  theme(legend.position = "right")
p_chobe


p_hip <- dt %>% filter(park_id %in% c("Hluhluwe – iMfolozi Park")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scale_color_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  labs(title = "Hluhluwe – iMfolozi Park", fill = "Habitat\nQuality") +
  theme_void() + coord_fixed() + 
  theme(legend.position = "right") 
p_hip

p_gon <- dt %>% filter(park_id %in% c("Gonarezhou")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scale_color_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  labs(title = "Gonarezhou", fill = "Habitat\nQuality") +
  theme_void() + coord_fixed()
p_gon

p_lapalala <- dt %>% filter(park_id %in% c("Lapalala Nature Reserve")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scale_color_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  labs(title = "Lapalala Nature Reserve", fill = "Habitat\nQuality") +
  theme_void() + coord_fixed() + 
  theme(legend.position = "right")
p_lapalala 

p_sl <- dt %>% filter(park_id %in% c("South Luangwa")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scale_color_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bamako", begin = 0.05, end = 0.95) +
  labs(title = "South Luangwa", fill = "Habitat\nQuality") +
  theme_void() + coord_fixed() + 
  theme(legend.position = "right")
p_sl

### density 
p_dens_hwange <- dt %>% filter(park_id %in% c("Hwange")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = local_density_km2)) +
  scale_color_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(title = "Hwange", fill = "Elephant\nDensity\n(Ind./km²)") +
  theme_void() + coord_fixed() + 
  theme(legend.position = "right")
p_dens_hwange

p_dens_chobe <- dt %>% filter(park_id %in% c("Chobe")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = local_density_km2)) +
  scale_color_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  theme_void() + coord_fixed() + 
  labs(title = "Chobe", fill = "Elephant\nDensity\n(Ind./km²)") +
  theme(legend.position = "right")
p_dens_chobe


p_dens_hip <- dt %>% filter(park_id %in% c("Hluhluwe – iMfolozi Park")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = local_density_km2)) +
  scale_color_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(title = "Hluhluwe – iMfolozi Park", fill = "Elephant\nDensity\n(Ind./km²)") +
  theme_void() + coord_fixed() + 
  theme(legend.position = "right") 
p_dens_hip

p_dens_gon <- dt %>% filter(park_id %in% c("Gonarezhou")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = local_density_km2)) +
  scale_color_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(title = "Gonarezhou", fill = "Elephant\nDensity\n(Ind./km²)") +
  theme_void() + coord_fixed()
p_dens_gon

p_dens_lapalala <- dt %>% filter(park_id %in% c("Lapalala Nature Reserve")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = local_density_km2)) +
  scale_color_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(title = "Lapalala Nature Reserve", fill = "Elephant\nDensity\n(Ind./km²)") +
  theme_void() + coord_fixed() + 
  theme(legend.position = "right")
p_dens_lapalala 

p_dens_sl <- dt %>% filter(park_id %in% c("South Luangwa")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = local_density_km2)) +
  scale_color_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(title = "South Luangwa", fill = "Elephant\nDensity\n(Ind./km²)") +
  theme_void() + coord_fixed() + 
  theme(legend.position = "right")
p_dens_sl

library(patchwork)
p_hq <- ((p_hwange | p_chobe | p_gon) / (p_lapalala | p_hip | p_sl))
p_hq

p_dens_hq <- ((p_dens_hwange | p_dens_chobe | p_dens_gon) / (p_dens_lapalala | p_dens_hip | p_dens_sl))
p_dens_hq

p_empty <- ggplot() + theme_void()

p <- ((p_hwange | p_chobe | p_gon) / (p_lapalala | p_hip | p_sl) / 
  p_empty / 
  (p_dens_hwange | p_dens_chobe | p_dens_gon) / (p_dens_lapalala | p_dens_hip | p_dens_sl)) + 
  plot_layout(heights = c(1, 1, 0.3, 1, 1))
p

ggsave(plot = p, "builds/plots/supplement/hq_examples_for_si.png", dpi = 600, height = 12, width = 10)
