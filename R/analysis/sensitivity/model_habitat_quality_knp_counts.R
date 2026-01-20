library(data.table)
library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(MuMIn)
library(performance)


dt <- fread("data/processed_data/data_fragments/knp_elephant_counts_habitat_covariates.csv") %>% 
  mutate(distance_to_water_km = as.numeric(scale(distance_to_water_km)), 
         human_modification = as.numeric(scale(human_modification)), 
         slope = as.numeric(scale(slope)), 
         evi_mean = as.numeric(scale(evi_mean)), 
         distance_to_settlement_km = as.numeric(scale(distance_to_settlement_km)), 
         )


m0 <- glmmTMB(total ~ 1 +
               (1 | year), 
             data = dt, 
             family = poisson())


m <- glmmTMB(total ~ slope +
               distance_to_water_km +
               distance_to_settlement_km +
               human_modification +
               evi_mean +
               (1 | year), 
             data = dt, 
             family = poisson())
summary(m)
r.squaredGLMM(m)
check_collinearity(m)

AIC(m0) - AIC(m) #delta AIC roughly 4200

dt_m <- tidy(m)

#next steps: predict habitat quality using these estimates

# 1000m -----
dt_grid_1000m <- fread("data/processed_data/data_fragments/knp_grid_1000m_with_habitat_quality.csv") %>% 
  group_by(park_id) %>% 
  mutate(dist_settlement_scaled = as.numeric(scale(distance_to_settlement_km)), 
         dist_water_scaled = as.numeric(scale(distance_to_water_km)),
         evi_scaled = as.numeric(scale(evi_mean)),
         human_mod_scaled = as.numeric(scale(human_modification)),
         slope_scaled = as.numeric(scale(slope))) %>% 
  mutate(
    habitat_quality_counts = (
      dt_m[dt_m$term == "distance_to_settlement_km", ]$estimate*dist_settlement_scaled +
        dt_m[dt_m$term == "distance_to_water_km", ]$estimate*dist_water_scaled +
        dt_m[dt_m$term == "evi_mean", ]$estimate*evi_scaled +
        dt_m[dt_m$term == "human_modification", ]$estimate*human_mod_scaled +
        dt_m[dt_m$term == "slope", ]$estimate*slope_scaled
    ),
    habitat_quality_counts_norm = scales::rescale(habitat_quality_counts)) %>% 
  ungroup()





p_1_1000m <- dt_grid_1000m %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1, begin = 0.1, end = 0.9) +
  labs(title = "HQ Movement Data", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_1_1000m

p_2_1000m <- dt_grid_1000m %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_dry_season_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1, begin = 0.1, end = 0.9) +
  labs(title = "HQ Movement Dry Season", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_2_1000m


p_3_1000m <- dt_grid_1000m %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_counts_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1, begin = 0.1, end = 0.9) +
  labs(title = "HQ Count Data", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_3_1000m

corr_4_1000m <- cor.test(dt_grid_1000m$habitat_quality_counts_norm, dt_grid_1000m$habitat_quality_norm, na.rm = T)

corr_4_1000m$estimate

p_4_1000m <- dt_grid_1000m %>% 
  ggplot() +
  geom_point(aes(x = habitat_quality_counts_norm, y = habitat_quality_norm), size = 0.1, alpha = 0.35) +
  scico::scale_fill_scico(palette = "bamako", direction = -1) +
  geom_abline(linetype = "dashed", color = "pink3") +
  labs(x = "HQ Count Data", y = "HQ Movement Data", title = paste0("cor = ", round(corr_4_1000m$estimate, 2))) +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_4_1000m

corr_5_1000m <- cor.test(dt_grid_1000m$habitat_quality_counts_norm, dt_grid_1000m$habitat_quality_dry_season_norm, na.rm = T)

corr_5_1000m$estimate

p_5_1000m <- dt_grid_1000m %>% 
  ggplot() +
  geom_point(aes(x = habitat_quality_counts_norm, y = habitat_quality_norm), size = 0.1, alpha = 0.35) +
  scico::scale_fill_scico(palette = "bamako", direction = -1) +
  geom_abline(linetype = "dashed", color = "pink3") +
  labs(x = "HQ Count Data", y = "HQ Movement Data (Dry Season)", title = paste0("cor = ", round(corr_5_1000m$estimate, 2))) +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_5_1000m

library(patchwork)
library()

p_empty <- ggplot() +theme_void()

p_1000m <- (p_1_1000m | p_empty | p_2_1000m | p_empty | p_3_1000m) / (p_4_1000m | p_5_1000m) + plot_layout(heights = c(2, 1))
p_1000m
ggsave(plot = p_1000m, "builds/plots/supplement/knp_habitat_quality_validation_1000m.png", dpi = 600, height = 10, width = 9)



# 100m -----
dt_grid_100m <- fread("data/processed_data/data_fragments/knp_grid_100m_with_habitat_quality.csv") %>% 
  group_by(park_id) %>% 
  mutate(dist_settlement_scaled = as.numeric(scale(distance_to_settlement_km)), 
         dist_water_scaled = as.numeric(scale(distance_to_water_km)),
         evi_scaled = as.numeric(scale(evi_mean)),
         human_mod_scaled = as.numeric(scale(human_modification)),
         slope_scaled = as.numeric(scale(slope))) %>% 
  mutate(
    habitat_quality_counts = (
      dt_m[dt_m$term == "distance_to_settlement_km", ]$estimate*dist_settlement_scaled +
        dt_m[dt_m$term == "distance_to_water_km", ]$estimate*dist_water_scaled +
        dt_m[dt_m$term == "evi_mean", ]$estimate*evi_scaled +
        dt_m[dt_m$term == "human_modification", ]$estimate*human_mod_scaled +
        dt_m[dt_m$term == "slope", ]$estimate*slope_scaled
    ),
    habitat_quality_counts_norm = scales::rescale(habitat_quality_counts)) %>% 
  ungroup()




p_1_100m <- dt_grid_100m %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scico::scale_fill_scico(palette = "bamako") +
  theme_void()
p_1_100m


p_1_100m <- dt_grid_100m %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1) +
  labs(title = "HQ Movement Data", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_1_100m

p_2_100m <- dt_grid_100m %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_dry_season_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1) +
  labs(title = "HQ Movement Dry Season", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_2_100m


p_3_100m <- dt_grid_100m %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_counts_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1) +
  labs(title = "HQ Count Data", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_3_100m

corr_4_100m <- cor.test(dt_grid_100m$habitat_quality_counts_norm, dt_grid_100m$habitat_quality_norm, na.rm = T)

corr_4_100m$estimate

p_4_100m <- dt_grid_100m %>% 
  ggplot() +
  geom_point(aes(x = habitat_quality_counts_norm, y = habitat_quality_norm), size = 0.1, alpha = 0.35) +
  scico::scale_fill_scico(palette = "bamako", direction = -1) +
  geom_abline(linetype = "dashed", color = "pink") +
  labs(x = "HQ Count Data", y = "HQ Movement Data", title = paste0("cor = ", round(corr_4_100m$estimate, 2))) +
  theme_classic()
p_4_100m

corr_5_100m <- cor.test(dt_grid_100m$habitat_quality_counts_norm, dt_grid_100m$habitat_quality_dry_season_norm, na.rm = T)

corr_5_100m$estimate

p_5_100m <- dt_grid_100m %>% 
  ggplot() +
  geom_point(aes(x = habitat_quality_counts_norm, y = habitat_quality_norm), size = 0.1, alpha = 0.35) +
  scico::scale_fill_scico(palette = "bamako", direction = -1) +
  geom_abline(linetype = "dashed", color = "pink") +
  labs(x = "HQ Count Data", y = "HQ Movement Data (Dry Season)", title = paste0("cor = ", round(corr_5_100m$estimate, 2))) +
  theme_classic()
p_5_100m

library(patchwork)
library()

p_empty <- ggplot() +theme_void()

p_100m <- (p_1_100m | p_empty | p_2_100m | p_empty | p_3_100m) / (p_4_100m | p_5_100m) + plot_layout(heights = c(2, 1))
p_100m
ggsave(plot = p_100m, "builds/plots/supplement/knp_habitat_quality_validation_100m.png", dpi = 600, height = 10, width = 9)

