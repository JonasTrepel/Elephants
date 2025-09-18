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

dt_m <- tidy(m)

#next steps: predict habitat quality using these estimates??? 
dt_grid <- fread("data/processed_data/data_fragments/knp_grid_with_habitat_quality.csv") %>% 
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




p_1 <- dt_grid %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scico::scale_fill_scico(palette = "bamako") +
  theme_void()
p_1


p_1 <- dt_grid %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1) +
  labs(title = "HQ Movement Data", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_1

p_2 <- dt_grid %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_dry_season_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1) +
  labs(title = "HQ Movement Dry Season", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_2


p_3 <- dt_grid %>% 
  ggplot() +
  geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = habitat_quality_counts_norm)) +
  scico::scale_fill_scico(palette = "batlow", direction = 1) +
  labs(title = "HQ Count Data", fill = "HQ") +
  theme_void() +
  theme(legend.position = "right") +
  coord_fixed() 
p_3

corr_4 <- cor.test(dt_grid$habitat_quality_counts_norm, dt_grid$habitat_quality_norm, na.rm = T)

corr_4$estimate

p_4 <- dt_grid %>% 
  ggplot() +
  geom_point(aes(x = habitat_quality_counts_norm, y = habitat_quality_norm), size = 0.1, alpha = 0.35) +
  scico::scale_fill_scico(palette = "bamako", direction = -1) +
  geom_abline(linetype = "dashed", color = "pink") +
  labs(x = "HQ Count Data", y = "HQ Movement Data", title = paste0("cor = ", round(corr_4$estimate, 2))) +
  theme_classic()
p_4

corr_5 <- cor.test(dt_grid$habitat_quality_counts_norm, dt_grid$habitat_quality_dry_season_norm, na.rm = T)

corr_5$estimate

p_5 <- dt_grid %>% 
  ggplot() +
  geom_point(aes(x = habitat_quality_counts_norm, y = habitat_quality_norm), size = 0.1, alpha = 0.35) +
  scico::scale_fill_scico(palette = "bamako", direction = -1) +
  geom_abline(linetype = "dashed", color = "pink") +
  labs(x = "HQ Count Data", y = "HQ Movement Data (Dry Season)", title = paste0("cor = ", round(corr_5$estimate, 2))) +
  theme_classic()
p_5

library(patchwork)
library()

p_empty <- ggplot() +theme_void()

p <- (p_1 | p_empty | p_2 | p_empty | p_3) / (p_4 | p_5) + plot_layout(heights = c(2, 1))
p
ggsave(plot = p, "builds/plots/supplement/knp_habitat_quality_validation.png", dpi = 600, height = 10, width = 9)
