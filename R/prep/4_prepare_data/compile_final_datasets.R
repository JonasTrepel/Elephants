library(tidyverse)
library(sf)
library(data.table)
library(broom)
library(ggrepel)
library(performance)
library(sjPlot)
library(tidylog)


#### Population counts 
dt_pc_raw <- fread("data/processed_data/clean_data/all_population_counts.csv") %>% 
  mutate(mean_density_km2 = mean_population_count/area_km2)


dt_pc_trend <- data.frame()
for(park in unique(dt_pc_raw$park_id)){
  
  dt_p <- dt_pc_raw %>% filter(park_id == park) %>% 
    mutate(year_1 = year - min(year), 
           population_count_scaled = as.numeric(scale(population_count)),
           density_km2 = population_count/area_km2)
           
  
  if(nrow(dt_p) < 3){next}
  if(any(is.na(dt_p$area_km2))){next}
  
  #regular trend
  m_lm <- lm(population_count ~ year_1, data = dt_p) 
  
  r2_m_lm <- r2(m_lm)
  r2_m_lm$R2_adjusted
  
  m_tidy_lm <- tidy(m_lm) %>% 
    filter(!grepl("Intercept", term)) %>%
    select(lm_population_trend_p_val = p.value, 
           lm_population_trend_estimate = estimate) %>% 
    mutate(park_id = park,
           population_trend_n = nrow(dt_p), 
           lm_population_trend_r2 =  r2_m_lm$R2_adjusted)
  
  # exponential trend / percentage growth 
  m_glm <- glm(population_count ~ year_1, 
      family = poisson(link = "log"), 
      data = dt_p)
  
  r2_m_glm <- r2(m_glm)

  
  plot_model(m_lm, type = "pred")
  plot_model(m_glm, type = "pred")
  
  
  m_tidy_glm <- tidy(m_glm) %>% 
    filter(!grepl("Intercept", term)) %>%
    select(glm_population_trend_p_val = p.value, 
           glm_population_trend_estimate = estimate) %>% 
    mutate(park_id = park,
           population_trend_n = nrow(dt_p), 
           percent_population_growth = (exp(glm_population_trend_estimate)-1)*100, 
           glm_population_trend_r2 = r2_m_glm$R2_Nagelkerke)
  
  #combine
  m_tidy_comb <- m_tidy_lm %>%
    left_join(m_tidy_glm) 
  
  
  dt_pc_trend <- rbind(m_tidy_comb, dt_pc_trend)
  
}
summary(dt_pc_trend)

plot(dt_pc_trend$lm_population_trend_estimate   , dt_pc_trend$percent_population_growth)
hist(dt_pc_trend$glm_population_trend_r2)

dt_pc <- dt_pc_raw %>% 
  select(park_id, mean_population_count, area_km2, mean_density_km2) %>% 
  unique() %>% 
  left_join(dt_pc_trend)


#### 1000m grid with habitat quality -----------
dt_grid_hq_1000m_raw <- fread("data/processed_data/data_fragments/pa_grid_1000m_with_habitat_quality.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))

dt_grid_trends_1000m <- fread("data/processed_data/data_fragments/pa_grid_1000m_with_trends.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))

names(dt_grid_trends_1000m)

dt_grid_hq_1000m <- dt_grid_hq_1000m_raw %>%
  #select(contains("_norm"), park_id) %>% 
  left_join(dt_pc) %>% 
  mutate(cell_area_km2 = 1) %>% #change to 0.01 for 100x100m grid! 
  group_by(park_id) %>% 
  mutate(total_hq = sum(habitat_quality_norm), 
         rel_hq = habitat_quality_norm/total_hq, 
         rel_pc = mean_population_count*rel_hq, 
         local_density_km2 = rel_pc/cell_area_km2) %>% 
  ungroup() %>% 
  dplyr::select(park_id, grid_id, habitat_quality_norm, local_density_km2, 
               mean_population_count, area_km2, mean_density_km2, 
               lm_population_trend_estimate, lm_population_trend_p_val, 
               population_trend_n, 
               percent_population_growth, glm_population_trend_estimate,
               glm_population_trend_p_val, glm_population_trend_r2, 
               evi_mean) %>% 
  left_join(dt_grid_trends_1000m)

sum(dt_grid_hq_1000m[dt_grid_hq_1000m$park_id == "Kruger National Park", ]$local_density_km2, na.rm = T)
sum(dt_grid_hq_1000m[dt_grid_hq_1000m$park_id == "Kruger National Park", ]$mean_density_km2, na.rm = T)

unique(dt_grid_hq_1000m[dt_grid_hq_1000m$park_id == "Kruger National Park", ]$percent_population_growth)


fwrite(dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  filter(!park_id %in% c("Zambezi", "iSimangaliso Wetland Park")), 
  "data/processed_data/clean_data/analysis_ready_grid_1000m.csv")

### Exploratory plots ----

park_order <- dt_grid_hq_1000m %>%
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>%
  group_by(park_id) %>%
  summarise(mean_density = mean(mean_density_km2, na.rm = TRUE)) %>%
  arrange(mean_density)  %>% 
  pull(park_id)


dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  mutate(park_id = factor(park_id, levels = park_order)) %>%
  ggplot() +
  geom_boxplot(aes(x = park_id, y = tree_cover_1000m_coef, fill = cluster_id)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red") +
  theme(axis.text.x = element_text(angle = 90))

dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  mutate(park_id = factor(park_id, levels = park_order)) %>%
  ggplot() +
  geom_boxplot(aes(x = park_id, y = evi_900m_coef, fill = cluster_id)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red") +
  theme(axis.text.x = element_text(angle = 90))

dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  mutate(park_id = factor(park_id, levels = park_order)) %>%
  ggplot() +
  geom_boxplot(aes(x = park_id, y = canopy_height_900m_coef, fill = cluster_id)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red") +
  theme(axis.text.x = element_text(angle = 90))


dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  mutate(park_id = factor(park_id, levels = park_order)) %>%
  ggplot() +
  geom_boxplot(aes(x = park_id, y = habitat_diversity_1000m_coef, fill = cluster_id)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red") +
  theme(axis.text.x = element_text(angle = 90))

dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  mutate(park_id = factor(park_id, levels = park_order)) %>%
  ggplot() +
  geom_boxplot(aes(x = park_id, y = evi_sd_900m_coef, fill = cluster_id)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red") +
  theme(axis.text.x = element_text(angle = 90))


dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  mutate(park_id = factor(park_id, levels = park_order)) %>%
  ggplot() +
  geom_boxplot(aes(x = park_id, y = canopy_height_sd_900m_coef, fill = cluster_id)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red") +
  theme(axis.text.x = element_text(angle = 90))


dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  mutate(park_id = factor(park_id, levels = park_order)) %>%
  ggplot() +
  geom_point(aes(x = evi_mean, y = evi_900m_coef))
  
cor.test(dt_grid_hq_1000m$evi_900m_coef, dt_grid_hq_1000m$mean_evi_900m)

dt_grid_hq_1000m %>% 
  dplyr::select(mean_density_km2, park_id) %>% 
  unique() %>% 
  arrange(-mean_density_km2) %>% 
  as.data.table()

dt_corr_1000m <- dt_grid_hq_1000m %>% 
  filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
  filter(!park_id %in% c("Zambezi", "iSimangaliso Wetland Park")) %>% #one vary high, the other strange (very unclear where in the park eephabts can go etc)
  select(evi_900m_coef, tree_cover_1000m_coef, canopy_height_900m_coef, 
         evi_sd_900m_coef, habitat_diversity_1000m_coef, canopy_height_sd_900m_coef, 
         
         local_density_km2, mean_density_km2, percent_population_growth, 
         prec_coef, mat_coef, n_deposition, fire_frequency,
         months_extreme_drought, months_severe_drought) %>% 
  filter(complete.cases(.))

corr <- round(cor(dt_corr_1000m), 2)
ggcorrplot::ggcorrplot(corr, hc.order = FALSE, type = "lower",
                       lab = TRUE)


#### 100m grid with habitat quality -----------
dt_grid_hq_100m_raw <- fread("data/processed_data/data_fragments/pa_grid_100m_with_habitat_quality.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))
unique(dt_grid_hq_100m_raw$park_id)

glimpse(dt_grid_hq_100m_raw)

dt_grid_trends_100m <- fread("data/processed_data/data_fragments/pa_grid_100m_with_trends.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))
unique(dt_grid_trends_100m$park_id)
names(dt_grid_trends_100m)

dt_grid_hq_100m <- dt_grid_hq_100m_raw %>%
  #filter(park_id == "Kruger National Park") %>% 
  #select(contains("_norm"), park_id) %>% 
  left_join(dt_pc) %>% 
  mutate(cell_area_ha = 1) %>% 
  group_by(park_id) %>% 
  mutate(total_hq = sum(habitat_quality_norm, na.rm = T), 
         rel_hq = habitat_quality_norm/total_hq, 
         rel_pc = mean_population_count*rel_hq, 
         local_density_ha = rel_pc/cell_area_ha) %>% 
  ungroup() %>% 
  dplyr::select(park_id, grid_id, habitat_quality_norm, local_density_ha, 
                mean_population_count, area_km2, mean_density_km2, 
                lm_population_trend_estimate, lm_population_trend_p_val, 
                population_trend_n, 
                percent_population_growth, glm_population_trend_estimate,
                glm_population_trend_p_val, glm_population_trend_r2, 
                evi_mean) %>% 
  left_join(dt_grid_trends_100m) %>% 
  mutate(local_density_km2 = local_density_ha*100)

summary(dt_grid_hq_100m)

sum(dt_grid_hq_100m[dt_grid_hq_100m$park_id == "Kruger National Park", ]$local_density_ha, na.rm = T)
sum(dt_grid_hq_100m[dt_grid_hq_100m$park_id == "Kruger National Park", ]$local_density_km2, na.rm = T)/100


unique(dt_grid_hq_100m[dt_grid_hq_100m$park_id == "Kruger National Park", ]$mean_population_count)


fwrite(dt_grid_hq_100m %>% 
         filter(cluster_id %in% c("kzn", "limpopo", "luangwa", "chobe")) %>% 
         filter(!park_id %in% c("Zambezi", "iSimangaliso Wetland Park")), 
       "data/processed_data/clean_data/analysis_ready_grid_100m.csv")


#100m grid -------------

# #Deprecated--------------------
# #### points ---------------
# dt_points <- fread("data/processed_data/data_fragments/pa_points_with_trends.csv") %>% 
#   mutate(wdpa_pid = as.character(wdpa_pid))
# 
# sf_points <- read_sf("data/spatial_data/grid/empty_points_pas.gpkg") %>% 
#   left_join(dt_points[, -c("x_mollweide", "y_mollweide", "lon", "lat")])
# 
# hist(sf_points$grass_cover_coef)
# 
# #### Join
# st_crs(sf_points) == st_crs(sf_grid_hq)
# names(sf_points)
# names(sf_grid_hq)
# 
# sf_grid_hq %>%
#   as.data.table() %>% 
#   mutate(geom = NULL) %>% 
#   select(park_id, population_trend_estimate, density_km2_estimate) %>% 
#   unique()
# 
# sf_points_ellies <- st_join(sf_points, sf_grid_hq %>% rename(park_id_grid = park_id))
# 
# #get Ids of points that overlap with multiple parks. Tjose we don't want
# problematic_ids <- sf_points_ellies %>% 
#   as.data.frame() %>% 
#   mutate(geom = NULL, geometry = NULL, x = NULL) %>% 
#   filter(!park_id == park_id_grid) %>% 
#   select(unique_id) %>% pull()
# 
# dt_points_ellies <- sf_points_ellies %>% 
#   as.data.frame() %>% 
#   mutate(geom = NULL, geometry = NULL, x = NULL) %>% 
#   filter(!unique_id %in% problematic_ids)
# 
# dt_points_ellies %>% select(park_id, population_trend_estimate, density_km2_estimate) %>% 
#   unique()
# 
# summary(dt_points_ellies)
# #### Save 
# fwrite(dt_points_ellies, "data/processed_data/clean_data/final_point_data.csv")
# 
# 
# #play 
# 
# dt_points_ellies %>% 
#   ggplot() +
#   geom_boxplot(aes(x = park_id, y = tree_cover_coef))
# 
# 
# dt_points_ellies %>% 
#   ggplot() +
#   geom_point(aes(x = local_density_km2, y = mean_evi_coef, color = park_id), alpha = 0.2) +
#   geom_smooth(aes(x = local_density_km2, y = mean_evi_coef), method = "lm") +
#   theme(legend.position = "none")
# 
# dt_points_ellies %>% 
#   ggplot() +
#   geom_point(aes(x = mean_density_km2, y = mean_evi_coef, color = park_id), alpha = 0.2) +
#   geom_smooth(aes(x = mean_density_km2, y = mean_evi_coef), method = "lm") +
#   theme(legend.position = "none")
# 
# dt_points_ellies %>% 
#   group_by(park_id) %>% 
#   summarize(
#     mean_density_km2 = mean(population_trend_scaled_estimate), 
#     tree_cover_coef = mean(grass_cover_coef, na.rm = T)
#   ) %>% 
#   #filter(mean_density_km2 < 3) %>% 
#   ggplot() +
#   geom_point(aes(x = mean_density_km2, y = tree_cover_coef, color = park_id), alpha = 0.9) +
#   geom_text_repel(aes(x = mean_density_km2, y = tree_cover_coef, label = park_id), 
#                   size = 3, max.overlaps = 50) +
#   geom_smooth(aes(x = mean_density_km2, y = tree_cover_coef), method = "gam") +
#   theme(legend.position = "none")
# 
# 
# dt_points_ellies %>% 
#   ggplot() +
#   geom_point(aes(x = population_trend_estimate, y = mean_evi_coef, color = park_id), alpha = 0.2) +
#   geom_smooth(aes(x = population_trend_estimate, y = mean_evi_coef), method = "lm") +
#   theme(legend.position = "none")
# 
# 
# dt_corr <- dt_points_ellies %>% 
#   select(mean_evi_coef, 
#          tree_cover_coef, shrub_cover_coef, grass_cover_coef, gr_n_cr_cover_coef, 
#          
#          local_density_km2, mean_density_km2, population_trend_estimate) %>% 
#   filter(complete.cases(.))
# 
# corr <- round(cor(dt_corr), 2)
# ggcorrplot::ggcorrplot(corr, hc.order = FALSE, type = "lower",
#            lab = TRUE)
