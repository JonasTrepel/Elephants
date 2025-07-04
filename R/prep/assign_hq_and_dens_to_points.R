library(tidyverse)
library(sf)
library(data.table)
library(broom)
library(ggrepel)



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
  m <- lm(population_count ~ year_1, data = dt_p) 
  
  m_tidy <- tidy(m) %>% 
    filter(!grepl("Intercept", term)) %>%
    select(population_trend_p_val = p.value, 
           population_trend_std_error = std.error, 
           population_trend_estimate = estimate) %>% 
    mutate(park_id = park,
           population_trend_n = nrow(dt_p))
  
  #scaled trend 
  m_scaled <- lm(population_count_scaled ~ year_1, data = dt_p) 
  
  m_tidy_scaled <- tidy(m_scaled) %>% 
    filter(!grepl("Intercept", term)) %>%
    select(population_trend_scaled_p_val = p.value, 
           population_trend_scaled_std_error = std.error, 
           population_trend_scaled_estimate = estimate) %>% 
    mutate(park_id = park,
           population_trend_scaled_n = nrow(dt_p))
  
  #density trend 
  m_dens <- lm(density_km2 ~ year_1, data = dt_p) 
  
  m_tidy_dens <- tidy(m_dens) %>% 
    filter(!grepl("Intercept", term)) %>%
    select(density_km2_p_val = p.value, 
           density_km2_std_error = std.error, 
           density_km2_estimate = estimate) %>% 
    mutate(park_id = park,
           density_km2_trend_n = nrow(dt_p))
  
  #combine
  m_tidy_comb <- m_tidy %>%
    left_join(m_tidy_scaled) %>% 
    left_join(m_tidy_dens)
  
  dt_pc_trend <- rbind(m_tidy_comb, dt_pc_trend)
  
}
summary(dt_pc_trend)

dt_pc <- dt_pc_raw %>% 
  select(park_id, mean_population_count, area_km2, mean_density_km2) %>% 
  unique() %>% 
  left_join(dt_pc_trend)


#### grid with habitat quality 
dt_grid_hq <- fread("data/processed_data/data_fragments/pa_grid_with_habitat_quality.csv")
sf_grid <- st_read("data/spatial_data/grid/empty_grid_pas.gpkg") %>% 
  left_join(dt_grid_hq[, -c("park_id", "country_code_iso3", "designation", "wdpa_pid", "iucn_cat",
                              "x_mollweide", "y_mollweide", "lon", "lat")])

sf_grid_hq <- sf_grid %>%
  select(contains("_norm"), park_id) %>% 
  left_join(dt_pc) %>% 
  mutate(cell_area_km2 = as.numeric(st_area(.) / 1000000)) %>%
  group_by(park_id) %>% 
  mutate(total_hq = sum(habitat_quality_12hr_norm), 
         rel_hq = habitat_quality_12hr_norm/total_hq, 
         rel_pc = mean_population_count*rel_hq, 
         local_density_km2 = rel_pc/cell_area_km2) %>% 
  ungroup() %>% 
  dplyr::select(park_id, habitat_quality_12hr_norm, local_density_km2, 
               mean_population_count, area_km2, mean_density_km2, 
               population_trend_estimate, population_trend_p_val, 
               population_trend_n, 
               population_trend_scaled_p_val, population_trend_scaled_std_error,
               population_trend_scaled_estimate, population_trend_scaled_n,
               density_km2_p_val, density_km2_std_error,density_km2_estimate)

sum(sf_grid_hq[sf_grid_hq$park_id == "Kruger National Park", ]$local_density_km2, na.rm = T)
sum(sf_grid_hq[sf_grid_hq$park_id == "Kruger National Park", ]$mean_density_km2, na.rm = T)

dt_grid_trends <- fread("data/processed_data/data_fragments/pa_grid_with_trends.csv")
#### get grids all inclusive

dt_grid_all <- dt_grid_hq %>% 
  left_join(dt_grid_trends)


#### points 
dt_points <- fread("data/processed_data/data_fragments/pa_points_with_trends.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))

sf_points <- read_sf("data/spatial_data/grid/empty_points_pas.gpkg") %>% 
  left_join(dt_points[, -c(
    "country_code_iso3", "designation", "wdpa_pid", "iucn_cat",
    "x_mollweide", "y_mollweide", "lon", "lat"
  )])

hist(sf_points$grass_cover_coef)

#### Join
st_crs(sf_points) == st_crs(sf_grid_hq)
names(sf_points)
names(sf_grid_hq)
sf_points_ellies <- st_join(sf_points, sf_grid_hq %>% dplyr::select(-park_id))

dt_points_ellies <- sf_points_ellies %>% 
  as.data.frame() %>% 
  mutate(geom = NULL, geometry = NULL, x = NULL)


summary(dt_points_ellies)
#### Save 
fwrite(dt_points_ellies, "data/processed_data/clean_data/final_point_data.csv")


#play 

dt_points_ellies %>% 
  ggplot() +
  geom_boxplot(aes(x = park_id, y = tree_cover_coef))


dt_points_ellies %>% 
  ggplot() +
  geom_point(aes(x = local_density_km2, y = mean_evi_coef, color = park_id), alpha = 0.2) +
  geom_smooth(aes(x = local_density_km2, y = mean_evi_coef), method = "lm") +
  theme(legend.position = "none")

dt_points_ellies %>% 
  ggplot() +
  geom_point(aes(x = mean_density_km2, y = mean_evi_coef, color = park_id), alpha = 0.2) +
  geom_smooth(aes(x = mean_density_km2, y = mean_evi_coef), method = "lm") +
  theme(legend.position = "none")

dt_points_ellies %>% 
  group_by(park_id) %>% 
  summarize(
    mean_density_km2 = mean(population_trend_scaled_estimate), 
    tree_cover_coef = mean(grass_cover_coef, na.rm = T)
  ) %>% 
  #filter(mean_density_km2 < 3) %>% 
  ggplot() +
  geom_point(aes(x = mean_density_km2, y = tree_cover_coef, color = park_id), alpha = 0.9) +
  geom_text_repel(aes(x = mean_density_km2, y = tree_cover_coef, label = park_id), 
                  size = 3, max.overlaps = 50) +
  geom_smooth(aes(x = mean_density_km2, y = tree_cover_coef), method = "gam") +
  theme(legend.position = "none")


dt_points_ellies %>% 
  ggplot() +
  geom_point(aes(x = population_trend_estimate, y = mean_evi_coef, color = park_id), alpha = 0.2) +
  geom_smooth(aes(x = population_trend_estimate, y = mean_evi_coef), method = "lm") +
  theme(legend.position = "none")


dt_corr <- dt_points_ellies %>% 
  select(mean_evi_coef, 
         tree_cover_coef, shrub_cover_coef, grass_cover_coef, gr_n_cr_cover_coef, 
         
         local_density_km2, mean_density_km2, population_trend_estimate) %>% 
  filter(complete.cases(.))

corr <- round(cor(dt_corr), 2)
ggcorrplot::ggcorrplot(corr, hc.order = FALSE, type = "lower",
           lab = TRUE)
