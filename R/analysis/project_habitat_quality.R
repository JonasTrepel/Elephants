#get space use 
library(tidyverse)
library(data.table)
library(terra)
library(exactextractr)
library(sf)
library(tidylog)
library(patchwork)


dt_est_raw <- fread("builds/model_outputs/issf_estimates_12hrs_steps.csv") 


#### cross Validatation ---------------------------------------

# Esimtaes 
dt_est_raw <- fread("builds/model_outputs/issf_estimates_12hrs_steps.csv") 

#load grid
dt_grid_vars <- fread("data/processed_data/data_fragments/grid_habitat_covariates.csv")

#load grid
sf_grid <- st_read("data/spatial_data/grid/empty_grid.gpkg") %>% 
  left_join(dt_grid_vars)

#load homeranges 
sf_hr <- st_read("data/spatial_data/elephants/mcp_home_ranges.gpkg") %>% 
  st_transform(., crs = "ESRI:54009")

#load location points 
sf_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = "ESRI:54009")

dt_cv <- data.frame()
plot_list <- list()
i = 0
for(id in unique(dt_est_raw$individual_id)){
  
  
  dt_est_sub <- dt_est_raw %>% 
    filter(!individual_id %in% id) %>% 
    group_by(term) %>% 
    summarise(mean_estimate = mean(estimate, na.rm = T), 
              std_error = sd(estimate)/sqrt(n()), 
              ci_lb = mean_estimate - 1.96*std_error,
              ci_ub = mean_estimate + 1.96*std_error, 
              p_value = median(p_value))
  
  hr_sub <- sf_hr %>% filter(individual_id == id)
  
  
  loc_sub <- sf_loc %>% filter(individual_id == id)
  loc_dry <- loc_sub %>%
    filter(month %in% c(5,6,7,8,9))
  loc_wet <- loc_sub %>%
    filter(!month %in% c(5,6,7,8,9))
  
  grid_sub <- sf_grid %>% 
    filter(lengths(st_intersects(., hr_sub)) > 0) %>% 
    mutate(n_points_total = lengths(st_intersects(., loc_sub)),
           rel_obs_total = (n_points_total / nrow(loc_sub)) * 100, 
           n_points_dry = lengths(st_intersects(., loc_dry)),
           rel_obs_dry = (n_points_dry / nrow(loc_dry)) * 100,
           n_points_wet = lengths(st_intersects(., loc_wet)),
           rel_obs_wet = (n_points_wet / nrow(loc_wet)) * 100, 
           individual_id = id) #%>% 
    #as.data.table() %>% 
   # mutate(geom = NULL)
  
  sf_hq <- grid_sub %>% 
    #filter(!is.na(pa_name)) %>% 
    #group_by(pa_name) %>% 
    mutate(dist_settlement_scaled = as.numeric(scale(distance_to_settlement_km)), 
           dist_water_scaled = as.numeric(scale(distance_to_water_km)),
           enerscape_scaled = as.numeric(scale(enerscape)),
           evi_scaled = as.numeric(scale(evi_mean)),
           human_mod_scaled = as.numeric(scale(human_modification)),
           slope_scaled = as.numeric(scale(slope))) %>% 
    mutate(habitat_quality = 
             (dt_est_sub[dt_est_sub$term == "distance_to_settlement_km", ]$mean_estimate*dist_settlement_scaled + 
                dt_est_sub[dt_est_sub$term == "distance_to_water_km", ]$mean_estimate*dist_water_scaled +
                dt_est_sub[dt_est_sub$term == "enerscape", ]$mean_estimate*enerscape_scaled +
                dt_est_sub[dt_est_sub$term == "evi_mean", ]$mean_estimate*evi_scaled +
                dt_est_sub[dt_est_sub$term == "human_modification", ]$mean_estimate*human_mod_scaled +
                dt_est_sub[dt_est_sub$term == "slope", ]$mean_estimate*slope_scaled),
           habitat_quality_norm = (habitat_quality - min(habitat_quality, na.rm = TRUE)) / 
             (max(habitat_quality, na.rm = TRUE) - min(habitat_quality, na.rm = TRUE))) #%>% filter(rel_obs_total > 0)
  
  p_hq <- sf_hq %>% 
    ggplot() +
    geom_sf(aes(fill = habitat_quality_norm, color = habitat_quality_norm)) + 
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    theme_void()
  p_hq
  
  p_ro <- sf_hq %>% 
    ggplot() +
    geom_sf(aes(fill = rel_obs_total, color = rel_obs_total)) + 
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    theme_void()
  p_ro
  
  p_rel <- sf_hq %>% 
    ggplot(aes(x = habitat_quality_norm, y = rel_obs_total)) +
    geom_point()+
    geom_smooth(method = "lm")
  p_rel
  
  p_id <- sf_hq %>% 
    ggplot() +
    labs(title = id, 
         subtitle = paste0(unique(loc_sub$park_id))) +
    theme_void()
  p_id
  
  p_all <- (p_hq | p_ro) /
    (p_id | p_rel)  
  print(p_all)

  plot_list[[id]] <- p_all
  
  ct <- cor.test(sf_hq$habitat_quality_norm, sf_hq$rel_obs_total, na.rm = T)
  m <- lm(rel_obs_total ~ habitat_quality_norm, data = sf_hq)
  m_sum <- summary(m)
  
  tidy_m = broom::tidy(m) %>% 
    filter(term != "(Intercept)")
  
  tmp_cor <- data.frame(
    r_sq_adj = m_sum$adj.r.squared, 
    r_sq = m_sum$r.squared,
    r_sq_adj = m_sum$adj.r.squared, 
    lm_estimate = tidy_m$estimate,
    lm_std_error = tidy_m$std.error, 
    lm_p = tidy_m$p.value, 
    lm_stat = tidy_m$statistic, 
    term = tidy_m$term, 
    response = "rel_obs_all", 
    cor = as.numeric(ct$estimate),
    individual_id = id, 
    sex = unique(loc_sub$sex)
  )
  
  dt_cv = rbind(tmp_cor, dt_cv)
  i = i+1
  print(paste0(id, " done (", i, " of ", n_distinct(dt_est_raw$individual_id), ")"))
  
}

pdf("builds/plots/exploratory/habitat_quality_vs_selection.pdf", width = 8, height = 6)
for (id in unique(dt_est_raw$individual_id)) {
  print(plot_list[[id]])
}

dev.off()

fwrite(dt_cv, "builds/model_outputs/habitat_selection_cross_validation.csv")



print(plot_list[["5779"]])

#########################################################
library(tidyverse)
library(data.table)
library(terra)
library(exactextractr)
library(sf)
library(tidylog)

dt_grid <- fread("data/processed_data/data_fragments/grid_with_all_covariates.csv")

##### Get PA ids #######
sf_pas <- read_sf("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  dplyr::select(pa_name = NAME, 
                wdpa_pid = WDPA_PID, 
                iucn_cat = IUCN_CAT, 
                country_code = ISO3) %>% 
  as.data.table() %>% 
  mutate(geom = NULL, 
         wdpa_pid = as.numeric(wdpa_pid))


pa_r <- rast("data/spatial_data/protected_areas/pa_id_raster.tif")

sf_grid_points <- st_as_sf(dt_grid, 
                           coords = c("x_mollweide", "y_mollweide"), 
                           crs = "ESRI:54009") %>% 
  st_buffer(dist = 100)

st_crs(pa_r) == st_crs(sf_grid_points)

extr <- exactextractr::exact_extract(pa_r, 
                                     sf_grid_points, 
                                     append_cols = c("grid_id"),
                                     fun = "mode")

dt_extr <- extr %>%
  rename(wdpa_pid = mode) %>%
  left_join(sf_pas)
unique(extr$mode, na.rm = T)
unique(sf_pas$wdpa_pid)

dt_grid <- dt_grid %>%
  left_join(dt_extr)
unique(dt_grid$pa_name)




dt_est_raw <- fread("builds/model_outputs/issf_estimates_12hrs_steps.csv") 

dt_est <- dt_est_raw %>% 
  group_by(term) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            ci_lb = mean_estimate - 1.96*std_error,
            ci_ub = mean_estimate + 1.96*std_error, 
            p_value = median(p_value))  %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ), 
  sig = ifelse(p_value < 0.05, "significant", "non-significant"), 
  sig_me = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant"))



dt_habitat_quality <- dt_grid %>% 
  filter(!is.na(pa_name)) %>% 
  group_by(pa_name) %>% 
  mutate(dist_settlement_scaled = as.numeric(scale(distance_to_settlement_km)), 
         dist_water_scaled = as.numeric(scale(distance_to_water_km)),
         enerscape_scaled = as.numeric(scale(enerscape)),
         evi_scaled = as.numeric(scale(evi_mean)),
         human_mod_scaled = as.numeric(scale(human_modification)),
         slope_scaled = as.numeric(scale(slope))) %>% 
  mutate(habitat_quality = 
           (dt_est[dt_est$term == "distance_to_settlement_km", ]$mean_estimate*dist_settlement_scaled + 
              dt_est[dt_est$term == "distance_to_water_km", ]$mean_estimate*dist_water_scaled +
              dt_est[dt_est$term == "enerscape", ]$mean_estimate*enerscape_scaled +
              dt_est[dt_est$term == "evi_mean", ]$mean_estimate*evi_scaled +
              dt_est[dt_est$term == "human_modification", ]$mean_estimate*human_mod_scaled +
              dt_est[dt_est$term == "slope", ]$mean_estimate*slope_scaled),
         habitat_quality_norm = (habitat_quality - min(habitat_quality, na.rm = TRUE)) / 
           (max(habitat_quality, na.rm = TRUE) - min(habitat_quality, na.rm = TRUE))) %>% 
  ungroup()

hist(dt_habitat_quality$habitat_quality_norm)
cor.test(dt_habitat_quality$habitat_quality_norm, dt_habitat_quality$mean_rel_obs_all)




sf_hq <- dt_habitat_quality %>% 
  filter(pa_name == "Hwange") %>% 
  st_as_sf(coords = c("x_mollweide",  "y_mollweide"), 
           crs = "ESRI:54009") %>% 
  st_buffer(dist = 500)

mapview(sf_hq)  

p1 <- sf_hq %>% ggplot() +
  geom_sf(aes(fill = habitat_quality_norm, color = habitat_quality_norm)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void()
p1

p2 <- sf_hq %>% ggplot() +
  geom_sf(aes(fill = mean_rel_obs_all, color = mean_rel_obs_all)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void()
p2

p3 <- sf_hq %>% ggplot() +
  geom_sf(aes(fill = distance_to_settlement_km, color = distance_to_settlement_km)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void()
p3

p4 <- sf_hq %>% ggplot() +
  geom_sf(aes(fill = log(enerscape), color = log(enerscape))) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void()
p4

p5 <- sf_hq %>% ggplot() +
  geom_sf(aes(fill = distance_to_water_km, color = distance_to_water_km)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void()
p5

p6 <- sf_hq %>% ggplot() +
  geom_sf(aes(fill = evi_mean, color = evi_mean)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void()
p6

p7 <- sf_hq %>% ggplot() +
  geom_sf(aes(fill = human_modification, color = human_modification)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void()
p7

p8 <- sf_hq %>% ggplot() +
  geom_sf(aes(fill = slope, color = slope)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void()
p8

p_h <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)
ggsave(plot = p_h, "builds/plots/exploratory/hwange_habitat_quality.png", dpi = 600, height = 5, width = 10)
