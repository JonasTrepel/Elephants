#get space use 
library(tidyverse)
library(data.table)
library(terra)
library(exactextractr)
library(sf)
library(tidylog)
library(patchwork)


dt_est_12 <- fread("builds/model_outputs/issf_estimates_12hrs_steps.csv") %>% 
  group_by(season, term) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            ci_lb = mean_estimate - 1.96*std_error,
            ci_ub = mean_estimate + 1.96*std_error, 
            p_value = median(p_value)) %>% 
  ungroup()
dt_est_3 <- fread("builds/model_outputs/issf_estimates_3hrs_steps.csv")  %>% 
  group_by(season, term) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            ci_lb = mean_estimate - 1.96*std_error,
            ci_ub = mean_estimate + 1.96*std_error, 
            p_value = median(p_value)) %>% 
  ungroup()
dt_est_1 <- fread("builds/model_outputs/issf_estimates_1hr_steps.csv")  %>% 
  group_by(season, term) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            ci_lb = mean_estimate - 1.96*std_error,
            ci_ub = mean_estimate + 1.96*std_error, 
            p_value = median(p_value)) %>% 
  ungroup()
dt_est_ig <- fread("builds/model_outputs/elephants_individual_grid_estimates_glm_nb.csv")  %>% 
  group_by(season, term) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            ci_lb = mean_estimate - 1.96*std_error,
            ci_ub = mean_estimate + 1.96*std_error, 
            p_value = median(p_value)) %>% 
  ungroup()


#load grid vars
dt_grid_vars <- fread("data/processed_data/data_fragments/pa_grids_with_covariates.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))

#load grid
sf_grid <- st_read("data/spatial_data/grid/empty_grid_pas.gpkg") %>% 
  mutate( 
    grid_id = paste0("grid_", 1:nrow(.))) %>% 
  left_join(dt_grid_vars)

  

  
  sf_grid_hq <- sf_grid %>% 
    group_by(park_id) %>% 
    mutate(dist_settlement_scaled = as.numeric(scale(distance_to_settlement_km)), 
           dist_water_scaled = as.numeric(scale(distance_to_water_km)),
           evi_scaled = as.numeric(scale(evi_mean)),
           human_mod_scaled = as.numeric(scale(human_modification)),
           slope_scaled = as.numeric(scale(slope))) %>% 
    mutate(
      habitat_quality_1hr = (
        dt_est_1[dt_est_1$term == "distance_to_settlement_km" & dt_est_1$season == "whole_year", ]$mean_estimate * dist_settlement_scaled +
          dt_est_1[dt_est_1$term == "distance_to_water_km" & dt_est_1$season == "whole_year", ]$mean_estimate * dist_water_scaled +
          dt_est_1[dt_est_1$term == "evi_mean" & dt_est_1$season == "whole_year", ]$mean_estimate * evi_scaled +
          dt_est_1[dt_est_1$term == "human_modification" & dt_est_1$season == "whole_year", ]$mean_estimate * human_mod_scaled +
          dt_est_1[dt_est_1$term == "slope" & dt_est_1$season == "whole_year", ]$mean_estimate * slope_scaled
      ),
      habitat_quality_1hr_norm = (habitat_quality_1hr - min(habitat_quality_1hr, na.rm = TRUE)) / 
        (max(habitat_quality_1hr, na.rm = TRUE) - min(habitat_quality_1hr, na.rm = TRUE)),
      
      habitat_quality_3hr = (
        dt_est_3[dt_est_3$term == "distance_to_settlement_km" & dt_est_3$season == "whole_year", ]$mean_estimate * dist_settlement_scaled +
          dt_est_3[dt_est_3$term == "distance_to_water_km" & dt_est_3$season == "whole_year", ]$mean_estimate * dist_water_scaled +
          dt_est_3[dt_est_3$term == "evi_mean" & dt_est_3$season == "whole_year", ]$mean_estimate * evi_scaled +
          dt_est_3[dt_est_3$term == "human_modification" & dt_est_3$season == "whole_year", ]$mean_estimate * human_mod_scaled +
          dt_est_3[dt_est_3$term == "slope" & dt_est_3$season == "whole_year", ]$mean_estimate * slope_scaled
      ),
      habitat_quality_3hr_norm = (habitat_quality_3hr - min(habitat_quality_3hr, na.rm = TRUE)) / 
        (max(habitat_quality_3hr, na.rm = TRUE) - min(habitat_quality_3hr, na.rm = TRUE)),
      
      habitat_quality_12hr = (
        dt_est_12[dt_est_12$term == "distance_to_settlement_km" & dt_est_12$season == "whole_year", ]$mean_estimate * dist_settlement_scaled +
          dt_est_12[dt_est_12$term == "distance_to_water_km" & dt_est_12$season == "whole_year", ]$mean_estimate * dist_water_scaled +
          dt_est_12[dt_est_12$term == "evi_mean" & dt_est_12$season == "whole_year", ]$mean_estimate * evi_scaled +
          dt_est_12[dt_est_12$term == "human_modification" & dt_est_12$season == "whole_year", ]$mean_estimate * human_mod_scaled +
          dt_est_12[dt_est_12$term == "slope" & dt_est_12$season == "whole_year", ]$mean_estimate * slope_scaled
      ),
      habitat_quality_12hr_norm = (habitat_quality_12hr - min(habitat_quality_12hr, na.rm = TRUE)) / 
        (max(habitat_quality_12hr, na.rm = TRUE) - min(habitat_quality_12hr, na.rm = TRUE)),
      
      habitat_quality_ig = (
        dt_est_ig[dt_est_ig$term == "distance_to_settlement_km" & dt_est_ig$season == "whole_year", ]$mean_estimate * dist_settlement_scaled +
          dt_est_ig[dt_est_ig$term == "distance_to_water_km" & dt_est_ig$season == "whole_year", ]$mean_estimate * dist_water_scaled +
          dt_est_ig[dt_est_ig$term == "evi_mean" & dt_est_ig$season == "whole_year", ]$mean_estimate * evi_scaled +
          dt_est_ig[dt_est_ig$term == "human_modification" & dt_est_ig$season == "whole_year", ]$mean_estimate * human_mod_scaled +
          dt_est_ig[dt_est_ig$term == "slope" & dt_est_ig$season == "whole_year", ]$mean_estimate * slope_scaled
      ),
      habitat_quality_ig_norm = (habitat_quality_ig - min(habitat_quality_ig, na.rm = TRUE)) / 
        (max(habitat_quality_ig, na.rm = TRUE) - min(habitat_quality_ig, na.rm = TRUE))
    ) %>% 
    ungroup()
  
  
for(park in unique(sf_grid_hq$park_id)){ 
  
  print(paste0("start with ", park))
  
sf_grid_hq_long <-  sf_grid_hq %>% 
  filter(park_id == park) %>% 
  pivot_longer(
    cols = starts_with("habitat_quality_") & ends_with("_norm"),
    names_to = "estimate_set",
    values_to = "habitat_quality"
  ) %>% 
    mutate(estimate_set = recode(estimate_set,
                                 "habitat_quality_1hr_norm" = "HQ 1hr",
                                 "habitat_quality_3hr_norm" = "HQ 3hr",
                                 "habitat_quality_12hr_norm" = "HQ 12hr",
                                 "habitat_quality_ig_norm" = "HQ Individual Grid"
    ))
  
sf_grid_hq_drivers_long <- sf_grid_hq %>% 
  filter(park_id == park) %>% 
  pivot_longer(
    cols = c(dist_settlement_scaled, dist_water_scaled,  
             evi_scaled, human_mod_scaled, slope_scaled),
    names_to = "driver_name",
    values_to = "driver_value"
  ) %>% 
    mutate(driver_name = recode(driver_name,
                                "dist_settlement_scaled" = "Dist. to Settlement",
                                "dist_water_scaled" = "Dist. to Water",
                                "evi_scaled" = "EVI",
                                "human_mod_scaled" = "Human Modification",
                                "slope_scaled" = "Slope"
    ))
  
p1 <- ggplot(sf_grid_hq_long) +
  geom_sf(aes(fill = habitat_quality, color = habitat_quality)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void() +
  facet_wrap(~ estimate_set, ncol = 4) +
  labs(fill = "Habitat Quality", color = "Habitat Quality")
p1



p2 <- ggplot(sf_grid_hq_drivers_long) +
  geom_sf(aes(fill = driver_value, color = driver_value)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  theme_void() +
  facet_wrap(~ driver_name, ncol = 5) +
  labs(fill = "Scaled Value", color = "Scaled Value")
p2  

p_comb <- p1 / p2 +
  plot_annotation(title = park)

filename <- paste0("builds/plots/supplement/",gsub(" ", "_", tolower(park)), "_habitat_quality.png")

ggsave(plot = p_comb, filename = filename, dpi = 600, height = 8, width = 12) 

}
