#get space use 
library(tidyverse)
library(data.table)
library(terra)
library(exactextractr)
library(sf)
library(tidylog)
library(patchwork)
library(ggcorrplot)


dt_est_12 <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv") %>% 
  group_by(season, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median(ci_lb),
            median_ci_ub = median(ci_ub),
            p_value = median(p_value)) %>% 
  ungroup()
dt_est_3 <- fread("builds/model_outputs/issf_estimates_3hr_steps.csv")  %>% 
  group_by(season, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median(ci_lb),
            median_ci_ub = median(ci_ub),
            p_value = median(p_value)) %>% 
  ungroup()
dt_est_1 <- fread("builds/model_outputs/issf_estimates_1hr_steps.csv")  %>% 
  group_by(season, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median(ci_lb),
            median_ci_ub = median(ci_ub),
            p_value = median(p_value)) %>% 
  ungroup()

dt_est_ig <- fread("builds/model_outputs/elephants_individual_grid_estimates_glm_nb.csv")  %>% 
  group_by(season, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median(ci_lb),
            median_ci_ub = median(ci_ub),
            p_value = median(p_value)) %>% 
  ungroup()

dt_est_12_sex <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv") %>% 
  group_by(sex, season, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median(ci_lb),
            median_ci_ub = median(ci_ub),
            p_value = median(p_value)) %>% 
  ungroup()


#load grid vars
dt_grid_vars <- fread("data/processed_data/data_fragments/pa_grids_with_covariates.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))

#load grid
sf_grid <- st_read("data/spatial_data/grid/empty_grid_pas.gpkg") %>% 
  left_join(dt_grid_vars[, -c("park_id", "country_code_iso3", "designation", "wdpa_pid", "iucn_cat",
                               "x_mollweide", "y_mollweide", "lon", "lat")])

  
sf_grid_hq <- sf_grid %>% 
    group_by(park_id) %>% 
    mutate(dist_settlement_scaled = as.numeric(scale(distance_to_settlement_km)), 
           dist_water_scaled = as.numeric(scale(distance_to_water_km)),
           evi_scaled = as.numeric(scale(evi_mean)),
           human_mod_scaled = as.numeric(scale(human_modification)),
           slope_scaled = as.numeric(scale(slope))) %>% 
    mutate(
      habitat_quality_1hr = (
        dt_est_1[dt_est_1$term == "distance_to_settlement_km" & dt_est_1$season == "whole_year", ]$median_estimate * dist_settlement_scaled +
          dt_est_1[dt_est_1$term == "distance_to_water_km" & dt_est_1$season == "whole_year", ]$median_estimate * dist_water_scaled +
          dt_est_1[dt_est_1$term == "evi_mean" & dt_est_1$season == "whole_year", ]$median_estimate * evi_scaled +
          dt_est_1[dt_est_1$term == "human_modification" & dt_est_1$season == "whole_year", ]$median_estimate * human_mod_scaled +
          dt_est_1[dt_est_1$term == "slope" & dt_est_1$season == "whole_year", ]$median_estimate * slope_scaled
      ),
      habitat_quality_1hr_norm = (habitat_quality_1hr - min(habitat_quality_1hr, na.rm = TRUE)) / 
        (max(habitat_quality_1hr, na.rm = TRUE) - min(habitat_quality_1hr, na.rm = TRUE)),
      
      habitat_quality_3hr = (
        dt_est_3[dt_est_3$term == "distance_to_settlement_km" & dt_est_3$season == "whole_year", ]$median_estimate * dist_settlement_scaled +
          dt_est_3[dt_est_3$term == "distance_to_water_km" & dt_est_3$season == "whole_year", ]$median_estimate * dist_water_scaled +
          dt_est_3[dt_est_3$term == "evi_mean" & dt_est_3$season == "whole_year", ]$median_estimate * evi_scaled +
          dt_est_3[dt_est_3$term == "human_modification" & dt_est_3$season == "whole_year", ]$median_estimate * human_mod_scaled +
          dt_est_3[dt_est_3$term == "slope" & dt_est_3$season == "whole_year", ]$median_estimate * slope_scaled
      ),
      habitat_quality_3hr_norm = (habitat_quality_3hr - min(habitat_quality_3hr, na.rm = TRUE)) / 
        (max(habitat_quality_3hr, na.rm = TRUE) - min(habitat_quality_3hr, na.rm = TRUE)),
      
      habitat_quality_12hr = (
        dt_est_12[dt_est_12$term == "distance_to_settlement_km" & dt_est_12$season == "whole_year", ]$median_estimate * dist_settlement_scaled +
          dt_est_12[dt_est_12$term == "distance_to_water_km" & dt_est_12$season == "whole_year", ]$median_estimate * dist_water_scaled +
          dt_est_12[dt_est_12$term == "evi_mean" & dt_est_12$season == "whole_year", ]$median_estimate * evi_scaled +
          dt_est_12[dt_est_12$term == "human_modification" & dt_est_12$season == "whole_year", ]$median_estimate * human_mod_scaled +
          dt_est_12[dt_est_12$term == "slope" & dt_est_12$season == "whole_year", ]$median_estimate * slope_scaled
      ),
      habitat_quality_12hr_norm = (habitat_quality_12hr - min(habitat_quality_12hr, na.rm = TRUE)) / 
        (max(habitat_quality_12hr, na.rm = TRUE) - min(habitat_quality_12hr, na.rm = TRUE)),
      
      habitat_quality_ig = (
        dt_est_ig[dt_est_ig$term == "distance_to_settlement_km" & dt_est_ig$season == "whole_year", ]$median_estimate * dist_settlement_scaled +
          dt_est_ig[dt_est_ig$term == "distance_to_water_km" & dt_est_ig$season == "whole_year", ]$median_estimate * dist_water_scaled +
          dt_est_ig[dt_est_ig$term == "evi_mean" & dt_est_ig$season == "whole_year", ]$median_estimate * evi_scaled +
          dt_est_ig[dt_est_ig$term == "human_modification" & dt_est_ig$season == "whole_year", ]$median_estimate * human_mod_scaled +
          dt_est_ig[dt_est_ig$term == "slope" & dt_est_ig$season == "whole_year", ]$median_estimate * slope_scaled
      ),
      habitat_quality_ig_norm = (habitat_quality_ig - min(habitat_quality_ig, na.rm = TRUE)) / 
        (max(habitat_quality_ig, na.rm = TRUE) - min(habitat_quality_ig, na.rm = TRUE)),
      
      ### Season 
      
      habitat_quality_12hr_dry = (
        dt_est_12[dt_est_12$term == "distance_to_settlement_km" & dt_est_12$season == "dry_season", ]$median_estimate * dist_settlement_scaled +
          dt_est_12[dt_est_12$term == "distance_to_water_km" & dt_est_12$season == "dry_season", ]$median_estimate * dist_water_scaled +
          dt_est_12[dt_est_12$term == "evi_mean" & dt_est_12$season == "dry_season", ]$median_estimate * evi_scaled +
          dt_est_12[dt_est_12$term == "human_modification" & dt_est_12$season == "dry_season", ]$median_estimate * human_mod_scaled +
          dt_est_12[dt_est_12$term == "slope" & dt_est_12$season == "dry_season", ]$median_estimate * slope_scaled
      ),
      habitat_quality_12hr_dry_norm = (habitat_quality_12hr_dry - min(habitat_quality_12hr_dry, na.rm = TRUE)) / 
        (max(habitat_quality_12hr_dry, na.rm = TRUE) - min(habitat_quality_12hr_dry, na.rm = TRUE)),
      
      habitat_quality_12hr_wet = (
        dt_est_12[dt_est_12$term == "distance_to_settlement_km" & dt_est_12$season == "wet_season", ]$median_estimate * dist_settlement_scaled +
          dt_est_12[dt_est_12$term == "distance_to_water_km" & dt_est_12$season == "wet_season", ]$median_estimate * dist_water_scaled +
          dt_est_12[dt_est_12$term == "evi_mean" & dt_est_12$season == "wet_season", ]$median_estimate * evi_scaled +
          dt_est_12[dt_est_12$term == "human_modification" & dt_est_12$season == "wet_season", ]$median_estimate * human_mod_scaled +
          dt_est_12[dt_est_12$term == "slope" & dt_est_12$season == "wet_season", ]$median_estimate * slope_scaled
      ),
      habitat_quality_12hr_wet_norm = (habitat_quality_12hr_wet - min(habitat_quality_12hr_wet, na.rm = TRUE)) / 
        (max(habitat_quality_12hr_wet, na.rm = TRUE) - min(habitat_quality_12hr_wet, na.rm = TRUE)),
      
    ### Sex 
    habitat_quality_12hr_male = (
      dt_est_12_sex[dt_est_12_sex$term == "distance_to_settlement_km" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "M", ]$median_estimate * dist_settlement_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "distance_to_water_km" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "M", ]$median_estimate * dist_water_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "evi_mean" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "M", ]$median_estimate * evi_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "human_modification" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "M", ]$median_estimate * human_mod_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "slope" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "M", ]$median_estimate * slope_scaled
    ),
    habitat_quality_12hr_male_norm = (habitat_quality_12hr_male - min(habitat_quality_12hr_male, na.rm = TRUE)) / 
      (max(habitat_quality_12hr_male, na.rm = TRUE) - min(habitat_quality_12hr_male, na.rm = TRUE)),
    
    habitat_quality_12hr_female = (
      dt_est_12_sex[dt_est_12_sex$term == "distance_to_settlement_km" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "F", ]$median_estimate * dist_settlement_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "distance_to_water_km" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "F", ]$median_estimate * dist_water_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "evi_mean" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "F", ]$median_estimate * evi_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "human_modification" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "F", ]$median_estimate * human_mod_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "slope" & dt_est_12_sex$season == "whole_year" & dt_est_12_sex$sex == "F", ]$median_estimate * slope_scaled
    ),
    habitat_quality_12hr_female_norm = (habitat_quality_12hr_female - min(habitat_quality_12hr_female, na.rm = TRUE)) / 
      (max(habitat_quality_12hr_female, na.rm = TRUE) - min(habitat_quality_12hr_female, na.rm = TRUE)),
    
    ### Sex x Season
    habitat_quality_12hr_male_dry = (
      dt_est_12_sex[dt_est_12_sex$term == "distance_to_settlement_km" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "M", ]$median_estimate * dist_settlement_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "distance_to_water_km" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "M", ]$median_estimate * dist_water_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "evi_mean" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "M", ]$median_estimate * evi_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "human_modification" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "M", ]$median_estimate * human_mod_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "slope" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "M", ]$median_estimate * slope_scaled
    ),
    habitat_quality_12hr_male_dry_norm = (habitat_quality_12hr_male_dry - min(habitat_quality_12hr_male_dry, na.rm = TRUE)) / 
      (max(habitat_quality_12hr_male_dry, na.rm = TRUE) - min(habitat_quality_12hr_male_dry, na.rm = TRUE)),
    
    habitat_quality_12hr_female_dry = (
      dt_est_12_sex[dt_est_12_sex$term == "distance_to_settlement_km" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "F", ]$median_estimate * dist_settlement_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "distance_to_water_km" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "F", ]$median_estimate * dist_water_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "evi_mean" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "F", ]$median_estimate * evi_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "human_modification" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "F", ]$median_estimate * human_mod_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "slope" & dt_est_12_sex$season == "dry_season" & dt_est_12_sex$sex == "F", ]$median_estimate * slope_scaled
    ),
    habitat_quality_12hr_female_dry_norm = (habitat_quality_12hr_female_dry - min(habitat_quality_12hr_female_dry, na.rm = TRUE)) / 
      (max(habitat_quality_12hr_female_dry, na.rm = TRUE) - min(habitat_quality_12hr_female_dry, na.rm = TRUE)),
    
    habitat_quality_12hr_male_wet = (
      dt_est_12_sex[dt_est_12_sex$term == "distance_to_settlement_km" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "M", ]$median_estimate * dist_settlement_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "distance_to_water_km" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "M", ]$median_estimate * dist_water_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "evi_mean" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "M", ]$median_estimate * evi_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "human_modification" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "M", ]$median_estimate * human_mod_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "slope" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "M", ]$median_estimate * slope_scaled
    ),
    habitat_quality_12hr_male_wet_norm = (habitat_quality_12hr_male_wet - min(habitat_quality_12hr_male_wet, na.rm = TRUE)) / 
      (max(habitat_quality_12hr_male_wet, na.rm = TRUE) - min(habitat_quality_12hr_male_wet, na.rm = TRUE)),
    
    habitat_quality_12hr_female_wet = (
      dt_est_12_sex[dt_est_12_sex$term == "distance_to_settlement_km" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "F", ]$median_estimate * dist_settlement_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "distance_to_water_km" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "F", ]$median_estimate * dist_water_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "evi_mean" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "F", ]$median_estimate * evi_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "human_modification" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "F", ]$median_estimate * human_mod_scaled +
        dt_est_12_sex[dt_est_12_sex$term == "slope" & dt_est_12_sex$season == "wet_season" & dt_est_12_sex$sex == "F", ]$median_estimate * slope_scaled
    ),
    habitat_quality_12hr_female_wet_norm = (habitat_quality_12hr_female_wet - min(habitat_quality_12hr_female_wet, na.rm = TRUE)) / 
      (max(habitat_quality_12hr_female_wet, na.rm = TRUE) - min(habitat_quality_12hr_female_wet, na.rm = TRUE))
      
    ) %>% 
    ungroup()
  
  
dt_grid_hq <- sf_grid_hq %>% 
  as.data.table() %>% 
  mutate(geom = NULL, x = NULL, geometry = NULL) 

fwrite(dt_grid_hq, "data/processed_data/data_fragments/pa_grid_with_habitat_quality.csv")

dt_corr <- dt_grid_hq %>% 
  dplyr::select(habitat_quality_12hr_norm, #habitat_quality_1hr_norm, habitat_quality_3hr_norm, habitat_quality_ig_norm, 
                habitat_quality_12hr_dry_norm, habitat_quality_12hr_wet_norm, 
                habitat_quality_12hr_male_norm, habitat_quality_12hr_female_norm, 
                habitat_quality_12hr_male_dry_norm, habitat_quality_12hr_female_dry_norm,
                habitat_quality_12hr_male_wet_norm, habitat_quality_12hr_female_wet_norm) %>% 
  filter(complete.cases(.))

corr <- round(cor(dt_corr), 2)
head(corr[, 1:6])
(p_corr <- ggcorrplot(corr, hc.order = F, type = "lower",
           lab = TRUE))
ggsave(plot = p_corr, "builds/plots/supplement/habitat_quality_correlations.png", dpi = 600, height = 10, width = 10)
  
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

filename <- paste0("builds/plots/supplement/habitat_quality_maps/",gsub(" ", "_", tolower(park)), "_habitat_quality.png")

ggsave(plot = p_comb, filename = filename, dpi = 600, height = 8, width = 12) 

}

## other approach 

for (park in unique(sf_grid_hq$park_id)) {
  
  print(paste0("Start with ", park))
  
  # Filter data for the current park
  park_data <- sf_grid_hq %>%
    filter(park_id == park) %>%
    select(geom, habitat_quality_12hr_norm,
           dist_settlement_scaled, dist_water_scaled,
           evi_scaled, human_mod_scaled, slope_scaled) %>%
    rename(
      `Habitat Quality` = habitat_quality_12hr_norm,
      `Dist. to Settlement` = dist_settlement_scaled,
      `Dist. to Water` = dist_water_scaled,
      `EVI` = evi_scaled,
      `Human Modification` = human_mod_scaled,
      `Slope` = slope_scaled
    ) %>%
    pivot_longer(
      cols = -geom,
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(variable) %>%
    mutate(value_rescaled = scales::rescale(value)) %>%
    ungroup() %>% 
    mutate(variable = factor(variable, levels = c(
      "Habitat Quality",
      "Dist. to Water",
      "EVI",
      "Human Modification",
      "Slope",
      "Dist. to Settlement"
    )))
  
  # Plot all variables in one row
  p <- ggplot(park_data) +
    geom_sf(aes(fill = value_rescaled, color = value_rescaled)) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    theme_void() +
    facet_wrap(~ variable, ncol = 6) +
    labs(fill = "Rescaled Value", color = "Rescaled Value") +
    labs(title = park) +
    theme(legend.position = "none", 
          strip.text = element_text(size = 10, face = "bold"))
  p
  # Save plot
  filename <- paste0("builds/plots/supplement/habitat_quality_maps/", gsub(" ", "_", tolower(park)), "_hq_and_drivers_rescaled.png")
  ggsave(plot = p, filename = filename, dpi = 600, height = 4, width = 12)
}
