#get space use 
library(tidyverse)
library(data.table)
library(terra)
library(exactextractr)
library(sf)
library(tidylog)
library(patchwork)
library(ggcorrplot)

#get data frame with cluster specific median estimates ready. 
dt_ele <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")

dt_est_raw <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv") %>% 
  left_join(dt_ele)

dt_est_wy <- dt_est_raw %>% 
  filter(season == "whole_year") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "whole_year_estimate_")

dt_est_ds <- dt_est_raw %>% 
  filter(season == "dry_season") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "dry_season_estimate_")

dt_est_ws <- dt_est_raw %>% 
  filter(season == "wet_season") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "wet_season_estimate_")


dt_est_m <- dt_est_raw %>% 
  filter(season == "whole_year" & sex == "M") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "males_estimate_")

dt_est_f <- dt_est_raw %>% 
  filter(season == "whole_year" & sex == "F") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "females_estimate_")


dt_est_f_ds <- dt_est_raw %>% 
  filter(season == "dry_season" & sex == "F") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "females_dry_season_estimate_")

dt_est_f_ws <- dt_est_raw %>% 
  filter(season == "wet_season" & sex == "F") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "females_wet_season_estimate_")

dt_est_m_ds <- dt_est_raw %>% 
  filter(season == "dry_season" & sex == "F") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "males_dry_season_estimate_")

dt_est_m_ws <- dt_est_raw %>% 
  filter(season == "wet_season" & sex == "M") %>% 
  group_by(cluster_id, term) %>% 
  summarise(median_estimate = median(estimate, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = median_estimate,
    names_prefix = "males_wet_season_estimate_")

dt_est <- dt_est_wy %>% 
  left_join(dt_est_ds) %>% 
  left_join(dt_est_ws) %>% 
  left_join(dt_est_f) %>% 
  left_join(dt_est_m) %>% 
  left_join(dt_est_f_ds) %>% 
  left_join(dt_est_m_ds) %>% 
  left_join(dt_est_f_ws) %>% 
  left_join(dt_est_m_ws)
  

#load grid vars
dt_grid_vars <- fread("data/processed_data/data_fragments/pa_grids_with_covariates.csv") %>% 
  mutate(wdpa_pid = as.character(wdpa_pid))


dt_grid_hq <- dt_grid_vars %>% 
  left_join(dt_est) %>% 
  group_by(park_id) %>% 
  mutate(dist_settlement_scaled = as.numeric(scale(distance_to_settlement_km)), 
         dist_water_scaled = as.numeric(scale(distance_to_water_km)),
         evi_scaled = as.numeric(scale(evi_mean)),
         human_mod_scaled = as.numeric(scale(human_modification)),
         slope_scaled = as.numeric(scale(slope))) %>% 
  mutate(
    habitat_quality = (
      whole_year_estimate_distance_to_settlement_km*dist_settlement_scaled +
        whole_year_estimate_distance_to_water_km*dist_water_scaled +
        whole_year_estimate_evi_mean*evi_scaled +
        whole_year_estimate_human_modification*human_mod_scaled +
        whole_year_estimate_slope*slope_scaled
    ),
    habitat_quality_norm = scales::rescale(habitat_quality),
 
    ### Season 
    habitat_quality_dry_season = (
      dry_season_estimate_distance_to_settlement_km*dist_settlement_scaled +
        dry_season_estimate_distance_to_water_km*dist_water_scaled +
        dry_season_estimate_evi_mean*evi_scaled +
        dry_season_estimate_human_modification*human_mod_scaled +
        dry_season_estimate_slope*slope_scaled
    ),
    habitat_quality_dry_season_norm = scales::rescale(habitat_quality_dry_season),
    
    habitat_quality_wet_season = (
      wet_season_estimate_distance_to_settlement_km*dist_settlement_scaled +
        wet_season_estimate_distance_to_water_km*dist_water_scaled +
        wet_season_estimate_evi_mean*evi_scaled +
        wet_season_estimate_human_modification*human_mod_scaled +
        wet_season_estimate_slope*slope_scaled
    ),
    habitat_quality_wet_season_norm = scales::rescale(habitat_quality_wet_season),
 
    ### Sex 
    
    habitat_quality_males = (
      males_estimate_distance_to_settlement_km*dist_settlement_scaled +
        males_estimate_distance_to_water_km*dist_water_scaled +
        males_estimate_evi_mean*evi_scaled +
        males_estimate_human_modification*human_mod_scaled +
        males_estimate_slope*slope_scaled
    ),
    habitat_quality_males_norm = scales::rescale(habitat_quality_males),
    
    habitat_quality_females = (
      females_estimate_distance_to_settlement_km*dist_settlement_scaled +
        females_estimate_distance_to_water_km*dist_water_scaled +
        females_estimate_evi_mean*evi_scaled +
        females_estimate_human_modification*human_mod_scaled +
        females_estimate_slope*slope_scaled
    ),
    habitat_quality_females_norm = scales::rescale(habitat_quality_females),
    
    ### Sex x Season
    
    habitat_quality_females_wet_season = (
      females_wet_season_estimate_distance_to_settlement_km*dist_settlement_scaled +
        females_wet_season_estimate_distance_to_water_km*dist_water_scaled +
        females_wet_season_estimate_evi_mean*evi_scaled +
        females_wet_season_estimate_human_modification*human_mod_scaled +
        females_wet_season_estimate_slope*slope_scaled
    ),
    habitat_quality_females_wet_season_norm = scales::rescale(habitat_quality_females_wet_season),
    
    habitat_quality_females_dry_season = (
      females_dry_season_estimate_distance_to_settlement_km*dist_settlement_scaled +
        females_dry_season_estimate_distance_to_water_km*dist_water_scaled +
        females_dry_season_estimate_evi_mean*evi_scaled +
        females_dry_season_estimate_human_modification*human_mod_scaled +
        females_dry_season_estimate_slope*slope_scaled
    ),
    habitat_quality_females_dry_season_norm = scales::rescale(habitat_quality_females_dry_season),
    
    habitat_quality_males_dry_season = (
      males_dry_season_estimate_distance_to_settlement_km*dist_settlement_scaled +
        males_dry_season_estimate_distance_to_water_km*dist_water_scaled +
        males_dry_season_estimate_evi_mean*evi_scaled +
        males_dry_season_estimate_human_modification*human_mod_scaled +
        males_dry_season_estimate_slope*slope_scaled
    ),
    habitat_quality_males_dry_season_norm = scales::rescale(habitat_quality_males_dry_season),
    
    habitat_quality_males_wet_season = (
      males_wet_season_estimate_distance_to_settlement_km*dist_settlement_scaled +
        males_wet_season_estimate_distance_to_water_km*dist_water_scaled +
        males_wet_season_estimate_evi_mean*evi_scaled +
        males_wet_season_estimate_human_modification*human_mod_scaled +
        males_wet_season_estimate_slope*slope_scaled
    ),
    habitat_quality_males_wet_season_norm = scales::rescale(habitat_quality_males_wet_season),
    
  ) %>% 
  ungroup()

glimpse(dt_grid_hq[dt_grid_hq$park_id == "Kafue", ])


fwrite(dt_grid_hq, "data/processed_data/data_fragments/pa_grid_with_habitat_quality.csv")

dt_corr <- dt_grid_hq %>% 
  dplyr::select(habitat_quality_norm,
                habitat_quality_dry_season_norm, habitat_quality_wet_season_norm, 
                habitat_quality_males_norm, habitat_quality_females_norm, 
                habitat_quality_males_dry_season_norm, habitat_quality_females_dry_season_norm,
                habitat_quality_males_wet_season_norm, habitat_quality_females_wet_season_norm) %>% 
  filter(complete.cases(.))

corr <- round(cor(dt_corr), 2)
head(corr[, 1:6])
(p_corr <- ggcorrplot::ggcorrplot(corr, hc.order = F, type = "lower",
                      lab = TRUE))
ggsave(plot = p_corr, "builds/plots/supplement/habitat_quality_correlations.png", dpi = 600, height = 10, width = 10)


#plots ---------------

dt_grid_hq <- fread("data/processed_data/data_fragments/pa_grid_with_habitat_quality.csv")

dt_pc <- fread("data/processed_data/clean_data/all_population_counts.csv") %>% 
  group_by(park_id) %>% 
  mutate(n = n()) %>% 
  filter(n() >= 3) %>% 
  ungroup()


# sf_grid_hq_raw <- st_read("data/spatial_data/grid/empty_grid_pas.gpkg")


# sf_grid_hq <- sf_grid_hq_raw %>% 
#  filter(wdpa_pid %in% unique(dt_pc$wdpa_pid)) %>% 
#  left_join(dt_grid_hq %>% dplyr::select( -c("park_id", "country_code_iso3", "designation", "wdpa_pid", "iucn_cat",
#                              "x_mollweide", "y_mollweide", "lon", "lat")))



for (park in unique(dt_grid_hq$park_id)) {
  
  print(paste0("Start with ", park))
  
  # Filter data for the current park
  park_data <- dt_grid_hq %>%
    filter(park_id == park) %>%
    select(cluster_id, x_mollweide, y_mollweide, habitat_quality_norm, 
           habitat_quality_wet_season_norm, habitat_quality_dry_season_norm, 
           habitat_quality_males_norm, habitat_quality_females_norm, 
           habitat_quality_males_wet_season_norm, habitat_quality_females_wet_season_norm, 
           habitat_quality_males_dry_season_norm, habitat_quality_females_dry_season_norm,
           dist_settlement_scaled, dist_water_scaled,
           evi_scaled, human_mod_scaled, slope_scaled) %>%
    rename(
      `Overall HQ` = habitat_quality_norm,
      `Wet Season HQ` = habitat_quality_wet_season_norm,
      `Dry Season HQ` = habitat_quality_dry_season_norm,
      `Males HQ` = habitat_quality_males_norm,
      `Females HQ` = habitat_quality_females_norm,
      `Males Wet Season HQ` = habitat_quality_males_wet_season_norm,
      `Females Wet Season HQ` = habitat_quality_females_wet_season_norm,
      `Males Dry Season HQ` = habitat_quality_males_dry_season_norm,
      `Females Dry Season HQ` = habitat_quality_females_dry_season_norm,
      `Dist. Settlement` = dist_settlement_scaled,
      `Dist. Water` = dist_water_scaled,
      `EVI` = evi_scaled,
      `HMI` = human_mod_scaled,
      `Slope` = slope_scaled
    ) %>%
    pivot_longer(
      cols = -c(x_mollweide, y_mollweide),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(variable) %>%
    mutate(value_rescaled = scales::rescale(value)) %>%
    ungroup() %>% 
    mutate(variable = factor(variable, levels = c(
      "Overall HQ",
      "Wet Season HQ",
      "Dry Season HQ", 
      "Males HQ", 
      "Females HQ", 
      "Males Wet Season HQ", 
      "Males Dry Season HQ", 
      "Females Wet Season HQ",
      "Females Dry Season HQ",
      "Dist. Water",
      "EVI",
      "HMI",
      "Slope",
      "Dist. Settlement"
    )))
  
  # Plot all variables in one row
  p_1 <- park_data %>% 
    filter(!variable %in% c("Dist. Water", 
                            "EVI", 
                            "HMI", 
                            "Slope", 
                            #"Males Wet Season HQ", 
                            #"Females Wet Season HQ",
                            #"Males Dry Season HQ", 
                            #"Females Dry Season HQ",
                            "Dist. Settlement"
                            )) %>% 
    ggplot() +
   # geom_sf(aes(fill = value_rescaled, color = value_rescaled)) +
    geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = value_rescaled)) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    theme_void() +
    facet_wrap(~ variable, ncol = 5) +
    labs(fill = "Rescaled Value", color = "Rescaled Value") +
    labs(title = park, subtitle = paste0("Cluster: ",  unique(park_data$cluster_id), "; HQ = Habitat Quality")) +
    theme(legend.position = "none", 
          strip.text = element_text(size = 10, face = "bold"), 
          plot.title = element_text(face = "bold", size = 14)) +
    coord_fixed()
  p_1
  
  p_2 <- park_data %>% 
    filter(variable %in% c("Dist. Water", 
                            "EVI", 
                            "HMI", 
                            "Slope", 
                            "Dist. Settlement"
    )) %>% 
    ggplot() +
   #geom_sf(aes(fill = value_rescaled, color = value_rescaled)) +
    geom_raster(aes(x = x_mollweide, y = y_mollweide, fill = value_rescaled)) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    theme_void() +
    facet_wrap(~ variable, ncol = 5) +
    labs(fill = "Rescaled Value", color = "Rescaled Value") +
    labs(subtitle = park, title = "Drivers of elephant habitat selection, used to calculate HQ") +
    theme(legend.position = "bottom", 
          strip.text = element_text(size = 10, face = "bold")) +
    coord_fixed()
  p_2
  
  library(patchwork)
  
  p <- p_1 / p_2 + plot_layout(heights = c(2, 1))
  # Save plot
  filename <- paste0("builds/plots/supplement/habitat_quality_maps/", gsub(" ", "_", tolower(park)), "_hq_and_drivers_rescaled.png")
  ggsave(plot = p, filename = filename, dpi = 600, height = 10, width = 12)
}
