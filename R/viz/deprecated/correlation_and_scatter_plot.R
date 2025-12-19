library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(performance)
library(sjPlot)
library(sf)
library(DHARMa)
library(broom)
library("sdmTMB")
library(sdmTMBextra)
library(future)
library(furrr)
library(groupdata2)
library(rnaturalearth)
#first sfuture#first stab at sdmTMB


#1 1000m -------------------------------------

#load data 
sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

dt <- fread("data/processed_data/clean_data/final_point_data.csv") %>% 
  mutate(grass_cover_coef = grass_cover_coef*100, 
         gr_n_cr_cover_coef = gr_n_cr_cover_coef*100, 
         tree_cover_coef = tree_cover_coef*100, 
         shrub_cover_coef = shrub_cover_coef*100, 
         bare_cover_coef = bare_cover_coef*100, 
         mean_evi_coef = mean_evi_coef/100
  ) %>% 
  filter(!park_id %in% c("Zambezi"))



# get dataframe with comlete and clean data fro mdoeling 
dt_mod <- dt %>% 
  filter(dw_min_mode_fraction >= 50) %>% 
  select(
    #mean values /habitat characteristics 
    mean_grass_cover, mean_gr_n_cr_cover, mean_tree_cover, mean_shrub_cover, mean_evi, 
    mean_habitat_diversity_100m, mean_habitat_diversity_1000m, 
    
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, 
    
    #Trends - Responses 
    grass_cover_coef, gr_n_cr_cover_coef, tree_cover_coef, shrub_cover_coef, bare_cover_coef, 
    habitat_diversity_100m_coef, habitat_diversity_1000m_coef, mean_evi_coef, 
    
    #Trends  - Predictors 
    mat_coef, prec_coef, burned_area_coef,
    
    #Elephant variables 
    mean_density_km2, local_density_km2, density_km2_estimate, population_trend_estimate, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, 
    
    #identifieres 
    park_id
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    local_density_km2_scaled = as.numeric(scale(log(local_density_km2 +0.0001))),
    density_km2_estimate_scaled = as.numeric(scale(density_km2_estimate)),
    mat_coef_scaled = as.numeric(scale(mat_coef)),
    prec_coef_scaled = as.numeric(scale(prec_coef)),
    n_deposition_scaled = as.numeric(scale(n_deposition)),
    fire_frequency_scaled = as.numeric(scale(fire_frequency)),
    burned_area_coef_scaled = as.numeric(scale(burned_area_coef)), 
    x_mollweide_scaled = as.numeric(scale(x_mollweide)), 
    y_mollweide_scaled = as.numeric(scale(y_mollweide)), 
    x_moll_km = x_mollweide/1000, 
    y_moll_km = y_mollweide/1000
  ) %>%
  group_by(park_id) %>% 
  filter(n() >= 10) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  fold(., #make sure to stratify folds in a way that each park is present in each fold
       k = 8,
       # method = "n_dist", 
       cat_col = "park_id") %>% 
  rename(fold_id = `.folds`) %>% 
  as.data.table() %>% 
  mutate(park_id = factor(park_id))


park_counts <- dt_mod[, .N, by = park_id] %>% arrange(N)
print(park_counts)
mean(park_counts$N)


hist(dt_mod$local_density_km2_scaled)
range(dt_mod$mean_density_km2)


#check model data 
table(dt_mod$park_id)
n_distinct(dt_mod$park_id)
glimpse(dt_mod)

### plot final dataset -----

#Check correlations 
dt_corr <- dt_mod %>% 
  select(-c(x_mollweide, y_mollweide, lon, lat, park_id, fold_id, x_moll_km, y_moll_km,
            mean_gr_n_cr_cover, gr_n_cr_cover_coef, bare_cover_coef), -contains("scaled")) %>% 
  rename(
    `Grass Cover` = mean_grass_cover,
    `Woody Cover` = mean_tree_cover,
    `Shrub Cover` = mean_shrub_cover,
    `EVI` = mean_evi,
    `Habitat Diversity (100 m)` = mean_habitat_diversity_100m,
    `Habitat Diversity (1000 m)` = mean_habitat_diversity_1000m,
    `Elevation` = elevation,
    `Mean Annual Temperature` = mat,
    `Mean Annual Precipitation` = map,
    `Slope` = slope,
    `Distance to Water (km)` = distance_to_water_km,
    `Nitrogen Deposition` = n_deposition,
    `Human Modification Index` = human_modification,
    `Fire Frequency` = fire_frequency,
    
    `Grass Cover Trend` = grass_cover_coef,
    `Woody Cover Trend` = tree_cover_coef,
    `Shrub Cover Trend` = shrub_cover_coef,
    `Habitat Diversity Trend (100 m)` = habitat_diversity_100m_coef,
    `Habitat Diversity Trend (1000 m)` = habitat_diversity_1000m_coef,
    `EVI Trend` = mean_evi_coef,
    `MAT Trend` = mat_coef,
    `Precipitation Trend` = prec_coef,
    `Burned Area Trend` = burned_area_coef,
    
    `Mean Elephant Density` = mean_density_km2,
    `Local Elephant Density` = local_density_km2,
    `Elephant Density Trend` = density_km2_estimate,
    `Elephant Population Trend` = population_trend_estimate
  )

corr <- round(cor(dt_corr), 1)
p_corr <- ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggsave(plot = p_corr, "builds/plots/supplement/correlations.png", dpi = 600, width = 12, height = 12)
hist(dt_mod$habitat_diversity_1000m_coef)

dt_corr2 <- dt_mod %>% 
  select(local_density_km2_scaled, density_km2_estimate_scaled, 
         mat_coef_scaled, prec_coef_scaled, n_deposition_scaled,
         fire_frequency_scaled, burned_area_coef_scaled) %>% 
  filter(complete.cases(.))

ggcorrplot(round(cor(dt_corr2), 2), hc.order = TRUE, type = "lower",
           lab = TRUE)


dt_long <- dt_mod %>% 
  mutate(log_mean_density_km2 = log(mean_density_km2), 
         log_local_density_km2 = log(local_density_km2 + 0.00001)) %>% 
  pivot_longer(cols = c(log_local_density_km2, log_mean_density_km2, n_deposition, 
               density_km2_estimate, burned_area_coef, prec_coef, mat_coef, fire_frequency), 
               names_to = "var_name", values_to = "var_value") %>% 
  mutate(clean_var_name = case_when(
    .default = var_name,
    var_name == "log_mean_density_km2" ~ "Log Mean Elephant Density",
    var_name == "log_local_density_km2" ~ "Log Local Elephant Density",
    var_name == "density_km2_estimate" ~ "Elephant Density Trend",
    var_name == "burned_area_coef" ~ "Burned Area Trend",
    var_name == "prec_coef" ~ "Precipitation Trend",
    var_name == "mat_coef" ~ "MAT Trend",
    var_name == "n_deposition" ~ "N deposition",
    var_name == "fire_frequency" ~ "Fire Frequency")) %>% 
  pivot_longer(cols = c(mean_evi_coef, habitat_diversity_100m_coef, habitat_diversity_1000m_coef, tree_cover_coef), 
               names_to = "response_name", values_to = "response_value") %>% 
  mutate(
    clean_response_name = case_when(
      .default = response_name, 
      response_name == "mean_evi_coef" ~ "EVI Trend",
      response_name == "habitat_diversity_100m_coef" ~ "Habitat Diversity Trend (100 m)",
      response_name == "habitat_diversity_1000m_coef" ~ "Habitat Diversity Trend (1000 m)",
      response_name == "tree_cover_coef" ~ "Woody Cover Trend"
    )
  )


p_scatter <- ggplot(dt_long, aes(x = var_value, y = response_value)) +
  geom_point(alpha = 0.6, size = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_grid(rows = vars(clean_response_name), cols = vars(clean_var_name), scales = "free") +
  labs(
    x = "Predictor",
    y = "Response"
  ) 
p_scatter
ggsave(plot = p_scatter, "builds/plots/supplement/raw_data_scatter_plot.png", dpi = 600, width = 12, height = 8)
