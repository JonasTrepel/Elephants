library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(broom)
library("sdmTMB")
library(future)
library(furrr)
library(groupdata2)
library(GGally)
library(glmmTMB)
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

#load data 
#sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100)



# get dataframe with comlete and clean data fro mdoeling 

quantile(dt$dw_min_median_mode_fraction, na.rm = T)
acceptable_numbers = seq(1, 10000000, 5)
table(dt$population_trend_n)
dt_mod <- dt %>% 
  filter(dw_min_median_mode_fraction >= 50) %>% 
  dplyr::select(
    #mean values /habitat characteristics 
    mean_tree_cover_1000m, mean_evi_900m, mean_canopy_height_900m, 
    
    #starting conditions
    tree_cover_1000m_2015_2016, evi_900m_2013_2014, canopy_height_900m_2000,
    
    # environmental predictors
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, months_severe_drought, months_extreme_drought,
    mat_coef, prec_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2,# density_trend_estimate, density_trend_estimate,
    
    #Trends - Responses 
    tree_cover_1000m_coef, evi_900m_coef, canopy_height_900m_coef, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, 
    
    #identifieres 
    park_id, cluster_id, grid_id
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    x_moll_km = x_mollweide/1000, 
    y_moll_km = y_mollweide/1000,
  ) %>%
  group_by(park_id) %>% 
  # mutate(park_row_nr = 1:n()) %>% 
  #  filter(park_row_nr %in% acceptable_numbers) %>% 
  filter(n() >= 10) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  fold(., #make sure to stratify folds in a way that each park is present in each fold
       k = 5,
       cat_col = "park_id") %>% 
  rename(fold_id = `.folds`) %>% 
  as.data.table() %>% 
  mutate(park_id = factor(park_id)) %>% 
  mutate(
    local_density_km2_scaled = as.numeric(scale(local_density_km2)),
    mean_density_km2_scaled = as.numeric(scale(mean_density_km2)),
    months_extreme_drought_scaled = as.numeric(scale(months_extreme_drought)),
    fire_frequency_scaled = as.numeric(scale(fire_frequency)),
    mat_coef_scaled = as.numeric(scale(mat_coef)),
    prec_coef_scaled = as.numeric(scale(prec_coef)),
    n_deposition_scaled = as.numeric(scale(n_deposition)), 
    mat_scaled = as.numeric(scale(mat)), 
    map_scaled = as.numeric(scale(map))
  )


cor.test(dt$evi_900m_coef, dt$evi_mean)
cor.test(dt$tree_cover_1000m_coef, dt$evi_mean)
cor.test(dt$canopy_height_900m_coef, dt$evi_mean)



library(mgcv)


m_tc <- bam(tree_cover_1000m_coef  ~ 
                 s(local_density_km2_scaled, k = 3) +
                 s(months_extreme_drought_scaled, k = 3) +
                 s(fire_frequency_scaled, k = 3) +
                 s(mat_coef_scaled, k = 3) + 
                 s(n_deposition_scaled, k = 3), 
               data = dt_mod, 
            method="fREML",
            spatial = "off")

concurvity(m_tc, full = T)
concurvity(m_tc, full = F) %>% 
  as.data.frame() %>% 
  mutate(across(where(is.numeric), round, 3))

m_ch <- bam(canopy_height_900m_coef  ~ 
              s(local_density_km2_scaled, k = 3) +
              s(months_extreme_drought_scaled, k = 3) +
              s(fire_frequency_scaled, k = 3) +
              s(mat_coef_scaled, k = 3) + 
              s(n_deposition_scaled, k = 3), 
            data = dt_mod, 
            method="fREML",
            spatial = "off")

concurvity(m_ch, full = T)
concurvity(m_ch, full = F) %>% 
  as.data.frame() %>% 
  mutate(across(where(is.numeric), round, 3))
