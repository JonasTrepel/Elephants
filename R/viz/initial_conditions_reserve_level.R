library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(broom)
library("sdmTMB")
library(rnaturalearth)
library(future)
library(ggspatial)
library(groupdata2)
library(GGally)
library(scico)
library(sf)
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

first_time = "no"

if(first_time == "yes"){
  


#load data 
#sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 
dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100) 



# get dataframe with comlete and clean data fro mdoeling 

quantile(dt$dw_min_median_mode_fraction, na.rm = T)
table(dt$population_trend_n)
dt_pad <- dt %>% 
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
    x_mollweide, y_mollweide, lon, lat, area_km2,
    
    #identifieres 
    park_id, cluster_id, grid_id
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    x_moll_km = x_mollweide/1000, 
    y_moll_km = y_mollweide/1000,
  ) %>%
  group_by(cluster_id, park_id) %>% 
  summarize(tree_cover_1000m_coef = mean(tree_cover_1000m_coef), 
            initial_tree_cover = mean(tree_cover_1000m_2015_2016), 
            initial_canopy_height = mean(canopy_height_900m_2000), 
            canopy_height_900m_coef = mean(canopy_height_900m_coef), 
            mean_density_km2 = mean(mean_density_km2),
            elevation = mean(elevation), 
            mat  = mean(mat),
            map = mean(map),
            n_deposition = mean(n_deposition), 
            human_modification = mean(human_modification), 
            fire_frequency = mean(fire_frequency),
            fire_frequency_sd = mean(fire_frequency),
            months_extreme_drought = mean(months_extreme_drought),
            mat_coef = mean(mat_coef),
            prec_coef = mean(prec_coef), 
            area_km2 = mean(area_km2),
            sd_local_density_km2 = sd(local_density_km2), 
            local_density_km2 = mean(local_density_km2),
            n = n()
  ) %>% 
  mutate(cv_local_density_km2 = (sd_local_density_km2/local_density_km2)*100) %>% 
  ungroup() %>% 
  mutate(mean_density_km2_scaled = as.numeric(scale(mean_density_km2))) #%>% mutate(mean_density_km2_scaled = mean_density_km2)
fwrite(dt_pad,"data/processed_data/clean_data/reserve_level_average_data.csv")

}

dt = fread("data/processed_data/clean_data/reserve_level_average_data.csv")
