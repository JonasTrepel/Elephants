#### anonymize step data 
library(data.table)
library(tidyverse)

#24 hour steps 
dt <- fread("data/processed_data/data_fragments/steps_24hrs_habitat_covariates.csv") 
glimpse(dt)
dt_anon <- dt %>% 
  dplyr::select(individual_id, case_, step_id_, burst_, source, sex, 
                obs_id, n_true, n_total, sl_km, min_kmh, dt_hour, 
                unique_id, n_false_step, elevation, map, slope, 
                distance_to_water_km, distance_to_settlement_km,
                human_modification, evi_mean, season)

fwrite(dt_anon, "data/processed_data/for_review_only/anon_steps_24hrs_habitat_covariates.csv")

#1 hour steps
dt <- fread("data/processed_data/data_fragments/steps_1hr_habitat_covariates.csv") 
glimpse(dt)
dt_anon <- dt %>% 
  dplyr::select(individual_id, case_, step_id_, burst_, source, sex, 
                obs_id, n_true, n_total, sl_km, min_kmh, dt_hour, 
                unique_id, n_false_step, elevation, map, slope, 
                distance_to_water_km, distance_to_settlement_km,
                human_modification, evi_mean, season)

fwrite(dt_anon, "data/processed_data/for_review_only/anon_steps_1hr_habitat_covariates.csv")

#3 hour steps
dt <- fread("data/processed_data/data_fragments/steps_3hrs_habitat_covariates.csv") 
glimpse(dt)
dt_anon <- dt %>% 
  dplyr::select(individual_id, case_, step_id_, burst_, source, sex, 
                obs_id, n_true, n_total, sl_km, min_kmh, dt_hour, 
                unique_id, n_false_step, elevation, map, slope, 
                distance_to_water_km, distance_to_settlement_km,
                human_modification, evi_mean, season)

fwrite(dt_anon, "data/processed_data/for_review_only/anon_steps_3hrs_habitat_covariates.csv")

#12 hour steps
dt <- fread("data/processed_data/data_fragments/steps_12hrs_habitat_covariates.csv") 
glimpse(dt)
dt_anon <- dt %>% 
  dplyr::select(individual_id, case_, step_id_, burst_, source, sex, 
                obs_id, n_true, n_total, sl_km, min_kmh, dt_hour, 
                unique_id, n_false_step, elevation, map, slope, 
                distance_to_water_km, distance_to_settlement_km,
                human_modification, evi_mean, season)

fwrite(dt_anon, "data/processed_data/for_review_only/anon_steps_12hrs_habitat_covariates.csv")


###  
dt_ele <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")
glimpse(dt_ele)
dt_anon <- dt_ele %>% 
  dplyr::select(-start_date, end_date, park_id, wdpa_pid)

fwrite(dt_anon, "data/processed_data/for_review_only/anon_elephant_id_meta_data.csv")


