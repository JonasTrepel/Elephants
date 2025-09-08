library(amt)
library(sf)
library(data.table)
library(tidyverse)
library(mapview)
library(exactextractr)
library(terra)

# 1. Load Data ---------------------------------
sf_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = "ESRI:54009")

coords <- st_coordinates(sf_loc)
sf_loc$x_moll <- coords[,1]
sf_loc$y_moll <- coords[,2]


track <- make_track(.x = x_moll, .y = y_moll, .t = date_time, tbl = sf_loc, 
                    all_cols = T, crs = "ESRI:54009")


# 2. Resample and create random steps -------------------------------
track_24_random <- data.frame()
track_12_random <- data.frame()
track_3_random <- data.frame()
track_1_random <- data.frame()
i <- 0
for(id in unique(track$individual_id)){
  
  
  track_sub <- track %>% filter(individual_id %in% id)
  
  
  #24 hour intervals 
  if(unique(track_sub$median_interval_hrs)  < 26){
    set.seed(161)
    
    tryCatch({
      track_sub_24 <- track_sub  %>% 
        track_resample(rate = hours(24), tolerance = minutes(240)) %>% 
        mutate(median_relocation_time = ifelse(n() > 1, median(diff(t_), na.rm = TRUE) / dminutes(1), NA)) %>% 
        steps(lonlat = FALSE, keep_cols = "end") %>% 
        random_steps(ncontrol = 10) %>% 
        mutate(individual_id = id)
      
      
      track_24_random <- rbind(track_sub_24, track_24_random)
      
      
    }, error = function(e) {
      message(paste0("error in 24h track for ", id, ": ", e$message))
    })
  }
  
  
  #12 hour intervals 
  if(unique(track_sub$median_interval_hrs) < 13){
    set.seed(161)
    
    tryCatch({
  track_sub_12 <- track_sub  %>% 
    track_resample(rate = hours(12), tolerance = minutes(120)) %>% 
    mutate(median_relocation_time = ifelse(n() > 1, median(diff(t_), na.rm = TRUE) / dminutes(1), NA)) %>% 
    steps(lonlat = FALSE, keep_cols = "end") %>% 
    random_steps(ncontrol = 10) %>% 
    mutate(individual_id = id)
  
  
  track_12_random <- rbind(track_sub_12, track_12_random)
  
   
   }, error = function(e) {
      message(paste0("error in 12h track for ", id, ": ", e$message))
    })
  }
  
  #3 hour intervals 
  if(unique(track_sub$median_interval_hrs) < 4){
    set.seed(161)
    
    tryCatch({
      track_sub_3 <- track_sub  %>% 
        track_resample(rate = hours(3), tolerance = minutes(30)) %>% 
        mutate(median_relocation_time = ifelse(n() > 1, median(diff(t_), na.rm = TRUE) / dminutes(1), NA)) %>% 
        steps(lonlat = FALSE, keep_cols = "end") %>% 
        random_steps(ncontrol = 10) %>% 
        mutate(individual_id = id)
      
      
      track_3_random <- rbind(track_sub_3, track_3_random)
      
      
    }, error = function(e) {
      message(paste0("error in 3h track for ", id, ": ", e$message))
    })
  }
  
  #1 hour intervals 
  
  if(unique(track_sub$median_interval_hrs) < 2){
    set.seed(1910)
    
    tryCatch({
  track_sub_1 <- track_sub  %>% 
    track_resample(rate = hours(1), tolerance = minutes(15)) %>% 
    mutate(median_relocation_time = ifelse(n() > 1, median(diff(t_), na.rm = TRUE) / dminutes(1), NA)) %>% 
    steps(lonlat = FALSE, keep_cols = "end") %>% 
    random_steps(ncontrol = 10) %>% 
    mutate(individual_id = id)
  
    }, error = function(e) {
      message(paste0("error in 1h track for ", id, ": ", e$message))
    })
    
  track_1_random <- rbind(track_sub_1, track_1_random)
  
  }
  
  i = i+1
  print(paste0(id, " done (",
               i, " of ", n_distinct(track$individual_id), 
               " (", Sys.time(),")"))
  
}



n_distinct(track_1_random$individual_id)
n_distinct(track_3_random$individual_id)
n_distinct(track_12_random$individual_id)
n_distinct(track_24_random$individual_id)


### Save ---------
library(tidylog)
names(track_1_random)
steps_1hr <- track_1_random %>% 
  as.data.table() %>% 
  dplyr::select(individual_id, 
                x1_, x2_, y1_, y2_, t1_, t2_,
                case_, sl_, ta_, dt_, step_id_, burst_,
                source, sex, park_id, wdpa_pid, overlap_percent, obs_id,
                hr_mcp_area_km2, hr_locoh_area_km2, hr_diameter_km) %>% 
  group_by(individual_id) %>%
  mutate(
    start_date = min(t2_),
    end_date = max(t2_),
    month = month(t2_),
    start_year = year(start_date),
    end_year = year(end_date),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    duration_years = duration_days / 365.25,
    mean_interval_mins = mean(t2_ - t1_, na.rm = TRUE) / dminutes(1),
    median_interval_mins = median(t2_ - t1_, na.rm = TRUE) / dminutes(1),
    n_true = n()/11, 
    n_total = n()) %>% 
  ungroup() %>% 
  mutate(dt_hour = as.numeric(dt_)/60, 
         sl_km = sl_/1000, 
         min_kmh = sl_km/dt_hour, 
         season = ifelse(month %in% c(5,6,7,8,9), "dry_season", "wet_season"), 
         unique_id = paste0("step_", 1:nrow(.))) %>% 
  filter(duration_years >= 1 & min_kmh < 25) %>% 
  filter(dt_hour < 2) %>% 
  as.data.table()
  
hist(steps_1hr$dt_hour, breaks = 100)
hist(as.numeric(track_1_random$dt_), breaks = 100)

summary(steps_1hr)
n_distinct(steps_1hr$individual_id)
quantile(steps_1hr$sl_km)

fwrite(steps_1hr, "data/processed_data/data_fragments/steps_1hr_incl_random.csv")

steps_3hrs <- track_3_random %>% 
  as.data.table() %>% 
  dplyr::select(individual_id, 
                x1_, x2_, y1_, y2_, t1_, t2_,
                case_, sl_, ta_, dt_, step_id_, burst_,
                source, sex, park_id, wdpa_pid, overlap_percent, obs_id,
                hr_mcp_area_km2, hr_locoh_area_km2, hr_diameter_km) %>% 
  group_by(individual_id) %>%
  mutate(
    start_date = min(t2_),
    end_date = max(t2_),
    month = month(t2_),
    start_year = year(start_date),
    end_year = year(end_date),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    duration_years = duration_days / 365.25,
    mean_interval_mins = mean(t2_ - t1_, na.rm = TRUE) / dminutes(1),
    median_interval_mins = median(t2_ - t1_, na.rm = TRUE) / dminutes(1),
    n_true = n()/11, 
    n_total = n()) %>% 
  ungroup() %>% 
  mutate(dt_hour = as.numeric(dt_), 
         sl_km = sl_/1000, 
         min_kmh = sl_km/dt_hour, 
         season = ifelse(month %in% c(5,6,7,8,9), "dry_season", "wet_season"), 
         unique_id = paste0("step_", 1:nrow(.))) %>% 
  filter(duration_years >= 1 & min_kmh < 25) %>% 
  filter(dt_hour < 4) %>%   
  as.data.table()

summary(steps_3hrs)
n_distinct(steps_3hrs$individual_id)
quantile(steps_3hrs$sl_km)

fwrite(steps_3hrs, "data/processed_data/data_fragments/steps_3hrs_incl_random.csv")

steps_12hrs <- track_12_random %>% 
  as.data.table() %>% 
  dplyr::select(individual_id, 
                x1_, x2_, y1_, y2_, t1_, t2_,
                case_, sl_, ta_, dt_, step_id_, burst_,
                source, sex, park_id, wdpa_pid, overlap_percent, obs_id,
                hr_mcp_area_km2, hr_locoh_area_km2, hr_diameter_km) %>% 
  group_by(individual_id) %>%
  mutate(
    start_date = min(t2_),
    end_date = max(t2_),
    month = month(t2_),
    start_year = year(start_date),
    end_year = year(end_date),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    duration_years = duration_days / 365.25,
    mean_interval_mins = mean(t2_ - t1_, na.rm = TRUE) / dminutes(1),
    median_interval_mins = median(t2_ - t1_, na.rm = TRUE) / dminutes(1),
    n_true = n()/11, 
    n_total = n()) %>% 
  ungroup() %>% 
  mutate(dt_hour = as.numeric(dt_), 
         sl_km = sl_/1000, 
         min_kmh = sl_km/dt_hour, 
         season = ifelse(month %in% c(5,6,7,8,9), "dry_season", "wet_season"), 
         unique_id = paste0("step_", 1:nrow(.))) %>% 
  filter(duration_years >= 1 & min_kmh < 25) %>% 
  filter(dt_hour < 14) %>% 
  as.data.table()

summary(steps_12hrs)
n_distinct(steps_12hrs$individual_id)
quantile(steps_12hrs$sl_km)

fwrite(steps_12hrs, "data/processed_data/data_fragments/steps_12hrs_incl_random.csv")

# ## 24 hours 
steps_24hrs <- track_24_random %>%
  as.data.table() %>%
  dplyr::select(individual_id,
                x1_, x2_, y1_, y2_, t1_, t2_,
                case_, sl_, ta_, dt_, step_id_, burst_,
                source, sex, park_id, wdpa_pid, overlap_percent, obs_id,
                hr_mcp_area_km2, hr_locoh_area_km2, hr_diameter_km) %>%
  group_by(individual_id) %>%
  mutate(
    start_date = min(t2_),
    end_date = max(t2_),
    month = month(t2_),
    start_year = year(start_date),
    end_year = year(end_date),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    duration_years = duration_days / 365.25,
    mean_interval_mins = mean(t2_ - t1_, na.rm = TRUE) / dminutes(1),
    median_interval_mins = median(t2_ - t1_, na.rm = TRUE) / dminutes(1),
    n_true = n()/11,
    n_total = n()) %>%
  ungroup() %>%
  mutate(dt_hour = as.numeric(dt_),
         sl_km = sl_/1000,
         min_kmh = sl_km/dt_hour,
         season = ifelse(month %in% c(5,6,7,8,9), "dry_season", "wet_season"),
         unique_id = paste0("step_", 1:nrow(.))) %>%
  filter(duration_years >= 1 & min_kmh < 40 & sl_km < 100 & dt_hour < 26) %>%
  as.data.table()

summary(steps_24hrs)
n_distinct(steps_24hrs$individual_id)
quantile(steps_24hrs$n_true)

fwrite(steps_24hrs, "data/processed_data/data_fragments/steps_24hrs_incl_random.csv")
