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

track_12_random <- data.frame()
track_1_random <- data.frame()
i <- 0

# 2. Resample and create random steps -------------------------------
for(id in unique(track$individual_id)){
  
  
  track_sub <- track %>% filter(individual_id %in% id)
  
  #12 hour intervals 
  if(unique(track_sub$median_interval_mins < 780)){

    tryCatch({
  track_sub_12 <- track_sub  %>% 
    track_resample(rate = hours(12), tolerance = minutes(60)) %>% 
    mutate(median_relocation_time = ifelse(n() > 1, median(diff(t_), na.rm = TRUE) / dminutes(1), NA)) %>% 
    steps_by_burst(lonlat = FALSE, keep_cols = "end") %>% 
    random_steps(ncontrol = 10) %>% 
    mutate(individual_id = id)
  
  
  track_12_random <- rbind(track_sub_12, track_12_random)
  
   
   }, error = function(e) {
      message(paste0("error in 12h track for ", id, ": ", e$message))
    })
  }
  
  #1 hour intervals 
  
  if(unique(track_sub$mean_interval_mins < 90)){
    
    tryCatch({
  track_sub_1 <- track_sub  %>% 
    track_resample(rate = hours(1), tolerance = minutes(15)) %>% 
    mutate(median_relocation_time = ifelse(n() > 1, median(diff(t_), na.rm = TRUE) / dminutes(1), NA)) %>% 
    steps_by_burst(lonlat = FALSE, keep_cols = "end") %>% 
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
n_distinct(track_12_random$individual_id)




quantile(track$median_interval_mins)
