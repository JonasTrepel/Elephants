### Compile location master dataset 

library(sf)
library(data.table)
library(tidyverse)
library("CoordinateCleaner")
library("countrycode")
library(rnaturalearth)
library(tidylog)
library(mapview)
library(amt)


######## LOAD AND CLEAN DATA ########

# Park Boundaries 

pas <- st_read("data/spatial_data/park_boundaries.gpkg")

# Africa 

world <- ne_countries(scale = "medium", returnclass = "sf")

africa <- world %>% filter(region_un == "Africa") %>% 
  filter(!name == "Madagascar")


## Cord limits: y = 
## Coordinate cleaner 

# CERU ---------------------------

dt_ceru_raw <- fread("data/raw_data/ceru/Full Telemetry Data.csv") %>% 
  dplyr::select(
    individual_id = EID, 
    lon = Longitude, 
    lat = Latitude, 
    date_time = DateTime, 
    sex = Sex
  ) %>% 
  mutate(population_id = NA, 
         park_id = NA, 
         source = "CERU") %>% 
  filter(!abs(lat) > 90 & !abs(lon) > 180) %>% 
  clean_coordinates(., 
                  lon = "lon", 
                  lat = "lat", 
                  species = "individual_id", 
                  tests = c("capitals", "centroids",
                            "equal", "zeros", "outliers", 
                            "seas"),
                  outliers_td = 100, #distance to all other points of a species in km
                  capitals_rad = 10000, #radius around capitol in m
                  outliers_method = "distance")


dt_ceru <- dt_ceru_raw %>%
  filter(.summary == TRUE) %>% #select only clean coords
  dplyr::select(-c(".otl", ".summary", ".val", ".equ", ".zer", ".cap", ".cen", ".sea")) %>% 
  filter(!lat < -35 & !lat > 35 & !lon < -20 & !lon > 55) #exclude points outside of africa
range(dt_ceru$lon)
range(dt_ceru$lat)

n_distinct(dt_ceru$individual_id) #335 elephants 

sf_ceru <- st_as_sf(dt_ceru, 
                    coords = c("lon", "lat"), 
                    crs = 4326)

sf_ceru %>% 
  sample_n(100000) %>% 
  ggplot() +
  ylim(-35, 0) +
  xlim(7.5, 40) +
  geom_sf(data = africa) +
  geom_sf(size = 0.1, alpha = 0.25)+
  theme_minimal()

# Hwange ---------------------------

dt_hwange_raw <- fread("data/raw_data/hwange/cnrs_gps_elephant_hwange.csv") %>% 
  dplyr::select(
    individual_id = animal, 
    lon = long, 
    lat = lat, 
    date_time = date_time_local, 
  ) %>% 
  mutate(population_id = NA, 
         park_id = NA, 
         source = "SCJ", 
         sex = "F") %>% #All females, in distinct herds. 
  filter(!abs(lat) > 90 & !abs(lon) > 180) %>% 
  clean_coordinates(., 
                    lon = "lon", 
                    lat = "lat", 
                    species = "individual_id", 
                    tests = c("capitals", "centroids",
                              "equal", "zeros", "outliers", 
                              "seas"),
                    outliers_td = 100, #distance to all other points of a species in km
                    capitals_rad = 10000, #radius around capital in m
                    outliers_method = "distance")


dt_hwange <- dt_hwange_raw %>%
  filter(.summary == TRUE) %>% #select only clean coords
  dplyr::select(-c(".otl", ".summary", ".val", ".equ", ".zer", ".cap", ".cen", ".sea")) %>% 
  filter(!lat < -35 & !lat > 35 & !lon < -20 & !lon > 55) #exclude points outside of africa
range(dt_hwange$lon)
range(dt_hwange$lat)

n_distinct(dt_hwange$individual_id) #335 elephants 

sf_hwange <- st_as_sf(dt_hwange, 
                    coords = c("lon", "lat"), 
                    crs = 4326)

sf_hwange %>% 
  sample_n(100000) %>% 
  ggplot() +
  ylim(-35, 0) +
  xlim(7.5, 40) +
  geom_sf(data = africa) +
  geom_sf(data = sf_ceru %>% sample_n(100000), size = 0.1) + 
  geom_sf(size = 0.1, color = "forestgreen", alpha = 0.25) +
  theme_minimal()


# Kaingo Earthranger ---------------------------
dt_kaingo_raw <- fread("data/raw_data/kaingo/Event Export 2025-02-28.csv") %>% 
  dplyr::select(
    individual_id = ID, 
    lon = Longitude, 
    lat = Latitude, 
    date_time = `Reported_At_(GMT+2:0)`, 
  ) %>% 
  mutate(population_id = NA,
         park_id = NA, 
         source = "Kaingo", 
         sex = case_when(
           individual_id %in% c("Kambaku", "Peanuts", "Tiny", "Unknown Bull/s") ~ "M", 
           individual_id %in% c("") ~ "U", 
           individual_id %in% c("Mokolo Herd", "Unknown Herd", "Kaingo Herd") ~ "F"), 
         individual_id = ifelse(individual_id %in% c("", "Unknown Herd"), "Unknown", individual_id)
  ) %>% #
  filter(!abs(lat) > 90 & !abs(lon) > 180) %>% 
  clean_coordinates(., 
                    lon = "lon", 
                    lat = "lat", 
                    species = "individual_id", 
                    tests = c("capitals", "centroids",
                              "equal", "zeros", "outliers", 
                              "seas"),
                    outliers_td = 100, #distance to all other points of a species in km
                    capitals_rad = 10000, #radius around capitol
                    outliers_method = "distance")


dt_kaingo <- dt_kaingo_raw %>%
  filter(.summary == TRUE) %>% #select only clean coords
  dplyr::select(-c(".otl", ".summary", ".val", ".equ", ".zer", ".cap", ".cen", ".sea")) %>% 
  filter(!lat < -35 & !lat > 35 & !lon < -20 & !lon > 55) #exclude points outside of africa
range(dt_kaingo$lon)
range(dt_kaingo$lat)

n_distinct(dt_kaingo$individual_id) #335 elephants 

sf_kaingo <- st_as_sf(dt_kaingo, 
                      coords = c("lon", "lat"), 
                      crs = 4326)

sf_kaingo %>% 
  #sample_n(100000) %>% 
  ggplot() +
  #ylim(-35, 0) +
  #xlim(7.5, 40) +
  #geom_sf(data = africa) +
  #geom_sf(data = sf_ceru %>% sample_n(100000), size = 0.1) + 
  geom_sf(size = 0.1, color = "forestgreen", alpha = 0.25) +
  theme_minimal()

mapview(sf_kaingo)

# Kaingo Peanuts -------------------------------------
unzip("data/raw_data/kaingo/Peanuts (2025-04-30 090720 AfricaJohannesburg).kmz", exdir = "data/raw_data/kaingo")


dt_peanuts_raw <- st_read("data/raw_data/kaingo/doc.kml") 
mapview(dt_peanuts_raw)

sf_peanuts_raw <- dt_peanuts_raw %>% 
  filter(!grepl("Last", Name)) %>% 
  filter(!Description == "2024-04-01 23:06:32 Africa/Johannesburg") %>% 
  st_collection_extract("POINT") %>%
  st_sf() %>% 
  st_zm(drop = TRUE, what = "ZM") 

sf_peanuts_raw$lon <- st_coordinates(sf_peanuts_raw)[,1]
sf_peanuts_raw$lat <- st_coordinates(sf_peanuts_raw)[,2]

dt_peanuts <- sf_peanuts_raw %>% 
  as.data.table() %>% 
  mutate(geometry = NULL, 
         population_id = NA, 
         park_id = NA, 
         source = "Kaingo", 
         sex = "M", 
         individual_id = "Peanuts", 
         date_time = as_datetime(gsub(" Africa/Johannesburg", "", Description))) %>% 
  dplyr::select(-Name, -Description)


sf_peanuts <- st_as_sf(dt_peanuts, 
                       coords = c("lon", "lat"), 
                       crs = 4326)


# SANParks ---------------------------------------



# Lapalala ---------------------------------------



# HiP ----------------------------------------



# Ithala --------------------------------------------



# Combine ---------------------------------------


dt_loc_raw <- rbind(
  dt_ceru, 
  dt_hwange, 
  dt_peanuts
)
setDT(dt_loc_raw)


################## Get Duration, Start and End of Tracking and mean relocation interval ###################

dt_loc <- dt_loc_raw %>%
  #mutate(date_time = ymd_hms(date_time)) %>%  #
  filter(!is.na(date_time)) %>% 
  arrange(individual_id, date_time) %>%
  group_by(individual_id) %>%
  mutate(
    start_date = min(date_time),
    end_date = max(date_time),
    start_year = year(start_date),
    end_year = year(end_date),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    duration_years = duration_days / 365.25,
    mean_interval_mins = ifelse(n() > 1, mean(diff(date_time), na.rm = TRUE) / dminutes(1), NA), 
    median_interval_mins = ifelse(n() > 1, median(diff(date_time), na.rm = TRUE) / dminutes(1), NA)) %>% 
  ungroup() %>% 
  as.data.table()

summary(dt_loc)
unique(dt_loc[start_year < 1990]$individual_id) # 1 - that's pretty unlikely to be true. 
unique(dt_loc[mean_interval_mins > 1440]$individual_id) # 
unique(dt_loc[median_interval_mins > 1440]$individual_id) # 


############################## Subset to > year & > 1 Observation per day ##########################


dt_loc_sub <- dt_loc %>% 
  filter(duration_days > 365 & median_interval_mins < 1440) %>% 
  filter(!start_year == 1909) %>% 
  filter(!(individual_id == "EF0215" & is.na(sex))) %>% 
  unique()

summary(dt_loc_sub)
n_distinct(dt_loc_sub$individual_id)

######################### Resample ########################## 

id_meta <- dt_loc_sub %>% 
  dplyr::select(individual_id, sex, population_id, park_id, source, start_date, 
                end_date, start_year, end_year, duration_days, duration_years, mean_interval_mins, 
                median_interval_mins) %>% 
  unique()

track <- make_track(dt_loc_sub %>% 
                      arrange(date_time),
                    .x = "lon", 
                    .y = "lat",
                    .t = "date_time",
                    individual_id = individual_id, 
                    crs = 4326)

track_resampled <- data.frame()
for(ind in unique(track$individual_id)){
  
  sub_track <- track %>% 
    filter(individual_id == ind)
  
  sub_track_res <- sub_track %>% 
    track_resample(rate = minutes(60), tolerance = minutes(15)) 
  
  
  track_resampled <- rbind(sub_track_res, track_resampled)
  
  rem <- nrow(sub_track) - nrow(sub_track_res)
  per <- round(rem/nrow(sub_track)*100, 1)
  
  print(paste0(ind, " done. Removed ", rem, " row (", per, "%)"))
}

dt_res <- track_resampled %>% 
  as.data.frame() %>% 
  dplyr::select(
    lon = x_, 
    lat = y_, 
    date_time = t_,
    individual_id
  ) %>% left_join(id_meta) %>% 
  arrange(individual_id, date_time) %>%
  group_by(individual_id) %>%
  mutate(
    start_date = min(date_time),
    end_date = max(date_time),
    start_year = year(start_date),
    end_year = year(end_date),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    duration_years = duration_days / 365.25,
    mean_interval_mins = ifelse(n() > 1, mean(diff(date_time), na.rm = TRUE) / dminutes(1), NA), 
    median_interval_mins = ifelse(n() > 1, median(diff(date_time), na.rm = TRUE) / dminutes(1), NA), 
    lag_time_diff_hours = as.numeric(difftime(date_time, lag(date_time), units = "hours")),
    lead_time_diff_hours = as.numeric(difftime(date_time, lead(date_time), units = "hours")),
    index = 1:n(),
    n = n()) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  filter(abs(lead_time_diff_hours) < 12.5, abs(lag_time_diff_hours) < 12.5) %>% 
  filter(n > 730)

n_distinct(dt_res$individual_id)

######################### Home Ranges  ########################## 
t_res <- make_track(dt_res %>% 
                      arrange(date_time),
                    .x = "lon", 
                    .y = "lat",
                    .t = "date_time",
                    individual_id = individual_id, 
                    crs = 4326)

hr_mcps <- data.frame()
hr_meta <- data.frame()
hr_locohs <- data.frame()
for(ind in unique(t_res$individual_id)){
  
  sub_track <- t_res %>% 
    filter(individual_id == ind) %>% 
    distinct(x_, y_)
  
  # Mininum convex polygon
  mcp <- hr_mcp(sub_track, levels = .95)
  mapview(mcp$mcp)
  
  mcp_trans <- st_transform(mcp$mcp, crs = "ESRI:54034") #https://epsg.io/54034
  
  # Local convex hull 
  sf_use_s2(FALSE)
  locoh <- hr_locoh(sub_track, levels = .95)
  sf_use_s2(TRUE)
  locoh_trans <- st_transform(locoh$locoh, crs = "ESRI:54034") #https://epsg.io/54034
  
  # get homerange areas
  hr_mcp_area_km2 <- as.numeric(st_area(st_make_valid(mcp_trans))/1000000)
  hr_locoh_area_km2 <- as.numeric(st_area(locoh_trans)/1000000)
  
  # get homerange diameter 
  mbc <- sf::st_minimum_bounding_circle(mcp_trans)
  circle_area_km2 <- as.numeric(st_area(mbc)/1000000)
  # Area = pi + r^2 ; r = sqrt(area/pi) --> diameter = r*2
  hr_diameter_km <- sqrt(circle_area_km2 / pi) * 2
  
  tmp_hr_meta <- data.frame(
    hr_mcp_area_km2 = hr_mcp_area_km2, 
    hr_locoh_area_km2 = hr_locoh_area_km2, 
    hr_diameter_km = hr_diameter_km, 
    individual_id = ind)
  
  hr_meta <- rbind(hr_meta, tmp_hr_meta)
    
  tmp_mcps <- mcp$mcp %>% 
    mutate(individual_id = ind)
  
  hr_mcps <- rbind(hr_mcps, tmp_mcps)
  
  tmp_locohs <- locoh$locoh %>% 
    mutate(individual_id = ind)
  
  hr_locohs <- rbind(hr_locohs, tmp_locohs)
  
  
  print(paste0(ind, " done. Home range diameter: km ", round(hr_diameter_km, 2)))
}

quantile(hr_meta$hr_diameter_km)
summary(hr_meta)
mean(hr_meta$hr_diameter_km)

######################### Park Association ########################## 


park_for_id <- data.frame()
for(ind in unique(hr_locohs$individual_id)){

  pol <- hr_locohs %>% filter(individual_id == ind)
  tmp <- assign_park(polygon = pol, pas = pas, ind = ind)  
  
  park_for_id <- rbind(tmp, park_for_id)
  print(ind)
}

summary(park_for_id)
sum(!is.na(park_for_id$park_id))

######################### Combine ########################## 

dt_final <- dt_res %>% 
  dplyr::select(-park_id) %>% 
  left_join(park_for_id[, -c("is_area_km2", "hr_area_km2")]) %>% 
  left_join(hr_meta)


fwrite(dt_final, "data/clean_data/all_location_data.csv")

######################### Vizualize ########################## 

pas_loc <- pas %>% 
  filter(NAME %in% unique(dt_final$park_id))

sf_loc <- st_as_sf(
  dt_final,
  coords = c("lon", "lat"), 
  crs = 4326
)

sf_loc %>% 
  sample_n(250000) %>% 
  ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  geom_sf(data = africa, fill = "grey95") +
  geom_sf(size = 0.1, aes(color = park_id), alpha = 0.5) +
  geom_sf(data = pas, alpha = 0.25, fill = "yellow") + 
  theme_minimal()


sf_use_s2(TRUE) #to be able to give distance in m

sf_loc %>% 
  sample_n(250000) %>% 
  ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  geom_sf(data = africa, fill = "grey95") +
  geom_sf(size = 0.1, aes(color = park_id), alpha = 0.5) +
  geom_sf(data = hr_locohs, alpha = 0.25, fill = "orange") + 
  geom_sf(data = pas, alpha = 0.25, fill = "yellow") + 
  theme_minimal()
mapview(hr_mcps)
nrow(dt_res[individual_id == "3991", ])
hr_meta[hr_meta$individual_id == "3991",]