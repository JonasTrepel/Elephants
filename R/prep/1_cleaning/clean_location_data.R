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
library(tidylog)

source("R/functions/assign_park.R")
source("R/functions/assign_cluster.R")


######## LOAD AND CLEAN DATA ########

# Park Boundaries 

pas <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg")


# Clusters
sf_clust <- st_read("data/spatial_data/protected_areas/pa_clusters.gpkg") %>% 
  st_transform(crs = "ESRI:54009")


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
                            "equal", "zeros"),
                  capitals_rad = 10000) #radius around capitol in m


dt_ceru <- dt_ceru_raw %>%
  filter(.summary == TRUE) %>% #select only clean coords
  dplyr::select(-c(".summary", ".val", ".equ", ".zer", ".cap", ".cen")) 

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
                              "equal", "zeros"),
                    capitals_rad = 10000) #radius around capitol in m


dt_hwange <- dt_hwange_raw %>%
  filter(.summary == TRUE) %>% #select only clean coords
  dplyr::select(-c(".summary", ".val", ".equ", ".zer", ".cap", ".cen")) %>% 
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
                              "equal", "zeros"),
                    capitals_rad = 10000) #radius around capitol in m


dt_kaingo <- dt_kaingo_raw %>%
  filter(.summary == TRUE) %>% #select only clean coords
  dplyr::select(-c(".summary", ".val", ".equ", ".zer", ".cap", ".cen")) %>% 
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
unzip("data/raw_data/kaingo/Peanuts (2022-05-13 to 2025-05-27).kmz", exdir = "data/raw_data/kaingo")


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


# Lapalala ---------------------------------------

lapalala_files <- list.files(path = "data/raw_data/lapalala/AWT Elephant collar data", 
                             pattern = ".csv", 
                             full.names = T)

dt_lapalala_raw <- data.frame()
for(i in 1:length(lapalala_files)){
  
  l_tmp <- fread(lapalala_files[i])
  dt_lapalala_raw <- rbind(dt_lapalala_raw, l_tmp)
}


glimpse(dt_lapalala_raw)

sf_lapalala_bound <- st_read("data/raw_data/lapalala/lapalala_boundary.gpkg") %>% 
  st_zm(., drop = TRUE, what = "ZM") %>% 
  st_make_valid()
st_bbox(sf_lapalala_bound)

dt_lapalala <- dt_lapalala_raw %>% 
  dplyr::select(
    individual_id = Tag, 
    lon = Longitude, 
    lat = Latitude, 
    date_time = `Time Stamp`, 
  ) %>% 
  mutate(population_id = NA,
         lon = gsub("°", "", lon), 
         lon = as.numeric(lon),
         lat = gsub("°", "", lat),
         lat = as.numeric(lat),
         date_time = as_datetime(date_time),
         park_id = "Lapalala", 
         source = "Lapalala", 
         sex = "U", 
         individual_id = gsub("African Elephant: ", "", individual_id), 
         individual_id = gsub("African Elephant ", "", individual_id), 
         individual_id = gsub("EF1: ", "", individual_id) 
  ) %>% 
  filter(lon > 28.16774 & lon < 28.43139 &
           lat > -23.94056 & lat < 23.73957)
unique(dt_lapalala$individual_id)
summary(dt_lapalala)

sf_lapalala <- st_as_sf(dt_lapalala, 
                       coords = c("lon", "lat"), 
                       crs = 4326)

mapview(sf_lapalala, zcol = "individual_id")


# HiP ----------------------------------------

dt_hip_raw <- fread("data/raw_data/hip/cnrs_gps_elephant_hip.csv") %>% 
  dplyr::select(
    individual_id = `Collar ID`, 
    lat = `Latitude [deg]`, 
    lon = `Longitude [deg]`, 
    date_time = `Acq. Time [UTC]`, 
  ) %>% 
  mutate(population_id = NA, 
         date_time = dmy_hm(date_time), 
         park_id = "HiP", 
         source = "SCJ_EKZNW", 
         sex = "F") %>% #All females, in distinct herds. 
  filter(!abs(lat) > 90 & !abs(lon) > 180) %>% 
  mutate(point_id = 1:nrow(.))

sf_hip <- st_as_sf(dt_hip_raw, 
                        coords = c("lon", "lat"), 
                        crs = 4326)

sf_hip_bound <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  filter(grepl("Hluhluwe", NAME)) %>% 
  st_transform(crs = 4326)

sf_hip_int <- sf_hip %>% 
  filter(lengths(st_intersects(., sf_hip_bound)) > 0)

mapview(sf_hip_bound)
mapview(sf_hip_int, zcol = "individual_id")

dt_hip <- dt_hip_raw %>% 
  filter(point_id %in% unique(sf_hip_int$point_id)) %>% 
  dplyr::select(-point_id) %>% 
  mutate(individual_id = paste0("hip_", individual_id))


# Ithala --------------------------------------------

dt_ithala_raw <- fread("data/raw_data/ithala/ithala_elephant_gps_2014_2023.csv") %>% 
  dplyr::select(
    individual_id = Tag, 
    lat = Latitude, 
    lon = Longitude, 
    date_time = Time, 
    sex = Sex
  ) %>% 
  mutate(population_id = NA, 
         park_id = "ithala", 
         source = "EKZNW", 
         sex = case_when(
           .default = "U",
           sex == "Cow" ~ "F", 
           sex == "Bull" ~ "M")) %>% #All females, in distinct herds. 
  filter(!abs(lat) > 90 & !abs(lon) > 180) %>% 
  mutate(point_id = 1:nrow(.), 
         date_time = dmy_hm(date_time))

sf_ithala <- st_as_sf(dt_ithala_raw, 
                   coords = c("lon", "lat"), 
                   crs = 4326)
#mapview(sf_ithala)

sf_ithala_bound <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  filter(grepl("Itala", NAME)) %>% 
  st_transform(crs = 4326)

sf_ithala_int <- sf_ithala %>% 
  filter(lengths(st_intersects(., sf_ithala_bound)) > 0)

mapview(sf_ithala_bound)
#mapview(sf_ithala_int, zcol = "individual_id")

dt_ithala <- dt_ithala_raw %>% 
  filter(point_id %in% unique(sf_ithala_int$point_id)) %>% 
  dplyr::select(-point_id) #%>%  mutate(date_time = parse_date_time(date_time, orders = "dmy HM"))


# Elephants alive --------------------------------------------

dt_ea_raw <- fread("data/raw_data/elephants_alive/elephants_alive_tracking_data.csv") %>% 
  dplyr::select(
    individual_id = name, 
    date_time = fixtime , 
    sex = sex, 
    geometry,
  ) %>% 
  mutate(population_id = NA, 
         park_id = NA, 
         source = "EA", 
         sex = case_when(
           .default = "U",
           sex == "female" ~ "F", 
           sex == "male" ~ "M"),
         date_time = as_datetime(date_time))

sf_ea_raw <- st_as_sf(
  dt_ea_raw,
  wkt = "geometry",  
  crs = 4326          
)

dt_ea <- sf_ea_raw %>% 
  mutate(lon = st_coordinates(sf_ea_raw)[, 1], 
         lat = st_coordinates(sf_ea_raw)[, 2]) %>% 
  filter(!lat < -35 & !lat > 0 & !lon > 45 & !lon < 0) %>% 
  as.data.frame() %>% 
  mutate(geometry = NULL) %>% 
  clean_coordinates(., 
                    lon = "lon", 
                    lat = "lat", 
                    species = "individual_id", 
                    tests = c("capitals", "centroids",
                              "equal", "zeros"),
                    capitals_rad = 10000) %>%
  filter(.summary == TRUE) %>% #select only clean coords
  dplyr::select(-c(".summary", ".val", ".equ", ".zer", ".cap", ".cen")) 

setDT(dt_ea)
summary(dt_ea)

dt_ea %>% 
  ggplot() +
  geom_point(aes(x = lon, y = lat), size = 0.1, alpha = 0.2) +
  facet_wrap(~individual_id, scales = "free") +
  theme_bw()


####### CLEAN DATASET ########


# Combine for the first time ---------------------------------------


dt_loc_raw <- rbind(
  dt_ceru, 
  dt_hwange , 
  dt_peanuts , 
  dt_lapalala, 
  dt_hip, 
  dt_ithala, 
  dt_ea
) %>% 
  mutate(obs_id = paste0("point_", 1:nrow(.))) %>% 
  filter(!is.na(date_time)) %>% 
  arrange(individual_id, date_time) %>%
  group_by(individual_id) %>%
  mutate(
    n_obs = n(), 
    start_date = min(date_time),
    end_date = max(date_time),
    start_year = year(start_date),
    end_year = year(end_date),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    duration_years = duration_days / 365.25) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  filter(n_obs > 365 & duration_years > 1)
  
n_distinct(dt_loc_raw$individual_id)
summary(dt_loc_raw)

# Remove points in unrealistic areas --------

sf_loc_raw <- dt_loc_raw %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(crs = "ESRI:54009") %>% 
  left_join(dt_loc_raw[, c("obs_id", "lon", "lat")])


#forbidden places 

#these are places in built up areas with an accumulation of location points
#likely that this is where some of the collars were stored. 

#pretoria 
sf_gaut <- data.frame(
  lon = c(28.18989, 28.04183), 
  lat = c(-25.74733, -26.20581), 
  name = c("pretoria", "joburg")) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(crs = "ESRI:54009") %>% 
  st_buffer(50000)

sf_fp <- data.frame(
  lon = c(31.9098441051575, 25.8222879691359, 
          25.2242191536082, 23.4576669792581), 
  lat = c(-21.2720049602878,-17.9290044076166,
          -17.7971340422376, -19.9697006721614), 
  name = c("chipinda_pools", "victoria_falls",
           "kazungala", "maun")) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(crs = "ESRI:54009") %>% 
  st_buffer(2500) %>% 
  rbind(sf_gaut)

#remove points in suspicious areas
africa_moll <- st_transform(africa, crs = "ESRI:54009")

discard_0 <- sf_loc_raw %>% 
  filter(!lengths(st_intersects(., africa_moll)) > 0) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()


discard_1 <- sf_loc_raw %>% 
  filter(lengths(st_intersects(., sf_fp)) > 0) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()
  
sf_loc_2 <- sf_loc_raw %>% 
  filter(!obs_id %in% discard_0) %>% 
  filter(!obs_id %in% discard_1)


# Remove spatial outliers ----------------------------------- 
discard_2 <- c()
sf_loc_2.5 <- sf_loc_2 %>% 
  filter(individual_id == "nope")
i <- 0
max_speed_kmh = 25 # 25 kmh https://doi.org/10.1242/jeb.02443
max_dist_km = 50

table(sf_loc_2[sf_loc_2$source == "EA", ]$individual_id)
#id = "0F08"
for(id in unique(sf_loc_2$individual_id)){ 
  
  # get in ordere
  sf_loc_sub <- sf_loc_2 %>% 
    filter(individual_id == id) %>% 
    arrange(date_time)
  
  # get distance, time and speed between consecutive ponits
  dist_prev <- as.numeric(st_distance(sf_loc_sub, lag(sf_loc_sub), by_element = TRUE)/1000) #in km
  dist_next <- as.numeric(st_distance(sf_loc_sub, lead(sf_loc_sub), by_element = TRUE)/1000)
  
  time_prev <- as.numeric(difftime(sf_loc_sub$date_time, lag(sf_loc_sub$date_time), units = "hours"))
  time_next <- as.numeric(difftime(lead(sf_loc_sub$date_time), sf_loc_sub$date_time, units = "hours"))
  
  kmh_prev <- dist_prev/time_prev
  kmh_next <- dist_next/time_next
  
  sf_loc_sub <- sf_loc_sub %>%
    mutate(
      dist_prev = dist_prev,
      dist_next = dist_next,
      time_prev = time_prev,
      time_next = time_next,
      kmh_prev = kmh_prev,
      kmh_next = kmh_next )
  
  too_far_away <- sf_loc_sub %>% 
    mutate(discard = (kmh_next > max_speed_kmh | kmh_prev > max_speed_kmh) |
             (dist_prev > max_dist_km | dist_next > max_dist_km)) %>% 
    filter(discard) %>% 
    pull(obs_id)
  
  
  sf_loc_sub <- sf_loc_sub %>% 
    mutate(discard = ifelse((kmh_next > max_speed_kmh | kmh_prev > max_speed_kmh) |
                              (dist_prev > max_dist_km | dist_next > max_dist_km), 
                            TRUE, FALSE))
  
  discard_2 <- c(discard_2, too_far_away)
  
  sf_loc_2.5 <- rbind(sf_loc_sub, sf_loc_2.5)
  
  i = i+1
  print(paste0(id, " done (",
               i, " of ", n_distinct(sf_loc_2$individual_id), 
               " (", Sys.time(),")"))
}


summary(sf_loc_2.5 %>% filter(source == "EA"))


sf_loc_3 <- sf_loc_2.5 %>% 
  mutate(discard = ifelse(is.na(discard), FALSE, discard)) %>% 
  filter(!discard == TRUE)

nrow(sf_loc_2.5) - nrow(sf_loc_3) #12521 

sf_loc_3 %>% sample_n(500000) %>% ggplot + geom_sf(size = 0.1) +
  geom_sf(data = sf_gaut, alpha = 0.1, color = "orange")


# Identify suspicious elephants ------------------------ 
# e.g., elephants having the majority of their points in a small area 

sus_ids <- c()
plot_list <- list()
i <- 0

for(id in unique(sf_loc_3$individual_id)){
  
  print(paste0("starting with: ",  id))
  
  
  sf_loc_sub <- sf_loc_3 %>% 
    filter(individual_id == id)
 
  if(nrow(sf_loc_sub) < 10){next}
    
  grid <- st_make_grid(sf_loc_sub, cellsize = 1000, square = TRUE) %>% 
    st_as_sf() %>% 
    mutate(n_obs = lengths(st_intersects(., sf_loc_sub)), 
           rel_obs = n_obs/nrow(sf_loc_sub)) %>% 
    filter(n_obs > 0)
  
  #print(mapview(grid, zcol = "rel_obs"))
  
  title_string <- paste0(id, " Source: ", unique(sf_loc_sub$source))
  
  p <- ggplot(grid) +
    geom_sf(aes(color = rel_obs, fill = rel_obs)) +
    labs(title = title_string) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    theme_minimal()
  

  if(nrow(grid[grid$rel_obs > 0.25, ]) > 0){
  sus_ids <- c(sus_ids, id)
  
  p <- p + labs(title = title_string, subtitle = "Strange")

  } 
  
  print(p)
  
  plot_list[[id]] <- p
  
  
  i = i+1
  print(paste0(id, " done (",
               i, " of ", n_distinct(sf_loc_3$individual_id), 
               " (", Sys.time(),")"))
  
}

pdf("builds/plots/exploratory/suspicious_individuals_plots.pdf", width = 8, height = 6)
for (id in unique(sf_loc_3$individual_id)) {
  print(plot_list[[id]])
}

dev.off()


ids_to_discard <- c("3438", #very unrealistic and high concentration in one cell (> 80 %)
                    "3439", #very unrealistic and high concentration in one cell (> 80 %)
                    "3440", #very unrealistic and high concentration in one cell (> 50 %)
                    "5433", #dude was hanging out in some weird enclosuree 
                    "5434" #dude was hanging out in some weird enclosure
)

#t <- sf_loc_3 %>% filter(individual_id == "1434252")
#mapview(t)

discard_3 <- sf_loc_3 %>% 
  filter((individual_id == "1434261" & lon > 25)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_4 <- sf_loc_3 %>% 
  filter((individual_id == "1442046" & lat < - 18.5)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_5 <- sf_loc_3 %>% 
  filter((individual_id == "1442047" & lat < - 19.5)) %>% as.data.frame() %>%  mutate(x = NULL, geom = NULL, geometry = NULL) %>% dplyr::select(obs_id) %>% pull()

discard_6 <- sf_loc_3 %>% 
  filter((individual_id == "1442053" & lat < - 19.5)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_7 <- sf_loc_3 %>% 
  filter((individual_id == "1442057" & lon < 23.75)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_8 <- sf_loc_3 %>% 
  filter((individual_id %in% c("3959", "3961", 
                               "3962", "3963", "3964",
                               "3965", "3966", 
                               "3967", "3968", 
                               "3969", "3970", 
                               "3971", "3972", 
                               "3973", "3991",
                               "EM0209", "EM0212"
                               ) & lat < -22)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_9 <- sf_loc_3 %>% 
  filter((individual_id %in% c("3971", "3972") & lat > -19.1)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_10 <- sf_loc_3 %>% 
  filter((individual_id == "3991" & lon > 25)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_11 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EF0013", "EF0012", "EF0017", "EF0018", "EM0014") & lat < -27)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_12 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EF0017", "EF0018") & lat > -26.8)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_13 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EF0208", "EF0244", "EF0251") & lat < -24.8)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_14 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EM0035") & lon > 23.2)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_15 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EM0188") & lon < 26)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_16 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EM0208") & lon < 32)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()


discard_17 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EM0212") & lat < -21.75 )) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()


discard_18 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EM216") & lat > -21.3 )) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_19 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EM0159") & lat > -17.6 )) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_20 <- sf_loc_3 %>% 
  filter((individual_id %in% c("1434252") & between(lat, -19.5, -19))) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_21 <- sf_loc_3 %>% 
  filter((individual_id %in% c("EM0251") & lat < -24.6 )) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()


discard_22 <- sf_loc_3 %>% 
  filter((individual_id %in% c("1434239") & lat < -19 )) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_23 <- sf_loc_3 %>% 
  filter((individual_id %in% c("1434200") & lat < -19.2 )) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

discard_24 <- sf_loc_3 %>% 
  filter((individual_id %in% c("1401602") & lon < 25 )) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()


rename_1434252_A <- sf_loc_3 %>% 
  filter((individual_id %in% c("1434252") &  lat > -19)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()

rename_1434252_B <- sf_loc_3 %>% 
  filter((individual_id %in% c("1434252") &  lat < -19.5)) %>% 
  as.data.frame() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  dplyr::select(obs_id) %>% 
  pull()



sf_loc_4 <- sf_loc_3 %>% 
  filter(!individual_id %in% ids_to_discard) %>% 
  filter(!obs_id %in% c(
    discard_3, 
    discard_4, 
    discard_5, 
    discard_6, 
    discard_7, 
    discard_8, 
    discard_9, 
    discard_10, 
    discard_11, 
    discard_12, 
    discard_13, 
    discard_14, 
    discard_15, 
    discard_16, 
    discard_17, 
    discard_18, 
    discard_19, 
    discard_20, 
    discard_21, 
    discard_22, 
    discard_23,
    discard_24)) %>% 
  mutate(individual_id = ifelse(obs_id %in% rename_1434252_A, paste0(individual_id, "_A"), individual_id), 
         individual_id = ifelse(obs_id %in% rename_1434252_B, paste0(individual_id, "_B"), individual_id))

nrow(sf_loc_3) - nrow(sf_loc_4)
################## Get Duration, Start and End of Tracking and mean relocation interval ###################

dt_loc <- dt_loc_raw %>%
  filter(obs_id %in% unique(sf_loc_4$obs_id)) %>%
  #mutate(date_time = ymd_hms(date_time)) %>%  #
  filter(!is.na(date_time)) %>% 
  arrange(individual_id, date_time) %>%
  group_by(individual_id) %>%
  mutate(
    n_obs = n(), 
    start_date = min(date_time),
    end_date = max(date_time),
    start_year = year(start_date),
    end_year = year(end_date),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    duration_years = duration_days / 365.25,
    interval_hrs = as.numeric(difftime(date_time, lag(date_time), units = "hours")), 
    mean_interval_hrs = mean(interval_hrs, na.rm = T), 
    median_interval_hrs = median(interval_hrs, na.rm = T)
    #mean_interval_mins = ifelse(n() > 1, mean(diff(date_time), na.rm = TRUE) / dminutes(1), NA), 
    #median_interval_mins = ifelse(n() > 1, median(diff(date_time), na.rm = TRUE) / dminutes(1), NA)
    ) %>% 
  ungroup() %>% 
  mutate(individual_id = ifelse(obs_id %in% rename_1434252_A, paste0(individual_id, "_A"), individual_id), 
         individual_id = ifelse(obs_id %in% rename_1434252_B, paste0(individual_id, "_B"), individual_id)) %>% 
  as.data.table()

summary(dt_loc)
unique(dt_loc$individual_id); n_distinct(dt_loc$individual_id)
unique(dt_loc[mean_interval_hrs > 24]$individual_id) # 
unique(dt_loc[median_interval_hrs > 24]$individual_id) # 



############################## Subset to > year & > 1 Observation per day ##########################


dt_loc_sub <- dt_loc %>% 
  filter(duration_days > 365 & median_interval_hrs < 24) %>% #720 min = 12 +/- 1 std 
  filter(!(individual_id == "EF0215" & is.na(sex))) %>% 
  unique()

summary(dt_loc_sub)

n_distinct(dt_loc_sub$individual_id)

######################### Home Ranges  ########################## 

track <- make_track(dt_loc_sub %>% 
                      arrange(date_time),
                    .x = "lon", 
                    .y = "lat",
                    .t = "date_time",
                    individual_id = individual_id, 
                    crs = 4326)

hr_mcps <- data.frame()
hr_meta <- data.frame()
hr_locohs <- data.frame()

for(ind in unique(track$individual_id)){
  
  sub_track <- track %>% 
    filter(individual_id == ind) %>% 
    distinct(x_, y_)
  
  # Mininum convex polygon
  mcp <- hr_mcp(sub_track, levels = .95)
  mapview(mcp$mcp)
  
  mcp_trans <- st_transform(mcp$mcp, crs = "ESRI:54009") 
  
  # Local convex hull 
  sf_use_s2(FALSE)
  locoh <- hr_locoh(sub_track, levels = .95)
  sf_use_s2(TRUE)
  locoh_trans <- st_transform(locoh$locoh, crs = "ESRI:54009")
  
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
  print(paste0(ind, " MCP size km2: ", round(hr_mcp_area_km2, 2)))
  
}

st_write(hr_mcps, "data/spatial_data/elephants/mcp_home_ranges.gpkg", append = FALSE)
st_write(hr_locohs, "data/spatial_data/elephants/locohs_home_ranges.gpkg", append = FALSE)

quantile(hr_meta$hr_diameter_km)
quantile(hr_meta$hr_mcp_area_km2 , seq(0, 1, 0.05))
library(units)
hr_mcps$area_km2 <- drop_units(hr_mcps$area)/1000000
big_boys <- hr_mcps[hr_mcps$area_km2 > 10000, ]

mapview(big_boys)

bb_hr_sizes <- big_boys %>% 
  as.data.frame() %>% 
  mutate(geom = NULL, geometry = NULL) %>% 
  select(area_km2, individual_id) %>% 
  unique()

dt_loc_sub %>% 
  filter(individual_id %in% unique(big_boys$individual_id)) %>% 
  left_join(bb_hr_sizes) %>% 
  mutate(individual_id = forcats::fct_reorder(individual_id, duration_years, .desc = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x=lon,y=lat, color = area_km2), size = 0.1) +
  scale_color_viridis_c(option = "B") +
  facet_wrap(~individual_id, scales = "free")

#seem to be mostly fine


######################### Park & Cluster Association ########################## 


park_for_id <- data.frame()
i = 0
for(ind in unique(hr_locohs$individual_id)){
  
  pol <- hr_locohs %>% filter(individual_id == ind) %>% 
    st_transform(crs = "ESRI:54009") %>% 
    st_make_valid()
  tmp <- assign_park(polygon = pol, pas = pas , ind = ind)  
  
  park_for_id <- rbind(tmp, park_for_id)
 i = i+1
 print(paste0(ind, " done. Associated park is: ", unique(tmp$park_id), 
              " (", i, " of ", n_distinct(hr_locohs$individual_id), ")"))
  
}

summary(park_for_id)
sum(!is.na(park_for_id$park_id))

#cluster ID
cluster_for_id <- data.frame()
i = 0
for(ind in unique(hr_locohs$individual_id)){
  
  pol <- hr_locohs %>% filter(individual_id == ind) %>% 
    st_transform(crs = "ESRI:54009") %>% 
    st_make_valid()
  
  tmp <- assign_cluster(polygon = pol, pas = sf_clust , ind = ind)  
  
  cluster_for_id <- rbind(tmp, cluster_for_id)
  i = i+1
  print(paste0(ind, " done. Associated cluster is: ", unique(tmp$cluster_id), 
               " (", i, " of ", n_distinct(hr_locohs$individual_id), ")"))
  
}



######################### Combine ########################## 

dt_final <- dt_loc_sub %>% 
  dplyr::select(-park_id) %>% 
  left_join(park_for_id[, -c("is_area_km2", "hr_area_km2")]) %>% 
  left_join(cluster_for_id[, c("individual_id", "cluster_id")]) %>% 
  left_join(hr_meta) %>% 
  unique()

fwrite(dt_final, "data/processed_data/clean_data/all_location_data.csv")

n_distinct(dt_final$individual_id)
dt_final %>% sample_n(500000) %>% ggplot() +geom_point(aes(x = lon, y = lat), size = 0.1, alpha = 0.5)

glimpse(dt_final)

dt_meta <- dt_final %>% 
  select(individual_id, 
         sex, 
         source, 
         n_obs,
         start_date, 
         end_date, 
         duration_days, 
         duration_years,
         mean_interval_hrs, 
         median_interval_hrs, 
         park_id, 
         wdpa_pid,
         cluster_id,
         hr_mcp_area_km2, 
         hr_locoh_area_km2, 
         hr_diameter_km) %>% 
  unique()
table(dt_meta$source)
n_distinct(dt_meta$individual_id)

fwrite(dt_meta, "data/processed_data/clean_data/elephant_id_meta_data.csv")

table(dt_meta$park_id)

# 
# ######################### Resample ########################## 
# 
# id_meta <- dt_loc_sub %>% 
#   dplyr::select(individual_id, sex, population_id, park_id, source, start_date, 
#                 end_date, start_year, end_year, duration_days, duration_years, mean_interval_mins, 
#                 median_interval_mins) %>% 
#   unique()
# 
# track <- make_track(dt_loc_sub %>% 
#                       arrange(date_time),
#                     .x = "lon", 
#                     .y = "lat",
#                     .t = "date_time",
#                     individual_id = individual_id, 
#                     crs = 4326)
# 
# track_resampled <- data.frame()
# for(ind in unique(track$individual_id)){
#   
#   sub_track <- track %>% 
#     filter(individual_id == ind)
#   
#   sub_track_res <- sub_track %>% 
#     track_resample(rate = hours(12), tolerance = minutes(60)) 
#   
#   
#   track_resampled <- rbind(sub_track_res, track_resampled)
#   
#   rem <- nrow(sub_track) - nrow(sub_track_res)
#   per <- round(rem/nrow(sub_track)*100, 1)
#   
#   print(paste0(ind, " done. Removed ", rem, " row (", per, "%)"))
# }
# 
# dtrack <- track_resampled %>% 
#   as.data.frame() %>% 
#   dplyr::select(
#     lon = x_, 
#     lat = y_, 
#     date_time = t_,
#     individual_id
#   ) %>% left_join(id_meta) %>% 
#   arrange(individual_id, date_time) %>%
#   group_by(individual_id) %>%
#   mutate(
#     start_date = min(date_time),
#     end_date = max(date_time),
#     start_year = year(start_date),
#     end_year = year(end_date),
#     duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
#     duration_years = duration_days / 365.25,
#     mean_interval_mins = ifelse(n() > 1, mean(diff(date_time), na.rm = TRUE) / dminutes(1), NA), 
#     median_interval_mins = ifelse(n() > 1, median(diff(date_time), na.rm = TRUE) / dminutes(1), NA), 
#     n = n()) %>% 
#   ungroup() %>% 
#   filter(n > 730) %>% 
#   as.data.table()
# 
# n_distinct(dtrack$individual_id)
# summary(dtrack)

