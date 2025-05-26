library(sf)
library(data.table)
library(tidyverse)
library(mapview)

### PARKS WITH LOCATION POINTS -------------------------------

# Location points
dt_lp <- fread("data/processed_data/clean_data/all_location_data.csv")

sf_lp <- st_as_sf(dt_lp, 
                  coords = c("lon", "lat"), 
                  crs = 4326) %>% st_transform(crs = "ESRI:54009")


# Load all PAs 
files <- list.files("data/spatial_data/protected_areas/wdpa_raw", pattern = "polygons.shp", recursive = T, full.names = T)

pas_all <- data.frame()
for(i in 1:length(files)){
  
  tmp <- st_read(files[i])
  
  pas_all <- rbind(pas_all, tmp)
  
}

sf_pas_moll <- pas_all %>% st_transform(crs = "ESRI:54009")

sf_pas_int <- sf_pas_moll %>% 
  filter(lengths(st_intersects(., sf_lp)) > 0) %>% 
  dplyr::select(NAME, WDPA_PID, WDPAID, DESIG_ENG, IUCN_CAT) %>% 
  filter(!DESIG_ENG == "Community Forest" &
           !DESIG_ENG == "Forest Reserve" &
           !grepl("Ramsar", DESIG_ENG)) %>% 
  filter(!WDPA_PID %in% c("301767",
                          "26944",
                          "9035_B",
                          "555705347", 
                          "555571049", 
                          "555563884", 
                          "95356", 
                          "555555542", 
                          "555555541", 
                          "145516", 
                          "555624128", 
                          "7449", 
                          "555766093", 
                          "20399", 
                          "10907", 
                          "555570576"))

mapview(sf_pas_int)

st_write(sf_pas_int,
         "data/spatial_data/protected_areas/pas_intersecting_with_locations_data.gpkg", 
         append = FALSE)



### PARKS WITH POPULATION COUNTS -------------------------------

# Population counts 

dt_pc <- fread("data/processed_data/clean_data/all_population_counts.csv")

# Load all PAs 
files <- list.files("data/spatial_data/protected_areas/wdpa_raw", pattern = "polygons.shp", recursive = T, full.names = T)

pas_all <- data.frame()
for(i in 1:length(files)){
  
  tmp <- st_read(files[i])
  
  pas_all <- rbind(pas_all, tmp)
  
}

mapview(pas_all[grepl("Pongola Nature Reserve", pas_all$NAME), ])

names_v_count <- c("North Luangwa",
                   "South Luangwa",
                   "Kasungu National Park",
                   "Nkasa Rupara",
                   "Hwange", 
                   "Chizarira",
                   "Matusadona",
                   "Kruger National Park",
                   "Klaserie Private Nature Reserve", 
                   "Timbavati Private Nature Reserve", 
                   "Tembe Elephant Park",
                   "Hluhluwe – iMfolozi Park", 
                   "Addo Elephant National Park", 
                   "Gonarezhou",
                   "Maputo", #exclude WDPA_PID: 555705347
                   "Khaudum", 
                   "Chobe",
                   "Pilanesberg National Park", 
                   "Letaba Ranch Nature Reserve",
                   "Manyeleti Nature Reserve", 
                   "Sabie Sands Private Nature Reserve", 
                   "Kafue",
                   "Lower Zambezi",
                   "Sioma Ngwezi", 
                   "Northern Tuli", 
                   "Lukusuzi",
                   "Umbabat Private Nature Reserve",
                   "Madikwe Nature Reserve", 
                   "Makgadikgadi Pans", 
                   "Nxai Pan",
                   "Victoria Falls", 
                   "Luambe", 
                   "Moremi",
                   "Itala Nature Reserve",
                   "Bwabwata", 
                   "Mapungupwe National Park",
                   "Mana Pools",
                   "iSimangaliso Wetland Park",
                   "Balule Nature Reserve",
                   "Limpopo",
                   "Luengue-Luiana National Park",
                   "Mavinga National Park", #CERU END
                   "Kaingo Private Game Reserve"
                   
                   )

unique(dt_pc$park_id)
pas_all[grepl("Mavinga", pas_all$NAME), ] 

#not found: 
  #Makuya NR
  #uMkhuze - could be part of iSimangaliso
  #Venetia Limpopo
  #Phongolo Game Reserve --> not formerly protected


pas_sub <- pas_all[pas_all$NAME %in% c(names_v_count), ] %>% 
  filter(WDPA_PID != 555705347)
st_write(pas_sub, "data/spatial_data/protected_areas/park_boundaries.gpkg", append = FALSE)
mapview(pas_sub)
mapview(pas_sub[grepl("ZAF", pas_sub$ISO3), ])



## create buffer of median elephant homerange diameter 

dt_loc <- fread("data/processed_data/clean_data/all_location_data.csv") 

dt_loc_nsa <- dt_loc %>% filter(!wdpa_pid %in% pas_all[pas_all$ISO3 == "ZAF", ]$WDPA_PID)
dt_loc_sa <- dt_loc %>% filter(wdpa_pid %in% pas_all[pas_all$ISO3 == "ZAF", ]$WDPA_PID)

unique(dt_loc_nsa$park_id)



dt_hr <- dt_loc_nsa %>% 
  dplyr::select(individual_id, hr_diameter_km, hr_mcp_area_km2) %>% 
  unique() 

dt_hr_sa <- dt_loc_sa %>% 
  dplyr::select(individual_id, hr_diameter_km, hr_mcp_area_km2) %>% 
  unique() 


hist(dt_hr$hr_diameter_km)

mean(dt_hr$hr_diameter_km, na.rm = T) # 87.35162
median(dt_hr$hr_diameter_km, na.rm = T) # 72.53294

mean(dt_hr_sa$hr_diameter_km, na.rm = T) # 50.76994
median(dt_hr_sa$hr_diameter_km, na.rm = T) # 47.48037

mean(dt_hr$hr_mcp_area_km2, na.rm = T) # 3931.655
quantile(dt_hr$hr_mcp_area_km2, na.rm = T) # 2168.6491

mean(dt_hr_sa$hr_mcp_area_km2, na.rm = T) # 1152.775
quantile(dt_hr_sa$hr_mcp_area_km2, na.rm = T) # 1169.70026

pas_buff_nsa <- pas_sub %>% 
  filter(!ISO3 == "ZAF") %>% 
  st_transform(crs = "ESRI:54009") %>% 
  st_buffer(dist = mean(dt_loc$hr_diameter_km, na.rm = T)) #65

pas_sa <- pas_sub %>% 
  filter(ISO3 == "ZAF") %>% 
  st_transform(crs = "ESRI:54009")

pas_buff <- rbind(pas_buff_nsa,pas_sa)

st_write(pas_buff, "data/spatial_data/protected_areas/park_boundaries_buffer_for_nsa.gpkg")
