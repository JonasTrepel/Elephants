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

#lapalala
sf_lapalala <- st_read("data/raw_data/lapalala/lapalala_boundary.gpkg") %>% 
  st_zm(., drop = TRUE, what = "ZM") %>% 
  st_make_valid()
#mapview(pas_all[grepl("Lapalala", pas_all$NAME), ])

pas_all[grepl("Lapalala", pas_all$NAME), ]$geometry <- sf_lapalala$geom

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

#mapview::mapview(pas_all[grepl("Thornybush", pas_all$NAME), ])

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
                  # "Addo Elephant National Park", 
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
                   "Zambezi", 
                   "Luambe", 
                   "Moremi",
                   "Itala Nature Reserve",
                   "Bwabwata", 
                   "Mapungupwe National Park",
                   "Mana Pools National Park",
                   "iSimangaliso Wetland Park",
                   "Balule Nature Reserve",
                   "Limpopo",
                   "Luengue-Luiana National Park",
                   "Mavinga National Park", #CERU END
                   "Kaingo Private Game Reserve", 
                   "Lapalala Nature Reserve",
                   "Thornybush Nature Reserve"
                  
                   
                   )

unique(dt_pc$park_id)
pas_all[grepl("Mavinga", pas_all$NAME), ] 

#not found: 
  #Makuya NR
  #uMkhuze - could be part of iSimangaliso
  #Venetia Limpopo
  #Phongolo Game Reserve --> not formerly protected

sf_lapalala <- st_read("data/raw_data/lapalala/lapalala_boundary.gpkg") %>% 
  st_zm(., drop = TRUE, what = "ZM") %>% 
  st_make_valid()
#mapview::mapview(pas_all[grepl("Lapalala", pas_all$NAME), ])

pas_all[grepl("Lapalala", pas_all$NAME), ]$geometry <- sf_lapalala$geom
#mapview::mapview(pas_all[grepl("Lapalala", pas_all$NAME), ])



pas_sub <- pas_all[pas_all$NAME %in% c(names_v_count), ] %>% 
  filter(WDPA_PID != 555705347) %>% 
  filter(!DESIG_ENG %in% c("Forest Reserve", "State Forest")) %>%
  st_transform(crs = "ESRI:54009") %>% 
  mutate(area_km2 = as.numeric(st_area(.)/1000000))
table(pas_sub$NAME)
unique(pas_sub[pas_sub$NAME == "Kruger National Park" , c("NAME", "area_km2")])

st_write(pas_sub, "data/spatial_data/protected_areas/park_boundaries.gpkg", append = FALSE)
mapview(pas_sub)
mapview(pas_sub[grepl("ZAF", pas_sub$ISO3), ])

# Rasterize PAs-----------
library(sf)
library(data.table)
library(tidyverse)
library(terra)
library(exactextractr)



### get raster of pa maps 
pas_trans <- st_transform(pas_sub, crs = "ESRI:54009") %>% 
  mutate(WDPA_PID = as.numeric(WDPA_PID))
temp_r_id <- rast(extent = ext(pas_trans), resolution = 250, crs = "ESRI:54009")
n_distinct(cells(temp_r_id))


pa_ids_r <- rasterize_polygons(pas_trans,
                               temp_r_id)

pa_ids_r[] <- pas_trans$WDPA_PID[pa_ids_r[]]

plot(pa_ids_r)

writeRaster(pa_ids_r, "data/spatial_data/protected_areas/pa_id_raster.tif", overwrite = TRUE)

