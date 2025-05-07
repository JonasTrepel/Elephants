library(sf)
library(data.table)
library(tidyverse)

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
  #Phongolo Game Reserve


pas_sub <- pas_all[pas_all$NAME %in% c(names_v_count), ] %>% 
  filter(WDPA_PID != 555705347)
st_write(pas_sub, "data/spatial_data/protected_areas/park_boundaries.gpkg", append = FALSE)
mapview(pas_sub)

#not found: 
#Makuya NR
