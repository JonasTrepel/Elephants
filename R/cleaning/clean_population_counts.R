# clean population count data 
library(data.table)
library(tidyverse)


# CERU ------------------------------

dt_ceru <- fread("data/raw_data/ceru/elephant_counts.csv") %>% 
  pivot_longer(cols = -Year, names_to = "park_id", values_to = "population_count") %>% 
  rename(year = Year) %>% 
  filter(!is.na(population_count))

dt_ceru %>% filter(park_id == "Hwange")
# Kaingo ------------------------------

dt_kaingo <- fread("data/raw_data/kaingo/kaingo_elephant_counts.csv") 

# Hwange --------------------------------------

#nothing new... 


# SANParks ---------------------------------------



# Lapalala ---------------------------------------



# HiP ----------------------------------------



# Ithala --------------------------------------------





#### Combine


dt_comb <- dt_ceru %>% 
  left_join(dt_kaingo) %>%
  filter(year > 2000) %>% 
  group_by(park_id) %>% 
  mutate(mean_population_count = mean(population_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(orig_name = park_id)


dt_comb %>%
  filter(year > 2000) %>% 
  group_by(orig_name) %>% 
  mutate(mean_population_count = mean(population_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(mean_population_count, orig_name) %>% 
  unique() %>% 
  dplyr::select(mean_population_count) %>% pull() %>% sum()

#### Fix park names 

dt_pn <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>%
  as.data.table() %>% 
  mutate(geometry = NULL, geom = NULL) %>% 
  dplyr::select(park_id = NAME, wdpa_pid = WDPA_PID, iso_3 = ISO3, area_km2)

unique(dt_pn$park_id)
unique(dt_comb$orig_name)
  
dt_fin <- dt_comb %>% 
  mutate(park_id = case_when(
    .default = orig_name, 
    orig_name == "Kruger" ~ "Kruger National Park",
    orig_name == "Moremi" ~ "Moremi",
    orig_name == "Chobe" ~ "Chobe",
    orig_name == "Nxai Pan" ~ "Nxai Pan",
    orig_name == "Makgakgadi" ~ "Makgadikgadi Pans",
    orig_name == "Vicoria Falls /Zambezi NP" ~ "Zambezi",
    orig_name == "Hwange" ~ "Hwange",
    orig_name == "Chizarira" ~ "Chizarira",
    orig_name == "Matusadonha" ~ "Matusadona",
    orig_name == "Northern Tuli" ~ "Northern Tuli",
    orig_name == "Mapungubwe" ~ "Mapungupwe National Park",
    orig_name == "Gonarezhou" ~ "Gonarezhou",
    orig_name == "Tembe" ~ "Tembe Elephant Park",
    orig_name == "iSimangaliso" ~ "iSimangaliso Wetland Park",
    orig_name == "HIP" ~ "Hluhluwe – iMfolozi Park",
    orig_name == "Pilanesberg" ~ "Pilanesberg National Park",
    orig_name == "Madikwe" ~ "Madikwe Nature Reserve",
    orig_name == "South Luangwa" ~ "South Luangwa",
    orig_name == "Kasungu" ~ "Kasungu National Park",
    orig_name == "Klaserie" ~ "Klaserie Private Nature Reserve",
    orig_name == "Balule" ~ "Balule Nature Reserve",
    orig_name == "Umbabat" ~ "Umbabat Private Nature Reserve",
    orig_name == "Timbavati" ~ "Timbavati Private Nature Reserve",
    orig_name == "Sabie Sands" ~ "Sabie Sands Private Nature Reserve",
    orig_name == "Limpopo" ~ "Limpopo",
    orig_name == "Maputo" ~ "Maputo",
    orig_name == "Ithala" ~ "Itala Nature Reserve",
    orig_name == "Addo" ~ "Addo Elephant National Park",
    orig_name == "Lower Zambezi" ~ "Lower Zambezi",
    orig_name == "Luengwe Luiana" ~ "Luengue-Luiana National Park",
    orig_name == "Khaudum" ~ "Khaudum",
    orig_name == "Bwabwata" ~ "Bwabwata",
    orig_name == "Sioma Ngwezi" ~ "Sioma Ngwezi",
    orig_name == "Lukusuzi" ~ "Lukusuzi",
    orig_name == "Nkasa Rupara (Mamili NP)" ~ "Nkasa Rupara",
    orig_name == "Letaba" ~ "Letaba Ranch Nature Reserve",
    orig_name == "Manyeleti" ~ "Manyeleti Nature Reserve",
    orig_name == "Luambe" ~ "Luambe",
    orig_name == "Mavinga" ~ "Mavinga National Park",
    orig_name == "Mana Pools" ~ "Mana Pools National Park",
    orig_name == "Kafue" ~ "Kafue",
    orig_name == "North Luangwa" ~ "North Luangwa"
  )) %>% 
  left_join(dt_pn)

fwrite(dt_fin, "data/processed_data/clean_data/all_population_counts.csv")
# on average 139706 Elephants 

# Plot population counts 

dt_pc <- fread("data/processed_data/clean_data/all_population_counts.csv") %>% 
  mutate(mean_density_km2 = mean_population_count/area_km2) %>% 
  group_by(park_id) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 3)

dt_pc %>% 
  dplyr::select(mean_density_km2, park_id) %>% 
  unique() %>% 
  select(mean_density_km2) %>% 
  pull() %>% 
  quantile(na.rm = T)


p_trends <- dt_pc %>% 
  ggplot() +
  geom_point(aes(x = year, y = population_count)) + 
  facet_wrap(~orig_name, scales = "free_y") +
  geom_smooth(aes(x = year, y = population_count), 
              method = "lm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_trends
ggsave(plot = p_trends, "builds/plots/supplement/linear_population_trends.png", dpi = 600, height = 8, width = 12 )
