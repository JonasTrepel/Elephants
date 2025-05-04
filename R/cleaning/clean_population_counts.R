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
  left_join(dt_kaingo) 

dt_comb %>%
  filter(year > 2000) %>% 
  group_by(park_id) %>% 
  mutate(mean_population_count = mean(population_count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(mean_population_count, park_id) %>% 
  unique() %>% 
  dplyr::select(mean_population_count) %>% pull() %>% sum()


fwrite(dt_comb, "data/clean_data/all_population_counts.csv")
# on average 139706 Elephants 