## explore elephant data 
library(sf)
library(data.table)
library(tidyverse)

dt <- fread("data/raw_data/elephant_counts.csv") 

n_distinct(dt$Year)



avg_all <- dt %>% 
  pivot_longer(names_to = "Area", values_to = "Count", cols = c(-Year)) %>%
  arrange(Area) %>% 
  group_by(Area) %>% 
  summarize(meanCount = mean(Count, na.rm = TRUE), 
            nas = sum(is.na(Count)), 
            timeSeriesPoints = 40-nas) %>% 
  dplyr::select(-nas)

sum(avg_all$meanCount, na.rm = T)

avg_10 <- dt %>% 
  pivot_longer(names_to = "Area", values_to = "Count", cols = c(-Year)) %>%
  arrange(Area) %>% 
  group_by(Area) %>% 
  filter(Year > 2014) %>% 
  summarize(meanCountSince2014 = mean(Count, na.rm = TRUE))

avg_5 <- dt %>% 
  pivot_longer(names_to = "Area", values_to = "Count", cols = c(-Year)) %>%
  arrange(Area) %>% 
  group_by(Area) %>% 
  filter(Year > 2019) %>% 
  summarize(meanCountSince2019 = mean(Count, na.rm = TRUE))

avg <- avg_all %>% 
  left_join(avg_10) %>% 
  left_join(avg_5) %>% 
  mutate_if(is.numeric, round, 1)%>% as.data.table()

mean(avg$meanCount, na.rm = T)
sd(avg$meanCount, na.rm = T)
min(avg[meanCount>0, ]$meanCount, na.rm = T)
max(avg$meanCount, na.rm = T)

mean(avg$timeSeriesPoints, na.rm = T)
sd(avg$timeSeriesPoints, na.rm = T)
min(avg[timeSeriesPoints>0, ]$timeSeriesPoints, na.rm = T)
max(avg$timeSeriesPoints, na.rm = T)


fwrite(avg, "builds/tables/average_elephant_densities.csv")


dt_tr <- fread("data/raw_data/tracking_meta_dodgy.csv") %>% 
  mutate(
    end_year = gsub("date", 2024, end_year), 
    end_year = as.numeric(end_year),
  tracking_duration = end_year-start_year
)


sum(dt_tr$total_tracked)
mean(dt_tr$tracking_duration)
sd(dt_tr$tracking_duration)
n_distinct(dt_tr$park)

median(dt_tr$tracking_duration)


ttp <- dt_tr %>% 
  group_by(park) %>% 
  summarize(total_tracked_park = sum(total_tracked))

mean(ttp$total_tracked_park) 
sd(ttp$total_tracked_park)  
median(ttp$total_tracked_park)  


### Ryans summary 

dt_sum <- fread("data/raw_data/Telemetry Inventory.csv")

dt_sum %>% dplyr::select(TotalElephants) %>% pull() %>% sum(na.rm = T)

dt_sum %>% filter(DataOwner == "CERU") %>% dplyr::select(TotalElephants) %>% pull() %>% sum(na.rm = T)
dt_sum %>% filter(!DataOwner == "CERU") %>% dplyr::select(TotalElephants) %>% pull() %>% sum(na.rm = T)

dt_sum %>% filter(DataOwner == "CERU") %>% dplyr::select(PA) %>% pull() %>% n_distinct()
dt_sum %>% filter(!DataOwner == "CERU") %>% dplyr::select(PA) %>% pull() %>% n_distinct()
