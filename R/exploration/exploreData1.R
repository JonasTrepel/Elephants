## explore elephant data 
library(sf)
library(data.table)
library(tidyverse)

dt <- fread("data/rawData/elephantCounts.csv") 

n_distinct(dt$Year)
dt.sub 
  
avgAll <- dt %>% 
  pivot_longer(names_to = "Area", values_to = "Count", cols = c(-Year)) %>%
  arrange(Area) %>% 
  group_by(Area) %>% 
  summarize(meanCount = mean(Count, na.rm = TRUE), 
            nas = sum(is.na(Count)), 
            timeSeriesPoints = 40-nas) %>% 
  dplyr::select(-nas)

avg10 <- dt %>% 
  pivot_longer(names_to = "Area", values_to = "Count", cols = c(-Year)) %>%
  arrange(Area) %>% 
  group_by(Area) %>% 
  filter(Year > 2014) %>% 
  summarize(meanCountSince2014 = mean(Count, na.rm = TRUE))

avg5 <- dt %>% 
  pivot_longer(names_to = "Area", values_to = "Count", cols = c(-Year)) %>%
  arrange(Area) %>% 
  group_by(Area) %>% 
  filter(Year > 2019) %>% 
  summarize(meanCountSince2019 = mean(Count, na.rm = TRUE))

avg <- avgAll %>% 
  left_join(avg10) %>% 
  left_join(avg5) %>% 
  mutate_if(is.numeric, round, 1)%>% as.data.table()

mean(avg$meanCount, na.rm = T)
sd(avg$meanCount, na.rm = T)
min(avg[meanCount>0, ]$meanCount, na.rm = T)
max(avg$meanCount, na.rm = T)

mean(avg$timeSeriesPoints, na.rm = T)
sd(avg$timeSeriesPoints, na.rm = T)
min(avg[timeSeriesPoints>0, ]$timeSeriesPoints, na.rm = T)
max(avg$timeSeriesPoints, na.rm = T)


fwrite(avg, "builds/tables/AverageElephantDensities.csv")


dt.tr <- fread("data/rawData/trackingMeta.csv") %>% 
  mutate(
    TrackingEndYear = gsub("date", 2024, TrackingEndYear), 
    TrackingEndYear = as.numeric(TrackingEndYear),
  TrackingDuration = TrackingEndYear-TrackingStartYear
)


sum(dt.tr$TotalTracked)
mean(dt.tr$TrackingDuration)
sd(dt.tr$TrackingDuration)
n_distinct(dt.tr$Park)

median(dt.tr$TrackingDuration)


ttp <- dt.tr %>% 
  group_by(Park) %>% 
  summarize(TotalTrackedPark = sum(TotalTracked))

mean(ttp$TotalTrackedPark) 
sd(ttp$TotalTrackedPark)  
median(ttp$TotalTrackedPark)  

