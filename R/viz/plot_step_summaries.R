library(data.table)
library(tidyverse)

dt_1 <- fread("data/processed_data/data_fragments/steps_1hr_incl_random.csv") %>% 
  filter(case_ == TRUE) %>% 
  group_by(individual_id) %>% filter(sum(case_) > 365) %>% ungroup()
dt_3 <- fread("data/processed_data/data_fragments/steps_3hrs_incl_random.csv") %>% 
  filter(case_ == TRUE) %>% group_by(individual_id) %>% filter(sum(case_) > 365) %>% ungroup()

dt_12 <- fread("data/processed_data/data_fragments/steps_12hrs_incl_random.csv") %>% 
  filter(case_ == TRUE) %>% group_by(individual_id) %>% filter(sum(case_) > 365) %>% ungroup()

dt_24 <- fread("data/processed_data/data_fragments/steps_24hrs_incl_random.csv") %>% 
  filter(case_ == TRUE) %>% group_by(individual_id) %>% filter(sum(case_) > 365) %>% ungroup()


unique(dt_12$individual_id)

dt_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  select(-c(lon, lat, date_time, month, obs_id)) %>% 
  unique()

# Summarize median step length of each individual 

# 1 hr 
dt_1_all <- dt_1 %>% 
  group_by(individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = "whole_year", 
         n_ids = n_distinct(individual_id), 
         sex = NA, 
         season = NA)

dt_1_season <- dt_1 %>% 
  group_by(season, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = season, 
         n_ids = n_distinct(individual_id), 
         sex = NA)

dt_1_sex <- dt_1 %>% 
  filter(sex %in% c("M", "F")) %>% 
  group_by(sex, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = sex, 
         n_ids = n_distinct(individual_id), 
         season = NA)

dt_1_sese <- dt_1 %>% 
  filter(sex %in% c("M", "F")) %>% 
  group_by(sex, season, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = paste0(season, "_", sex), 
         n_ids = n_distinct(individual_id))
  
dt_1_sum <- rbind(dt_1_all, dt_1_season, dt_1_sex, dt_1_sese) %>% 
  mutate(clean_tier = case_when(
    .default = tier,
    tier == "whole_year"     ~ "Whole Year\nAll Individuals", 
    tier == "dry_season"     ~ "Dry Season\nAll Individuals",
    tier == "wet_season"     ~ "Wet Season\nAll Individuals",
    tier == "F"              ~ "Whole Year\nFemales",
    tier == "M"              ~ "Whole Year\nMales",
    tier == "dry_season_F"   ~ "Dry Season\nFemales",
    tier == "wet_season_F"   ~ "Wet Season\nFemales",
    tier == "dry_season_M"   ~ "Dry Season\nMales",
    tier == "wet_season_M"   ~ "Wet Season\nMales"), 
    step_time = "1hr")


# 3 hrs 

dt_3_all <- dt_3 %>% 
  group_by(individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = "whole_year", 
         n_ids = n_distinct(individual_id), 
         sex = NA, 
         season = NA)

dt_3_season <- dt_3 %>% 
  group_by(season, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = season, 
         n_ids = n_distinct(individual_id), 
         sex = NA)

dt_3_sex <- dt_3 %>% 
  filter(sex %in% c("M", "F")) %>% 
  group_by(sex, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = sex, 
         n_ids = n_distinct(individual_id), 
         season = NA)

dt_3_sese <- dt_3 %>% 
  filter(sex %in% c("M", "F")) %>% 
  group_by(sex, season, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = paste0(season, "_", sex), 
         n_ids = n_distinct(individual_id))

dt_3_sum <- rbind(dt_3_all, dt_3_season, dt_3_sex, dt_3_sese) %>% 
  mutate(clean_tier = case_when(
    .default = tier,
    tier == "whole_year"     ~ "Whole Year\nAll Individuals", 
    tier == "dry_season"     ~ "Dry Season\nAll Individuals",
    tier == "wet_season"     ~ "Wet Season\nAll Individuals",
    tier == "F"              ~ "Whole Year\nFemales",
    tier == "M"              ~ "Whole Year\nMales",
    tier == "dry_season_F"   ~ "Dry Season\nFemales",
    tier == "wet_season_F"   ~ "Wet Season\nFemales",
    tier == "dry_season_M"   ~ "Dry Season\nMales",
    tier == "wet_season_M"   ~ "Wet Season\nMales"), 
    step_time = "3hrs")


# 12 hrs 

dt_12_all <- dt_12 %>% 
  group_by(individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = "whole_year", 
         n_ids = n_distinct(individual_id), 
         sex = NA, 
         season = NA)

dt_12_season <- dt_12 %>% 
  group_by(season, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = season, 
         n_ids = n_distinct(individual_id), 
         sex = NA)

dt_12_sex <- dt_12 %>% 
  filter(sex %in% c("M", "F")) %>% 
  group_by(sex, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = sex, 
         n_ids = n_distinct(individual_id), 
         season = NA)

dt_12_sese <- dt_12 %>% 
  filter(sex %in% c("M", "F")) %>% 
  group_by(sex, season, individual_id) %>% 
  summarize(median_sl = median(sl_km, na.rm = T), 
            median_ta = median(ta_, na.rm = T), 
            mean_sl = mean(sl_km, na.rm = T), 
            mean_ta = mean(ta_, na.rm = T), 
            q25_sl = quantile(sl_km, .25, na.rm = T), 
            q25_ta = quantile(ta_, .25, na.rm = T), 
            q75_sl = quantile(sl_km, .75, na.rm = T), 
            q75_ta = quantile(ta_, .75, na.rm = T), 
            n_steps = n()) %>% 
  ungroup() %>% 
  mutate(tier = paste0(season, "_", sex), 
         n_ids = n_distinct(individual_id))

dt_12_sum <- rbind(dt_12_all, dt_12_season, dt_12_sex, dt_12_sese) %>% 
  mutate(clean_tier = case_when(
    .default = tier,
    tier == "whole_year"     ~ "Whole Year\nAll Individuals", 
    tier == "dry_season"     ~ "Dry Season\nAll Individuals",
    tier == "wet_season"     ~ "Wet Season\nAll Individuals",
    tier == "F"              ~ "Whole Year\nFemales",
    tier == "M"              ~ "Whole Year\nMales",
    tier == "dry_season_F"   ~ "Dry Season\nFemales",
    tier == "wet_season_F"   ~ "Wet Season\nFemales",
    tier == "dry_season_M"   ~ "Dry Season\nMales",
    tier == "wet_season_M"   ~ "Wet Season\nMales"), 
    step_time = "12hrs")


# summarize 
dt_sum <- rbind(dt_1_sum, dt_3_sum, dt_12_sum)

p_sum <- dt_sum %>% 
  ggplot() +
  geom_boxplot(aes(y = median_sl, x = clean_tier)) +
  labs(y = "Median Step Length (km)", x = "") +
  facet_wrap(~step_time, scales = "free_y", ncol = 1) +
  theme_bw()
p_sum

ggsave(plot = p_sum, "builds/plots/supplement/step_length_distribution.png", dpi = 600, height = 9, width = 9)
