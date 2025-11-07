library(data.table)
library(tidyverse)
library(rnaturalearth)
library(scico) 
library(patchwork)
library(sf)

##Homeranges ---------

dt_ele_raw <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")

dt_est <- fread("builds/model_outputs/issf_estimates_24hr_steps.csv") %>% 
  left_join(dt_ele_raw) %>%
  mutate(cluster_id = case_when(
    cluster_id == "greater_kruger" ~ "Limpopo", 
    cluster_id == "greater_waterberg" ~ "Limpopo", 
    cluster_id == "limpopo" ~ "Limpopo", 
    cluster_id == "kzn" ~ "KZN", 
    cluster_id == "luangwa" ~ "Luangwa", 
    cluster_id == "chobe" ~ "Chobe", 
    cluster_id == "kafue" ~ "Kafue", 
    cluster_id == "zambezi" ~ "Zambezi"
  )) 

dt_ele <- dt_ele_raw %>%
  filter(individual_id %in% unique(dt_est$individual_id))

sf_mcp <- st_read("data/spatial_data/elephants/mcp_home_ranges.gpkg") %>%
  filter(individual_id %in% unique(dt_est$individual_id))

# World 
sf_world <- ne_countries(scale = "medium", returnclass = "sf")

# Africa 
sf_africa <- sf_world %>% filter(region_un == "Africa") %>% 
  filter(!name == "Madagascar") %>% 
  st_transform(., crs = 4326)

### Plots ------------------------------------------
p_1 <- sf_mcp %>% 
  left_join(dt_ele) %>% 
  filter(!sex == "U") %>% 
  ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  geom_sf(data = sf_africa, fill = "linen", color = "grey25") +
  geom_sf(alpha = 0.25, aes(fill = sex, color = sex)) +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  # geom_sf(data = sf_clust, alpha = 0.1, fill = "yellow") + 
  theme_void() +
  theme(legend.position = "none")
p_1


p_2 <- dt_ele %>% 
  filter(sex %in% c("M", "F")) %>% 
  ggplot() +
  geom_boxplot(aes(x = sex, y = hr_mcp_area_km2, color = sex, fill = sex), alpha = 0.75) +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(x = "Sex", y = "Home Range Area (km2)") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_2

p_3 <- dt_ele %>% 
  mutate(cluster_id = case_when(
    cluster_id == "greater_kruger" ~ "Limpopo", 
    cluster_id == "greater_waterberg" ~ "Limpopo", 
    cluster_id == "limpopo" ~ "Limpopo", 
    cluster_id == "kzn" ~ "KZN", 
    cluster_id == "luangwa" ~ "Luangwa", 
    cluster_id == "chobe" ~ "Chobe", 
    cluster_id == "kafue" ~ "Kafue", 
    cluster_id == "zambezi" ~ "Zambezi"
  )) %>%
  filter(cluster_id %in% c("Limpopo", "KZN", "Luangwa", "Chobe")) %>% 
  filter(sex %in% c("M", "F")) %>% 
  ggplot() +
  geom_boxplot(aes(x = sex, y = hr_mcp_area_km2, color = sex, fill = sex), alpha = 0.75) +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(x = "Sex", y = "Home Range Area (km2)") +
  facet_wrap(~cluster_id, scales = "free_y", ncol = 4) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_3

library(patchwork)
p_hr <- (p_1 | p_2) / p_3
p_hr
ggsave(plot = p_hr, "builds/plots/supplement/home_range_plot.png", dpi = 600, height = 8, width = 8)

#### Step Summaries 

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


# 24 hrs 

dt_24_all <- dt_24 %>% 
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

dt_24_season <- dt_24 %>% 
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

dt_24_sex <- dt_24 %>% 
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

dt_24_sese <- dt_24 %>% 
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

dt_24_sum <- rbind(dt_24_all, dt_24_season, dt_24_sex, dt_24_sese) %>% 
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
    step_time = "24hrs")


# summarize 
dt_sum <- rbind(dt_1_sum, dt_3_sum, dt_12_sum, dt_24_sum)

library(scico)

p_sum <- dt_sum %>% 
  mutate(step_time = factor(step_time, levels = c("1hr", "3hrs", "12hrs", "24hrs"))) %>% 
  ggplot() +
  geom_boxplot(aes(y = median_sl, x = clean_tier, fill = clean_tier, color = clean_tier), alpha = 0.75) +
  labs(y = "Median Step Length (km)", x = "") +
  facet_wrap(~step_time, scales = "free_y", ncol = 1) +
  scale_fill_scico_d(palette = "batlow") +
  scale_color_scico_d(palette = "batlow") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_sum

ggsave(plot = p_sum, "builds/plots/supplement/step_length_distribution.png", dpi = 600, height = 10, width = 9)
