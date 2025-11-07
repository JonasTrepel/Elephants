#make nice viz of estimates 

library(data.table)
library(tidyverse)
library(ggridges)
library(MetBrewer)
library(scico)
library(patchwork)
library(rnaturalearth)
library(sf)
library(ggspatial)

dt_ele <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")

dt_est <- fread("builds/model_outputs/issf_estimates_24hr_steps.csv") %>% 
  left_join(dt_ele) %>%
  #filter(cluster_id %in% c("chobe", "limpopo", "kzn", "luangwa")) %>% 
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




### Median estimate ------------------------


p_est_ridges <- dt_est %>% 
  filter(season == "whole_year") %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  )) %>% 
  ggplot() +
  geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x)), 
                               color = "grey90", alpha = 0.8) +
  scico::scale_fill_scico(palette = "vik", midpoint = 0, direction = 1) +
  #scico::scale_color_scico(palette = "vik", midpoint = 0, direction = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Estimate Distribution", y = "", x = "Estimate", fill = "") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_ridges 


dt_me <- dt_est %>% 
  group_by(term, season) %>% 
  summarise(n = n(), 
            std_error = sd(estimate, na.rm = T)/sqrt(n), 
            
            mean_estimate = mean(estimate, na.rm = T), 
            mean_ci_lb = mean_estimate - 1.96*std_error, 
            mean_ci_ub = mean_estimate + 1.96*std_error, 
            
            
            median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median_estimate - 1.96*std_error, 
            median_ci_ub = median_estimate + 1.96*std_error, 
            
            median_p_value = median(p_value))  %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ), 
  sig = ifelse(median_p_value < 0.05, "significant", "non-significant"), 
  sig_mean = case_when(
    .default = "non-significant", 
    mean_ci_lb > 0 ~ "positive", 
    mean_ci_ub < 0 ~ "negative"),
  sig_median = case_when(
    .default = "non-significant", 
    median_ci_lb > 0 ~ "positive", 
    median_ci_ub < 0 ~ "negative"))

scico(palette = "vik", n = 7)

#"#65014B" "#B5549C" "#E4ADD6" "#F5F0F0" "#C0D9A1" "#5F903D" "#0C4C00"
#"#001260" "#06558B" "#71A7C4" "#EBE5E0" "#D29773" "#AA4613" "#590007"
p_est <- dt_me %>% 
  filter(season == "whole_year") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(x = clean_term, y = median_estimate, ymin = median_ci_lb, ymax = median_ci_ub,
        color = sig_median,fill = sig_median,  shape = sig_median), 
    position = position_dodge(width = 0.75),
    size = 1, linewidth = 1.1, alpha = 0.9
  ) +
  scale_shape_manual(values = c("positive" = 23, "negative" = 23, "non-significant" = 21), guide = "none") +
  theme_minimal() +
  scale_color_manual(values = c("positive" = "#AA4613", 
                                "negative" = "#06558B", 
                                "non-significant" = "grey75")) +
  scale_fill_manual(values = c("positive" = "#AA4613", 
                                "negative" = "#06558B", 
                                "non-significant" = "grey75")) +
  labs(x = "", y = "Estimate", color = "Season", fill = "Season", 
       subtitle = paste0("Median Estimates (± 95 % CI)\nn = ", n_distinct(dt_est$individual_id))) +
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  ) +
  coord_flip() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est


p_med_est <- p_est / p_est_ridges
p_med_est
ggsave(plot = p_med_est, "builds/plots/median_estimates_12hr_steps.png", dpi = 600, width = 4, height = 8)


#### Estimates vs context ----------------------
p_est_tm <- dt_est %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ), 
  term_mean = case_when(
    term == "evi_mean" ~ evi_mean_mean,
    term == "distance_to_water_km" ~ distance_to_water_km_mean,
    term == "distance_to_settlement_km" ~ distance_to_settlement_km_mean,
    term == "human_modification" ~ human_modification_mean,
    #   term == "enerscape" ~ enerscape_mean,
    term == "slope" ~ slope_mean
  )) %>% 
  ggplot() + 
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = term_mean, y = estimate, color = cluster_id), size = 0.75, alpha = 0.5) +
  geom_smooth(aes(x = term_mean, y = estimate), method = "lm") +
  facet_wrap(~clean_term, scales = "free", ncol = 5) +
  labs(x = "Variable Mean", y = "Estimate", subtitle = "Estimate ~ Variable Mean") +
  scale_color_scico_d(palette = "batlow") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_est_tm

p_est_tr <- dt_est %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ), 
  term_range = case_when(
    term == "evi_mean" ~ evi_mean_range,
    term == "distance_to_water_km" ~ distance_to_water_km_range,
    term == "distance_to_settlement_km" ~ distance_to_settlement_km_range,
    term == "human_modification" ~ human_modification_range,
    #   term == "enerscape" ~ enerscape_range,
    term == "slope" ~ slope_range
  )) %>% 
  ggplot() + 
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = term_range, y = estimate, color = cluster_id), size = 0.75, alpha = 0.5) +
  geom_smooth(aes(x = term_range, y = estimate), method = "lm") +
  facet_wrap(~clean_term, scales = "free", ncol = 5) +
  labs(x = "Variable Range", y = "Estimate", subtitle = "Estimate ~ Variable Range") +
  scale_color_scico_d(palette = "batlow") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))


p_est_tr

library(patchwork)
p_evc <- p_est_tm / p_est_tr
p_evc
ggsave(plot = p_evc, "builds/plots/supplement/estimates_vs_var_range_and_mean.png", 
       dpi = 600, height = 5, width = 10)
unique(dt_est$cluster_id)


#### cluster specific estimates -----------------------

dt_me_cluster <- dt_est %>% 
  group_by(term, season, cluster_id) %>% 
  summarise(n = n(), 
            std_error = sd(estimate, na.rm = T)/sqrt(n), 
            
            mean_estimate = mean(estimate, na.rm = T), 
            mean_ci_lb = mean_estimate - 1.96*std_error, 
            mean_ci_ub = mean_estimate + 1.96*std_error, 
            
            
            median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median_estimate - 1.96*std_error, 
            median_ci_ub = median_estimate + 1.96*std_error,
            
            p_value = median(p_value))  %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ), 
  sig = ifelse(p_value < 0.05, "significant", "non-significant"), 
  sig_mean = case_when(
    .default = "non-significant", 
    mean_ci_lb > 0 ~ "positive", 
    mean_ci_ub < 0 ~ "negative"),
  sig_median = case_when(
    .default = "non-significant", 
    median_ci_lb > 0 ~ "positive", 
    median_ci_ub < 0 ~ "negative"))


dt_est %>%
  dplyr::select(individual_id, cluster_id) %>% 
  unique() %>% 
  pull(cluster_id) %>% 
  table()

#Chobe     KZN Limpopo Luangwa Zambezi 
#118      28      87      42       2 

p_est_cluster <- dt_me_cluster %>% 
  filter(season == "whole_year" & !is.na(cluster_id)) %>% 
  filter(cluster_id %in% c("Chobe", "Limpopo", "KZN", "Luangwa")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(x = clean_term, y = median_estimate, ymin = median_ci_lb, ymax = median_ci_ub,
        color = cluster_id, fill = cluster_id, shape = sig_median, alpha = sig_median), 
    position = position_dodge(width = 0.75),
    size = 1, linewidth = 1.1
  ) +
  scale_shape_manual(values = c("positive" = 23, "negative" = 23, "non-significant" = 21), guide = "none") +
  scale_alpha_manual(values = c("significant" = 0.9, "non-significant" = 0.5), guide = "none") +
  theme_bw() +
  scico::scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scico::scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(x = "", y = "Estimate", color = "Season", fill = "Season", 
       subtitle = paste0("Cluster-Specific Median Estimates (± 95 % CI)")) +
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  ) +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen")) +
  facet_wrap(~cluster_id, ncol = 5)
p_est_cluster

### Maps ----------------------------

# Location points 
sf_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = 4326)

#24hr estimates 

dt_est <- fread("builds/model_outputs/issf_estimates_24hr_steps.csv")


sf_clust <- st_read("data/spatial_data/protected_areas/pa_clusters.gpkg") %>% 
  st_transform(crs = 4326) %>% 
  filter(cluster_id %in% c("limpopo", "kzn", "chobe", "luangwa")) %>% 
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

# World 
sf_world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Africa 
sf_africa <- sf_world %>% filter(region_un == "Africa") %>% 
  filter(!name == "Madagascar") %>% 
  st_transform(., crs = 4326)


p_loc <- sf_loc %>% 
  filter(individual_id %in% unique(dt_est$individual_id)) %>%
  sample_n(500000) %>% 
  ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  annotation_scale(location = "br", bar_cols = c("ivory4", "white")) +
  geom_sf(data = sf_africa, fill = "linen", color = "grey25") +
  geom_sf(data = sf_clust, aes(color = cluster_id, fill = cluster_id), alpha = 0.25, size = 1.5,
         # fill = "transparent", 
          linetype = "dashed", 
          linewidth = 1.01) +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  geom_sf(size = 0.1, alpha = 0.1, color = "black") +
  theme_void()+
  theme(legend.position = "none")
p_loc

### summarize --------------------------------------

library(patchwork)

p_wy <- p_mean_var / p_var_range
p_wy  
ggsave(p_wy, filename = "builds/plots/cluster_var_means_and_ranges.png",
       dpi = 600, height = 6, width = 10)


###### combine estimate figure
p_empty <- ggplot() + theme_void()
(p_est_map <- ((p_est / p_est_ridges)  | p_loc) +
  plot_layout(widths = c(1, 2.8)))

(p_est_comb <- p_est_map / p_est_cluster + plot_layout(heights = c(2.5, 1)))
p_est_comb  

ggsave(p_est_comb, filename = "builds/plots/main_estimate_figure.png",
       dpi = 600, height = 10, width = 10)


# Sex and Season specifics ------------------------------

p_est_ridges_season <- dt_est %>% 
  mutate(season = case_when(
    season == "whole_year" ~ "Full Year", 
    season == "dry_season" ~ "Dry Season", 
    season == "wet_season" ~ "Wet Season"
  )) %>% 
  # filter(season == "whole_year") %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  )) %>% 
  ggplot() +
  geom_density_ridges(aes(y = clean_term, x = estimate, fill = season), alpha = 0.75) +
  #geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x))) +
  scico::scale_fill_scico_d(palette = "batlow") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Split Up By Season", y = "", x = "Estimate", fill = "Season") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        axis.text.y = element_blank(), 
  
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen")) 
p_est_ridges_season 


p_est_ridges_sex <- dt_est %>% 
  filter(sex %in% c("M", "F")) %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  )) %>% 
  ggplot() +
  geom_density_ridges(aes(y = clean_term, x = estimate, fill = sex), alpha = 0.75) +
  #geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x))) +
  scico::scale_fill_scico_d(palette = "batlow") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Split Up By Sex", y = "", x = "Estimate", fill = "Sex") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_ridges_sex 


p_est_ridges_sex_season <- dt_est %>% 
  filter(sex %in% c("M", "F")) %>% 
  mutate(clean_season = case_when(
    season == "whole_year" ~ "in full year",
    season == "dry_season" ~ "in dry season",
    season == "wet_season" ~ "in wet season"),
    sex_season = paste(sex, clean_season)) %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  )) %>% 
  ggplot() +
  geom_density_ridges(aes(y = clean_term, x = estimate, fill = sex_season), alpha = 0.75) +
  #geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x))) +
  scico::scale_fill_scico_d(palette = "batlow") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Split Up By Sex & Season", y = "", x = "Estimate", fill = "Sex") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        axis.text.y = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_ridges_sex_season 

p_rid <- p_est_ridges_sex | p_est_ridges_season | p_est_ridges_sex_season
p_rid

##cluster specific -----
p_est_ridges_season_cluster <- dt_est %>% 
  filter(cluster_id %in% c("Chobe", "Limpopo", "KZN", "Luangwa")) %>% 
  filter(!is.na(cluster_id)) %>% 
  mutate(season = case_when(
    season == "whole_year" ~ "Full Year", 
    season == "dry_season" ~ "Dry Season", 
    season == "wet_season" ~ "Wet Season"
  )) %>% 
  # filter(season == "whole_year") %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  )) %>% 
  ggplot() +
  geom_density_ridges(aes(y = clean_term, x = estimate, fill = season), alpha = 0.75) +
  #geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x))) +
  scico::scale_fill_scico_d(palette = "batlow") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Estimate Distribution Split Up By Season", y = "", x = "Estimate", fill = "Season") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen")) +
  facet_wrap(~cluster_id, ncol = 5, scales = "free_x")
p_est_ridges_season_cluster 


p_est_ridges_sex_cluster <- dt_est %>% 
  filter(cluster_id %in% c("Chobe", "Limpopo", "KZN", "Luangwa")) %>% 
  filter(!is.na(cluster_id)) %>% 
  filter(sex %in% c("M", "F")) %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  )) %>% 
  ggplot() +
  geom_density_ridges(aes(y = clean_term, x = estimate, fill = sex), alpha = 0.75) +
  #geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x))) +
  scico::scale_fill_scico_d(palette = "batlow") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Estimate Distribution Split Up By Sex", y = "", x = "Estimate", fill = "Sex") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen")) +
  facet_wrap(~cluster_id, ncol = 5, scales = "free_x")
p_est_ridges_sex_cluster 


p_est_ridges_sex_season_cluster <- dt_est %>% 
  filter(cluster_id %in% c("Chobe", "Limpopo", "KZN", "Luangwa")) %>% 
  filter(!is.na(cluster_id)) %>% 
  filter(sex %in% c("M", "F")) %>% 
  mutate(clean_season = case_when(
    season == "whole_year" ~ "in full year",
    season == "dry_season" ~ "in dry season",
    season == "wet_season" ~ "in wet season"),
    sex_season = paste(sex, clean_season)) %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  )) %>% 
  ggplot() +
  geom_density_ridges(aes(y = clean_term, x = estimate, fill = sex_season), alpha = 0.75) +
  #geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x))) +
  scico::scale_fill_scico_d(palette = "batlow") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Estimate Distribution Split Up By Sex & Season", y = "", x = "Estimate", fill = "Sex / Season") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen")) +
  facet_wrap(~cluster_id, ncol = 5, scales = "free_x")
p_est_ridges_sex_season_cluster 

#combine 
p_rid_cluster <- (p_est_ridges_sex | p_est_ridges_season | p_est_ridges_sex_season) / p_est_ridges_sex_cluster / p_est_ridges_season_cluster / p_est_ridges_sex_season_cluster
p_rid_cluster


ggsave(p_rid_cluster, filename = "builds/plots/all_sex_season_estimates_figure.png",
       dpi = 600, height = 10, width = 11)

