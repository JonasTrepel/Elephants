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

review = FALSE

if(review){
  dt_ele_raw <- fread("data/processed_data/for_review_only/anon_elephant_id_meta_data.csv")
  
}else{

dt_ele_raw <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")

}

dt_est_raw <- fread("builds/model_outputs/issf_estimates_24hr_steps.csv") 

dt_ele <- dt_ele_raw %>%
  filter(individual_id %in% unique(dt_est_raw$individual_id)) %>% 
  mutate(pop_data_avail = ifelse(park_id %in% c(
    "Luengue-Luiana National Park", "Chobe", "Nxai Pan", "Makgadikgadi Pans",
    "Moremi", "Northern Tuli", "Maputo", "Limpopo",
    "Kasungu National Park", "Khaudum", "Nkasa Rupara", "Bwabwata",
    "Kruger National Park", "Itala Nature Reserve", "Manyeleti Nature Reserve", "Pilanesberg National Park",
    "Tembe Elephant Park", "Madikwe Nature Reserve", "Klaserie Private Nature Reserve", "Mapungupwe National Park",
    "Sabie Sands Private Nature Reserve", "Timbavati Private Nature Reserve", "Umbabat Private Nature Reserve", "Kaingo Private Game Reserve",
    "Letaba Ranch Nature Reserve", "Hluhluwe – iMfolozi Park", "Lapalala Nature Reserve", "Balule Nature Reserve",
    "South Luangwa", "Sioma Ngwezi", "North Luangwa", "Luambe",
    "Gonarezhou", "Hwange"), "yes", "no")) %>% 
  group_by(park_id) %>% 
  mutate(n_ele_per_park = n_distinct(individual_id)) %>% 
  ungroup()

dt_ele %>% 
  filter(pop_data_avail == "yes" & n_ele_per_park >= 5) %>% 
  pull(park_id) %>% 
  unique()

dt_est <- dt_est_raw %>% 
  left_join(dt_ele) %>%
  #filter(cluster_id %in% c("chobe", "limpopo", "kzn", "luangwa")) %>% 
  mutate(cluster_id = case_when(
    cluster_id == "greater_kruger" ~ "GL & GM", 
    cluster_id == "greater_waterberg" ~ "GL & GM", 
    cluster_id == "limpopo" ~ "GL & GM", 
    cluster_id == "kzn" ~ "Lebombo", 
    cluster_id == "luangwa" ~ "MAZA", 
    cluster_id == "chobe" ~ "KAZA", 
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
ggsave(plot = p_med_est, "builds/plots/median_estimates_24hr_steps.png", dpi = 600, width = 4, height = 8)



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
  filter(cluster_id %in% c("GL & GM", "Lebombo", "KAZA", "MAZA")) %>% 
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

ggsave(plot = p_est_cluster, "builds/plots/supplement/cluster_ssf_estimates.png", 
       dpi = 900, height = 3, width = 9)

#### park specific estimates -----------------------

dt_me_park <- dt_est %>% 
  filter(pop_data_avail == "yes" & n_ele_per_park >= 5) %>% 
  group_by(term, season, park_id) %>% 
  summarise(n = n(), 
            std_error = sd(estimate, na.rm = T)/sqrt(n), 
            cluster_id = unique(cluster_id), 
            n_ele_per_park = unique(n_ele_per_park),
            
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
  filter(pop_data_avail == "yes" & n_ele_per_park >= 5) %>% 
  dplyr::select(individual_id, park_id) %>% 
  unique() %>% 
  pull(park_id) %>% 
  table()

#Chobe     KZN Limpopo Luangwa Zambezi 
#118      28      87      42       2 

p_est_park <- dt_me_park %>%
  mutate(
    park_clean = case_when(
      .default = park_id, 
      park_id == "Luengue-Luiana National Park" ~ "Luengue-Luiana National Park",
      park_id == "Chobe" ~ "Chobe National Park",
      park_id == "Nxai Pan" ~ "Nxai Pan National Park",
      park_id == "Makgadikgadi Pans" ~ "Makgadikgadi Pans National Park",
      park_id == "Moremi" ~ "Moremi Game Reserve",
      park_id == "Northern Tuli" ~ "Northern Tuli Game Reserve",
      park_id == "Maputo" ~ "Maputo National Park",
      park_id == "Limpopo" ~ "Limpopo National Park",
      park_id == "Kasungu National Park" ~ "Kasungu National Park",
      park_id == "Khaudum"  ~ "Khaudum National Park",
      park_id == "Nkasa Rupara" ~ "Nkasa Rupara National Park",
      park_id == "Bwabwata" ~ "Bwabwata National Park",
      park_id == "Kruger National Park" ~ "Kruger National Park",
      park_id == "Itala Nature Reserve" ~ "Ithala Game Reserve",
      park_id == "Manyeleti Nature Reserve" ~ "Manyeleti Private Nature Reserve",
      park_id == "Pilanesberg National Park" ~ "Pilanesberg Provincial Reserve",
      park_id == "Tembe Elephant Park"  ~ "Tembe Elephant Park",
      park_id == "Madikwe Nature Reserve" ~ "Madikwe Provincial Reserve",
      park_id == "Klaserie Private Nature Reserve"  ~ "Klaserie Private Nature Reserve",
      park_id == "Mapungupwe National Park"  ~ "Mapungubwe National Park",
      park_id == "Sabie Sands Private Nature Reserve" ~ "Sabie Sands Private Nature Reserve",
      park_id == "Timbavati Private Nature Reserve" ~ "Timbavati Private Nature Reserve",
      park_id == "Umbabat Private Nature Reserve" ~ "Umbabat Private Nature Reserve",
      park_id == "Kaingo Private Game Reserve" ~ "Kaingo Private Game Reserve",
      park_id == "Letaba Ranch Nature Reserve" ~ "Letaba Ranch Nature Reserve",
      park_id == "Hluhluwe – iMfolozi Park" ~ "Hluhluwe–iMfolozi Park",
      park_id == "Lapalala Nature Reserve" ~ "Lapalala Private Nature Reserve",
      park_id == "Balule Nature Reserve" ~ "Balule Private Nature Reserve",
      park_id == "South Luangwa" ~ "South Luangwa National Park",
      park_id == "Sioma Ngwezi" ~ "Sioma Ngwezi National Park",
      park_id == "North Luangwa" ~ "North Luangwa National Park",
      park_id == "Luambe" ~ "Luambe National Park",
      park_id == "Gonarezhou" ~ "Gonarezhou National Park",
      park_id == "Hwange" ~ "Hwange National Park"
    )
  ) %>%
  ungroup() %>% 
  mutate(
    park_clean = factor(
      park_clean,
      levels = distinct(., park_clean, cluster_id) %>% 
        arrange(cluster_id) %>% 
        distinct(park_clean) %>% 
        pull(park_clean)),
    park_label = paste0(park_clean, "\n(n = ", n_ele_per_park, " elephants)")
    ) %>%
  mutate(
    park_label = factor(
           park_label,
           levels = distinct(., park_label, cluster_id) %>% 
             arrange(cluster_id) %>% 
             distinct(park_label) %>% 
             pull(park_label))) %>% 
  filter(season == "whole_year" & !is.na(park_id)) %>%
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
       subtitle = paste0("Park-Specific Median Estimates (± 95 % CI)")) +
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
        strip.background = element_rect(fill = "linen", color = "linen"),
        strip.text = element_text(size = 8, lineheight = 0.9)) +
  facet_wrap(~park_label, ncol = 5)

p_est_park


### Maps ----------------------------

#### necessary data cannot be shared :(

# Location points 
sf_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., crs = 4326)

sf_pas <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  mutate(park_id = NAME) %>%
  left_join(sf_loc %>%
              as.data.frame() %>%
              st_drop_geometry() %>%
              dplyr::select(cluster_id, park_id) %>% 
              unique() %>% 
              filter(!park_id == "") %>% 
              mutate(cluster_id = case_when(
                cluster_id == "greater_kruger" ~ "GL & GM", 
                cluster_id == "greater_waterberg" ~ "GL & GM", 
                cluster_id == "limpopo" ~ "GL & GM", 
                cluster_id == "kzn" ~ "Lebombo", 
                cluster_id == "luangwa" ~ "MAZA", 
                cluster_id == "chobe" ~ "KAZA", 
                cluster_id == "kafue" ~ "Kafue", 
                cluster_id == "zambezi" ~ "Zambezi"
              )))


#24hr estimates 

dt_est <- fread("builds/model_outputs/issf_estimates_24hr_steps.csv")


sf_clust <- st_read("data/spatial_data/protected_areas/pa_clusters.gpkg") %>% 
  st_transform(crs = 4326) %>% 
  filter(cluster_id %in% c("limpopo", "kzn", "chobe", "luangwa")) %>% 
  mutate(cluster_id = case_when(
    cluster_id == "greater_kruger" ~ "GL & GM", 
    cluster_id == "greater_waterberg" ~ "GL & GM", 
    cluster_id == "limpopo" ~ "GL & GM", 
    cluster_id == "kzn" ~ "Lebombo", 
    cluster_id == "luangwa" ~ "MAZA", 
    cluster_id == "chobe" ~ "KAZA", 
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
  geom_sf(data = sf_africa, fill = "linen", color = "ivory3", alpha = .25) +
  geom_sf(data = sf_clust, aes(color = cluster_id, fill = cluster_id), alpha = 0.25, size = 1.5,
          fill = "transparent", 
          linetype = "dashed", 
          linewidth = 1.002) +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  geom_sf(size = 0.1, alpha = 0.025, color = "grey25") +
  geom_sf(data = sf_pas %>% 
            filter(park_id %in% unique(dt_me_park$park_id)), aes(color = cluster_id, fill = cluster_id), alpha = 0.25, size = 1.5,
          linetype = "solid", 
          linewidth = 1.001) +
  theme_void()+
  theme(legend.position = "none")
p_loc

### summarize --------------------------------------

library(patchwork)

###### combine estimate figure cluster level summary 
p_empty <- ggplot() + theme_void()
(p_est_map <- ((p_est / p_est_ridges)  | p_loc) +
  plot_layout(widths = c(1, 2.8)))

(p_est_comb <- p_est_map / p_est_cluster + plot_layout(heights = c(2.5, 1)))
p_est_comb  

ggsave(p_est_comb, filename = "builds/plots/main_estimate_figure.png",
       dpi = 600, height = 10, width = 10)

###### combine estimate figure park level summary
p_empty <- ggplot() + theme_void()
(p_est_map <- ((p_est / p_est_ridges)  | p_loc) +
    plot_layout(widths = c(1, 2.8)))

(p_est_comb <- p_est_map / p_est_park )#+ plot_layout(heights = c(2.5, 1)))
p_est_comb  

ggsave(p_est_comb, filename = "builds/plots/main_estimate_figure_park_level.png",
       dpi = 600, height = 12, width = 10.5)


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

