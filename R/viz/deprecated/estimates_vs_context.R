library(data.table)
library(tidyverse)
library(ggridges)
library(MetBrewer)
library(scico)
library(patchwork)


dt_ele <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")

dt_est <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv") %>% 
  left_join(dt_ele) %>% 
  filter(cluster_id %in% c("chobe", "greater_kruger", "greater_waterberg", "kzn", "luangwa"))



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
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = term_mean, y = estimate, color = cluster_id), size = 0.5, alpha = 0.75) +
  geom_smooth(aes(x = term_mean, y = estimate), method = "lm") +
  facet_wrap(~clean_term, scales = "free", ncol = 5) +
  labs(x = "Variable Mean (in Homerange") +
  scale_color_scico_d(palette = "batlow") +
  theme(legend.position = "none", 
        panel.grid = element_blank())
  
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
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = term_range, y = estimate, color = cluster_id), size = 0.5, alpha = 0.75) +
  geom_smooth(aes(x = term_range, y = estimate), method = "lm") +
  facet_wrap(~clean_term, scales = "free", ncol = 5) +
  labs(x = "Variable Range (in Homerange") +
  scale_color_scico_d(palette = "batlow") +
  theme(legend.position = "none", 
        panel.grid = element_blank())


p_est_tr

library(patchwork)
p_comb <- p_est_tm / p_est_tr
p_comb
ggsave(plot = p_comb, "builds/plots/supplement/estimates_vs_var_range_and_mean.png", 
       dpi = 600, height = 5, width = 10)
unique(dt_est$cluster_id)
