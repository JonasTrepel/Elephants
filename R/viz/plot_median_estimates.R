#make nice viz of estimates 

library(data.table)
library(tidyverse)
library(ggridges)
library(MetBrewer)
library(scico)
library(patchwork)

dt_est <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv")


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
  geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x))) +
  scico::scale_fill_scico(palette = "bam", midpoint = 0, direction = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Estimate Distribution", y = "", x = "Estimate", fill = "") +
  theme_classic() +
  theme(legend.position = "none")
p_est_ridges 




dt_me <- dt_est %>% 
  group_by(term, season) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), ,
            mean_ci_lb = mean(ci_lb),
            mean_ci_ub = mean(ci_ub),
            
            median_estimate = median(estimate, na.rm = T), ,
            median_ci_lb = median(ci_lb),
            median_ci_ub = median(ci_ub),
            
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
  sig_mean = ifelse(mean_ci_lb > 0 | mean_ci_ub < 0, "significant", "non-significant"),
  sig_median = ifelse(median_ci_lb > 0 | median_ci_ub < 0, "significant", "non-significant"))


p_est <- dt_me %>% 
  filter(season == "whole_year") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(x = clean_term, y = median_estimate, ymin = median_ci_lb, ymax = median_ci_ub,
        color = sig_median), 
    position = position_dodge(width = 0.75),
    size = 1, linewidth = 1.1, alpha = 0.9
  ) +
  scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
  theme_classic() +
  scale_color_met_d(name = "Egypt", direction = -1) +
  scale_fill_met_d(name = "Egypt", direction = -1) +
  labs(x = "", y = "Estimate", color = "Season", fill = "Season", 
       subtitle = paste0("Median Estimates (± 95 % CI)\nn = ", n_distinct(dt_est$individual_id))) +
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  ) +
  theme(
    legend.position = "bottom",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip() +
  theme(legend.position = "none")
p_est


p <- p_est | p_est_ridges
p
ggsave(plot = p, "builds/plots/median_estimates_12hr_steps.png", dpi = 600, width = 8, height = 3)

#### test if we see a difference between SA and not

dt_pc <- fread("data/processed_data/clean_data/all_population_counts.csv") %>% 
  select(park_id, iso_3) %>% 
  unique()

dt_id <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  select(individual_id, park_id, hr_mcp_area_km2, hr_locoh_area_km2) %>% 
  unique %>% 
  left_join(dt_pc)

dt_est_id <- dt_est %>% 
  left_join(dt_id) %>% 
  mutate(iso_3 = ifelse(is.na(iso_3), "NotZAF", iso_3),
    fenced = ifelse(iso_3 == "ZAF", "Fenced", "Unfenced"))


dt_me_fence <- dt_est_id %>% 
  group_by(term, season, fenced) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), ,
            mean_ci_lb = mean(ci_lb),
            mean_ci_ub = mean(ci_ub),
            
            median_estimate = median(estimate, na.rm = T), ,
            median_ci_lb = median(ci_lb),
            median_ci_ub = median(ci_ub),
            
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
  sig_mean = ifelse(mean_ci_lb > 0 | mean_ci_ub < 0, "significant", "non-significant"),
  sig_median = ifelse(median_ci_lb > 0 | median_ci_ub < 0, "significant", "non-significant"))


p_est_fence <- dt_me_fence %>% 
  filter(season == "whole_year") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(x = clean_term, y = median_estimate, ymin = median_ci_lb, ymax = median_ci_ub,
        color = fenced), 
    position = position_dodge(width = 0.75),
    size = 1, linewidth = 1.1, alpha = 0.9
  ) +
  scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
  theme_classic() +
  scale_color_met_d(name = "Egypt", direction = -1) +
  scale_fill_met_d(name = "Egypt", direction = -1) +
  labs(x = "", y = "Estimate", color = "Season", fill = "Season", 
       subtitle = paste0("Median Estimates (± 95 % CI)\nn = ", n_distinct(dt_est$individual_id))) +
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  ) +
  theme(
    legend.position = "bottom",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip() +
  theme(legend.position = "right")
p_est_fence #the negative estimate of slope in fenced areas is most likely because they respond to a stronger gradient inside ZAF


table(dt_est_id[dt_est_id$season == "whole_year",]$fenced)

# steps <- fread("data/processed_data/data_fragments/steps_12hrs_habitat_covariates.csv") %>% 
#   left_join(dt_pc)
# glimpse(steps)
# 
# dt_fenced <- steps %>% 
#   mutate(iso_3 = ifelse(is.na(iso_3), "NotZAF", iso_3),
#          fenced = ifelse(iso_3 == "ZAF", "Fenced", "Unfenced")) %>% 
#   group_by(fenced, individual_id) %>% 
#   summarize(slope_range = max(slope, na.rm =T)-min(slope, na.rm = T), 
#             evi_range = max(evi_mean, na.rm =T)-min(evi_mean, na.rm = T), 
#             mean_slope = mean(slope))
# 
# 
# dt_fenced %>% ggplot() +
#   geom_boxplot(aes(x = fenced, y = slope_range))
# 
# dt_fenced %>% ggplot() +
#   geom_boxplot(aes(x = fenced, y = mean_slope))
