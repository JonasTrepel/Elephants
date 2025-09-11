library(data.table)
library(tidyverse)
library(tidylog)
library(mapview)
library(rnaturalearth)


dt_ele <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")
## get estimates 
dt_est <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv") %>% 
  left_join(dt_ele) %>%
  filter(cluster_id %in% c("chobe", "greater_kruger", "greater_waterberg", "kzn", "luangwa")) %>% 
  mutate(cluster_id = case_when(
    cluster_id == "greater_kruger" ~ "Greater Kruger", 
    cluster_id == "greater_waterberg" ~ "Greater Waterberg", 
    cluster_id == "kzn" ~ "KZN", 
    cluster_id == "luangwa" ~ "Luangwa", 
    cluster_id == "chobe" ~ "Chobe", 
    cluster_id == "kafue" ~ "Kafue", 
    cluster_id == "zambezi" ~ "Zambezi"
  )) 


#### ESIMATES OF WHOLE YEAR ------------------------------------------------


dt_me <- dt_est %>% 
  group_by(term, season, cluster_id) %>% 
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
  filter(season == "wet_season" & !is.na(cluster_id)) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(x = clean_term, y = median_estimate, ymin = median_ci_lb, ymax = median_ci_ub,
        color = cluster_id, fill = cluster_id, shape = sig_median, alpha = sig_median), 
    position = position_dodge(width = 0.75),
    size = 1, linewidth = 1.1
  ) +
  scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
  scale_alpha_manual(values = c("significant" = 0.9, "non-significant" = 0.5), guide = "none") +
  theme_bw() +
  scico::scale_color_scico_d(palette = "batlow") +
  scico::scale_fill_scico_d(palette = "batlow") +
  labs(x = "", y = "Estimate", color = "Season", fill = "Season", 
       subtitle = paste0("Median Estimates (± 95 % CI)")) +
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  ) +
  theme(
    legend.position = "bottom",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "grey97")) +
  facet_wrap(~cluster_id, ncol = 5)
p_est

p_mean_var <- dt_est %>% 
  filter(season == "whole_year" & !is.na(cluster_id)) %>% 
  pivot_longer(cols = c(evi_mean_mean, slope_mean,
                        distance_to_water_km_mean, distance_to_settlement_km_mean, 
                        human_modification_mean), 
               names_to = "var_name", values_to = "var_mean_value") %>% 
  mutate(clean_var_name = case_when(
    .default = var_name,
    var_name == "evi_mean_mean" ~ "EVI",
    var_name == "distance_to_water_km_mean" ~ "Distance to Water",
    var_name == "distance_to_settlement_km_mean" ~ "Distance to Settlement",
    var_name == "human_modification_mean" ~ "Human Modification Index",
    var_name == "slope_mean" ~ "Slope")) %>% 
  ggplot() +
  geom_boxplot(aes(x = cluster_id, y = var_mean_value, color = cluster_id, fill = cluster_id), alpha = 0.5) +
  scico::scale_color_scico_d(palette = "batlow") +
  scico::scale_fill_scico_d(palette = "batlow") +
  facet_wrap(~clean_var_name, ncol = 5, scales = "free_y") +
  labs(y = "Home Range Variable Mean", x = "Cluster ID") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "grey97"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average variable values of elephant home ranges")

p_mean_var

#range
p_var_range <- dt_est %>% 
  filter(season == "whole_year" & !is.na(cluster_id)) %>% 
  pivot_longer(cols = c(evi_mean_range, slope_range,
                        distance_to_water_km_range, distance_to_settlement_km_range, 
                        human_modification_range), 
               names_to = "var_name", values_to = "var_range_value") %>% 
  mutate(clean_var_name = case_when(
    .default = var_name,
    var_name == "evi_mean_range" ~ "EVI",
    var_name == "distance_to_water_km_range" ~ "Distance to Water",
    var_name == "distance_to_settlement_km_range" ~ "Distance to Settlement",
    var_name == "human_modification_range" ~ "Human Modification Index",
    var_name == "slope_range" ~ "Slope")) %>% 
  ggplot() +
  geom_boxplot(aes(x = cluster_id, y = var_range_value, color = cluster_id, fill = cluster_id), alpha = 0.5) +
  scico::scale_color_scico_d(palette = "batlow") +
  scico::scale_fill_scico_d(palette = "batlow") +
  facet_wrap(~clean_var_name, ncol = 5, scales = "free_y") +
  labs(y = "Home Range Variable Range", x = "Cluster ID") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "grey97"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Variable ranges within elephant home ranges")

p_var_range

### summarize 

library(patchwork)

p_wy <- p_est / p_mean_var / p_var_range
p_wy  
ggsave(p_wy, filename = "builds/plots/cluster_estimates_and_ranges.png",
       dpi = 600, height = 8, width = 10)

ggsave(p_est, filename = "builds/plots/cluster_estimates.png",
       dpi = 600, height = 3, width = 10)

### season specifics ---------------


p_est_ridges_season <- dt_est %>% 
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
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "grey97")) +
  facet_wrap(~cluster_id, ncol = 5, scales = "free_x")
p_est_ridges_season 


p_est_ridges_sex <- dt_est %>% 
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
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "grey97")) +
  facet_wrap(~cluster_id, ncol = 5, scales = "free_x")
p_est_ridges_sex 


p_est_ridges_sex_season <- dt_est %>% 
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
  labs(subtitle = "Estimate Distribution Split Up By Sex & Season", y = "", x = "Estimate", fill = "Sex") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "grey97")) +
  facet_wrap(~cluster_id, ncol = 5, scales = "free_x")
p_est_ridges_sex_season 

#combine 
p_rid <- p_est_ridges_sex / p_est_ridges_season / p_est_ridges_sex_season
p_rid

ggsave(p_rid, filename = "builds/plots/supplement/cluster_estimates_ridges_sex_and_season.png",
       dpi = 600, height = 8, width = 10)

##### formalize 
#Summary: there are occasionally differences between males and females,
# and in the case of distance to water also seasonal, but overall effect directions are always the same

library(emmeans)

# Evi
m_evi <- glm(estimate ~ sex + season + evi_mean_mean + evi_mean_range, 
             data = dt_est[dt_est$term == "evi_mean" & dt_est$sex != "U", ])
summary(m_evi)
emmeans(m_evi, pairwise ~ sex)
emmeans(m_evi, pairwise ~ season)

# Slope
m_slope <- glm(estimate ~ sex + season + slope_mean + slope_range, 
             data = dt_est[dt_est$term == "slope" & dt_est$sex != "U", ])
summary(m_slope)
emmeans(m_slope, pairwise ~ sex)
emmeans(m_slope, pairwise ~ season)


# Human modification 
m_humod <- glm(estimate ~ sex + season + human_modification_mean + human_modification_range, 
               data = dt_est[dt_est$term == "human_modification" & dt_est$sex != "U", ])
summary(m_humod)
emmeans(m_humod, pairwise ~ sex)
emmeans(m_humod, pairwise ~ season)

# Distance to settlement
m_settle <- glm(estimate ~ sex + season + distance_to_settlement_km_mean + distance_to_settlement_km_range, 
                data = dt_est[dt_est$term == "distance_to_settlement_km" & dt_est$sex != "U", ])
summary(m_settle)
emmeans(m_settle, pairwise ~ sex)
emmeans(m_settle, pairwise ~ season)

# Distance to water
m_water <- glm(estimate ~ sex + season + distance_to_water_km_mean + distance_to_water_km_range, 
               data = dt_est[dt_est$term == "distance_to_water_km" & dt_est$sex != "U", ])
summary(m_water)
emmeans(m_water, pairwise ~ sex)
emmeans(m_water, pairwise ~ season)



