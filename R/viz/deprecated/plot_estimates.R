#make nice viz of estimates 

library(data.table)
library(tidyverse)
library(ggridges)
library(MetBrewer)
library(scico)
library(patchwork)

dt_est <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv")
dt_ele <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")

dt_est <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv") %>% 
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
  geom_density_ridges_gradient(aes(y = clean_term, x = estimate, fill = after_stat(x))) +
  scico::scale_fill_scico(palette = "bam", midpoint = 0, direction = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(subtitle = "Estimate Distribution", y = "", x = "Estimate", fill = "") +
  theme_classic() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
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

scico(palette = "bam", n = 7)

#"#65014B" "#B5549C" "#E4ADD6" "#F5F0F0" "#C0D9A1" "#5F903D" "#0C4C00"

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
  theme_classic() +
  scale_color_manual(values = c("positive" = "#5F903D", 
                                "negative" = "#B5549C", 
                                "non-significant" = "grey75")) +
  scale_fill_manual(values = c("positive" = "#5F903D", 
                                "negative" = "#B5549C", 
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
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est


p_med_est <- p_est | p_est_ridges
p_med_est
ggsave(plot = p_med_est, "builds/plots/median_estimates_12hr_steps.png", dpi = 600, width = 8, height = 3)


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
  labs(x = "Variable Mean", y = "Estimate") +
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


p_est_cluster <- dt_me_cluster %>% 
  filter(season == "whole_year" & !is.na(cluster_id)) %>% 
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
  scico::scale_color_scico_d(palette = "batlow") +
  scico::scale_fill_scico_d(palette = "batlow") +
  labs(x = "", y = "Estimate", color = "Season", fill = "Season", 
       subtitle = paste0("Cluster-Specific Median Estimates (± 95 % CI)")) +
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  ) +
  theme_classic() +
  coord_flip() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen")) +
  facet_wrap(~cluster_id, ncol = 5)
p_est_cluster

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
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"),
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
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Variable ranges within elephant home ranges")

p_var_range

### summarize 

library(patchwork)

p_wy <- p_mean_var / p_var_range
p_wy  
ggsave(p_wy, filename = "builds/plots/cluster_var_means_and_ranges.png",
       dpi = 600, height = 6, width = 10)

###### combine estimate figure

p_est_comb <- p_med_est / p_est_tr / p_est_cluster
p_est_comb  

ggsave(p_est_comb, filename = "builds/plots/main_estimate_figure.png",
       dpi = 600, height = 7.5, width = 10)


# Sex and Season specifics ------------------------------









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
