library(data.table)
library(tidyverse)
library(patchwork)


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




dt_me <- dt_est %>% 
  group_by(term, season) %>% 
  summarise(n = n(), 
            pop_std_error = sd(estimate, na.rm = T)/sqrt(n), 
            
            mean_estimate = weighted.mean(estimate, 
                                                   w = (1/std_error), 
                                                   na.rm = T), 
            mean_ci_lb = mean_estimate - 1.96*pop_std_error, 
            mean_ci_ub = mean_estimate + 1.96*pop_std_error, 
            
            
            median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median_estimate - 1.96*pop_std_error, 
            median_ci_ub = median_estimate + 1.96*pop_std_error, 
            
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
p_est_mean <- dt_me %>% 
  filter(season == "whole_year") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(x = clean_term, y = mean_estimate, ymin = mean_ci_lb, ymax = mean_ci_ub,
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
       subtitle = paste0("Weighted Mean Estimates (± 95 % CI)\nn = ", n_distinct(dt_est$individual_id))) +
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
        strip.background = element_rect(fill = "linen", color = "linen")) +
  ylim(-.2, .2)
p_est_mean


p_est_median <- dt_me %>% 
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
        strip.background = element_rect(fill = "linen", color = "linen")) +
  ylim(-.2, .2)
p_est_median



dt_me_cluster <- dt_est %>% 
  group_by(term, season, cluster_id) %>% 
  summarise(n = n(), 
            pop_std_error = sd(estimate, na.rm = T)/sqrt(n), 
            
            mean_estimate = weighted.mean(estimate, 
                                          w = (1/std_error), 
                                          na.rm = T), 
            mean_ci_lb = mean_estimate - 1.96*pop_std_error, 
            mean_ci_ub = mean_estimate + 1.96*pop_std_error, 
            
            
            median_estimate = median(estimate, na.rm = T), 
            median_ci_lb = median_estimate - 1.96*pop_std_error, 
            median_ci_ub = median_estimate + 1.96*pop_std_error, 
            
            
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

p_est_cluster_median <- dt_me_cluster %>% 
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
p_est_cluster_median


p_est_cluster_mean <- dt_me_cluster %>% 
  filter(season == "whole_year" & !is.na(cluster_id)) %>% 
  filter(cluster_id %in% c("Chobe", "Limpopo", "KZN", "Luangwa")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(x = clean_term, y = mean_estimate, ymin = mean_ci_lb, ymax = mean_ci_ub,
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
       subtitle = paste0("Cluster-Specific Weigted Mean Estimates (± 95 % CI)")) +
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
p_est_cluster_mean

library(patchwork)
p <- (p_est_median|p_est_mean) / p_est_cluster_median / p_est_cluster_mean
p
ggsave(plot = p, "builds/plots/supplement/median_vs_weighted_mean_estimates.png", dpi = 900, 
       height = 8, width = 10)
