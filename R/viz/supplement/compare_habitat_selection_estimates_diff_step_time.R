library(terra)
library(sf)
library(exactextractr)
library(data.table)
library(tidyverse)


dt_est_1 <- fread("builds/model_outputs/issf_estimates_1hr_steps.csv")  %>% 
  dplyr::select(estimate_1 = estimate,
         std_error_1 = std_error, 
         p_value_1 = p_value, 
         term, 
         season, 
         individual_id) %>% unique()

table(dt_est_1[dt_est_1$season == "whole_year", ]$term)

dt_est_3 <- fread("builds/model_outputs/issf_estimates_3hr_steps.csv") %>% 
  dplyr::select(estimate_3 = estimate,
         std_error_3 = std_error, 
         p_value_3 = p_value,
         term, 
         season, 
         individual_id) %>% unique()

dt_est_12 <- fread("builds/model_outputs/issf_estimates_12hr_steps.csv") %>% 
  dplyr::select(estimate_12 = estimate,
         std_error_12 = std_error, 
         p_value_12 = p_value,
         term, 
         season, 
         individual_id) %>% unique()

dt_est_24 <- fread("builds/model_outputs/issf_estimates_24hr_steps.csv") %>% 
  dplyr::select(estimate_24 = estimate,
                std_error_24 = std_error, 
                p_value_24 = p_value,
                term, 
                season, 
                individual_id) %>% unique()

dt_comp <- dt_est_1 %>% 
  left_join(dt_est_12) %>% 
  left_join(dt_est_3) %>%
  left_join(dt_est_24) %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "slope" ~ "Slope",
  )) %>% 
  as.data.table() %>% 
  filter(complete.cases(.))


p_est_1 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_12, y = estimate_1), alpha = 0.5) +
  geom_smooth(aes(x = estimate_12, y = estimate_1), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (12 hr steps)", y = "Estimate (1 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_12, dt_comp$estimate_1, method = "s"), 2))) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_1

p_est_2 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_12, y = estimate_3), alpha = 0.5) +
  geom_smooth(aes(x = estimate_12, y = estimate_3), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (12 hr steps)", y = "Estimate (3 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_12, dt_comp$estimate_3, method = "s"), 2))) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_2

p_est_3 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_12, y = estimate_24), alpha = 0.5) +
  geom_smooth(aes(x = estimate_12, y = estimate_24), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (12 hr steps)", y = "Estimate (24 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_12, dt_comp$estimate_24, method = "s"), 2))) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_3

p_est_4 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_3, y = estimate_1), alpha = 0.5) +
  geom_smooth(aes(x = estimate_3, y = estimate_1), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (3 hr steps)", y = "Estimate (1 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_3, dt_comp$estimate_1, method = "s"), 2))) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_4

p_est_5 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_3, y = estimate_24), alpha = 0.5) +
  geom_smooth(aes(x = estimate_3, y = estimate_24), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (3 hr steps)", y = "Estimate (24 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_3, dt_comp$estimate_24, method = "s"), 2))) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_5

p_est_6 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_1, y = estimate_24), alpha = 0.5) +
  geom_smooth(aes(x = estimate_1, y = estimate_24), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (1 hr steps)", y = "Estimate (24 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_1, dt_comp$estimate_24, method = "s"), 2))) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est_6

library(patchwork)
p_comb <- p_est_1 / p_est_2 / p_est_3 / p_est_4 / p_est_5 / p_est_6
p_comb

ggsave(plot =p_comb,
       "builds/plots/supplement/compare_estimates_diff_step_lengths.png",
       dpi = 600, height = 11, width = 10)



################## compare difference btw. sexes and seasons

dt_ele_meta <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")


dt_est <- fread("builds/model_outputs/issf_estimates_24hr_steps.csv") %>% 
  dplyr::select(estimate,
                std_error, 
                p_value,
                term, 
                sex,
                season, 
                ci_lb, 
                ci_ub,
                individual_id) %>% unique() %>% 
  left_join(dt_ele_meta)


# Test correlations across the dataset: 

dt_me_season_a = dt_est %>% 
  group_by(term, season) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            #  ci_lb = median_estimate - 1.96*std_error,
            #  ci_ub = median_estimate + 1.96*std_error, 
            ci_lb = median(ci_lb, na.rm = T),
            ci_ub = median(ci_ub, na.rm = T), 
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
  sig_me = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant")) %>% 
  mutate(season = case_when(
    season == "whole_year" ~ "Both", 
    season == "dry_season" ~ "Dry", 
    season == "wet_season" ~ "Wet"
  ))



dt_me_season_a %>%
  ungroup() %>%  
  pivot_wider(
    id_cols = term,
    names_from = season,
    values_from = median_estimate
  ) %>%
  summarise(
    spearman_rho = cor(Dry, Wet, method = "spearman"),
    p_value = cor.test(Dry, Wet, method = "spearman")$p.value
  )



dt_me_sex_a = dt_est %>% 
  filter(season == "whole_year") %>% 
  group_by(term, sex) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            #  ci_lb = median_estimate - 1.96*std_error,
            #  ci_ub = median_estimate + 1.96*std_error, 
            ci_lb = median(ci_lb, na.rm = T),
            ci_ub = median(ci_ub, na.rm = T), 
            p_value = median(p_value))  %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ))



dt_me_sex_a %>%
  select(clean_term, sex, median_estimate) %>%
  pivot_wider(
    id_cols = clean_term,
    names_from = sex,
    values_from = median_estimate
  ) %>%
  summarise(
    spearman_rho = cor(M, `F`, method = "spearman"),
    p_value = cor.test(M, `F`, method = "spearman")$p.value
  )





# Cluster specific correlations ----------------

dt_me_season = dt_est %>% 
  group_by(term, season, cluster_id) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            #  ci_lb = median_estimate - 1.96*std_error,
            #  ci_ub = median_estimate + 1.96*std_error, 
            ci_lb = median(ci_lb, na.rm = T),
            ci_ub = median(ci_ub, na.rm = T), 
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
  sig_me = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant")) %>% 
  mutate(season = case_when(
    season == "whole_year" ~ "Both", 
    season == "dry_season" ~ "Dry", 
    season == "wet_season" ~ "Wet"
  ))



dt_me_season %>%
  filter(cluster_id != "") %>% 
  select(cluster_id, term, season, median_estimate) %>%
  pivot_wider(
    names_from = season,
    values_from = median_estimate
  ) %>%
  group_by(cluster_id) %>%
  summarise(
    spearman_rho = cor(Dry, Both, method = "spearman"),
    p_value = cor.test(Dry, Both, method = "spearman")$p.value
  )

dt_me_season %>%
  filter(cluster_id != "") %>% 
  select(cluster_id, term, season, median_estimate) %>%
  pivot_wider(
    names_from = season,
    values_from = median_estimate
  ) %>%
  group_by(cluster_id) %>%
  summarise(
    spearman_rho = cor(Both, Wet, method = "spearman"),
    p_value = cor.test(Both, Wet, method = "spearman")$p.value
  )

dt_me_season %>%
  filter(cluster_id != "") %>% 
  select(cluster_id, term, season, median_estimate) %>%
  pivot_wider(
    names_from = season,
    values_from = median_estimate
  ) %>%
  group_by(cluster_id) %>%
  summarise(
    spearman_rho = cor(Dry, Wet, method = "spearman"),
    p_value = cor.test(Dry, Wet, method = "spearman")$p.value
  )




dt_me_sex = dt_est %>% 
  filter(season == "whole_year") %>% 
  group_by(term, sex, cluster_id) %>% 
  summarise(median_estimate = median(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            #  ci_lb = median_estimate - 1.96*std_error,
            #  ci_ub = median_estimate + 1.96*std_error, 
            ci_lb = median(ci_lb, na.rm = T),
            ci_ub = median(ci_ub, na.rm = T), 
            p_value = median(p_value))  %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ))



dt_me_sex %>%
  filter(cluster_id != "" & cluster_id != "zambezi") %>% 
  select(cluster_id, term, sex, median_estimate) %>%
  pivot_wider(
    names_from = sex,
    values_from = median_estimate
  ) %>%
  group_by(cluster_id) %>%
  summarise(
    spearman_rho = cor(M, `F`, method = "spearman"),
    p_value = cor.test(M, `F`, method = "spearman")$p.value
  )


