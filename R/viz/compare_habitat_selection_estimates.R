library(terra)
library(sf)
library(exactextractr)


dt_est_1 <- fread("builds/model_outputs/issf_estimates_1hr_steps.csv")  %>% 
  dplyr::select(estimate_1 = estimate,
         std_error_1 = std_error, 
         p_value_1 = p_value, 
         term, 
         season, 
         individual_id) %>% unique()

table(dt_est_1[dt_est_1$season == "whole_year", ]$term)

dt_est_3 <- fread("builds/model_outputs/issf_estimates_3hrs_steps.csv") %>% 
  dplyr::select(estimate_3 = estimate,
         std_error_3 = std_error, 
         p_value_3 = p_value,
         term, 
         season, 
         individual_id) %>% unique()
dt_est_12 <- fread("builds/model_outputs/issf_estimates_12hrs_steps.csv") %>% 
  dplyr::select(estimate_12 = estimate,
         std_error_12 = std_error, 
         p_value_12 = p_value,
         term, 
         season, 
         individual_id) %>% unique()

dt_est_ig <- fread("builds/model_outputs/elephants_individual_grid_estimates_glm_nb.csv") %>% 
  dplyr::select(estimate_ig = estimate,
         std_error_ig = std_error, 
         p_value_ig = p_value,
         term, 
         season, 
         individual_id) %>% unique()


dt_comp <- dt_est_1 %>% 
  left_join(dt_est_12) %>% 
  left_join(dt_est_3) %>%
  left_join(dt_est_ig) %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "slope" ~ "Slope",
  )) %>% 
  as.data.table()


p_est_1 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_12, y = estimate_1), alpha = 0.5) +
  geom_smooth(aes(x = estimate_12, y = estimate_1), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (12 hr steps)", y = "Estimate (1 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_12, dt_comp$estimate_1, na.rm = T), 2)))
p_est_1

p_est_2 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_12, y = estimate_3), alpha = 0.5) +
  geom_smooth(aes(x = estimate_12, y = estimate_3), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (12 hr steps)", y = "Estimate (3 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_12, dt_comp$estimate_3), 2)))
p_est_2

p_est_3 <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_1, y = estimate_3), alpha = 0.5) +
  geom_smooth(aes(x = estimate_1, y = estimate_3), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (1 hr steps)", y = "Estimate (3 hr steps)", 
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_1, dt_comp$estimate_3), 2)))
p_est_3

cor.test(dt_comp[season == "whole_year", ]$estimate_3, dt_comp[season == "whole_year", ]$estimate_1)
cor.test(dt_comp[, ]$estimate_3, dt_comp[, ]$estimate_1)
cor.test(dt_comp$estimate_3, dt_comp$estimate_12)
cor.test(dt_comp$estimate_12, dt_comp$estimate_1)

p_comp <- gridExtra::grid.arrange(p_est_1, p_est_2, p_est_3)
ggsave(plot =p_comp,
       "builds/plots/supplement/compare_model_results_diff_time_steps.png",
       dpi = 600, height = 8, width = 12)


p_est_1_ig <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_ig, y = estimate_1), alpha = 0.5) +
  geom_smooth(aes(x = estimate_ig, y = estimate_1), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (individual grids)", y = "Estimate (1 hr steps)", 
       subtitle = paste0("corr = ", round(cor.test(dt_comp$estimate_ig, dt_comp$estimate_1, na.rm = T)$estimate, 2)))
p_est_1_ig

p_est_2_ig <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_ig, y = estimate_3), alpha = 0.5) +
  geom_smooth(aes(x = estimate_ig, y = estimate_3), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (individual grids)", y = "Estimate (3 hr steps)", 
       subtitle = paste0("corr = ", round(cor.test(dt_comp$estimate_ig, dt_comp$estimate_3, na.rm = T)$estimate, 2)))
p_est_2_ig

p_est_3_ig <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_ig, y = estimate_12), alpha = 0.5) +
  geom_smooth(aes(x = estimate_ig, y = estimate_12), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (individual grids)", y = "Estimate (12 hr steps)", 
       subtitle = paste0("corr = ", round(cor.test(dt_comp$estimate_ig, dt_comp$estimate_12, na.rm = T)$estimate, 2)))
p_est_3_ig

p_comp_ig <- gridExtra::grid.arrange(p_est_1_ig, p_est_2_ig, p_est_3_ig)
ggsave(plot =p_comp_ig,
       "builds/plots/supplement/compare_model_results_ssf_vs_individual_grids.png",
       dpi = 600, height = 8, width = 12)
