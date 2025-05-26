library(terra)
library(sf)
library(exactextractr)


dt_est_1 <- fread("builds/model_outputs/issf_estimates_1hr_steps.csv")  %>% 
  rename(estimate_1 = estimate,
         std_error_1 = std_error, 
         p_value_1 = p_value)
dt_est_12 <- fread("builds/model_outputs/issf_estimates_12hrs_steps.csv") %>% 
  rename(estimate_12 = estimate,
         std_error_12 = std_error, 
         p_value_12 = p_value)

dt_comp <- dt_est_1 %>% 
  dplyr::select(term, estimate_1, std_error_1, p_value_1, individual_id) %>% 
  left_join(dt_est_12[, c("term", "estimate_12", "std_error_12", "p_value_12", "individual_id")]) %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ))


p_est <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = estimate_12, y = estimate_1), alpha = 0.5) +
  geom_smooth(aes(x = estimate_12, y = estimate_1), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Estimate (12 hr steps)", y = "Estimate (1 hr steps)")
p_est


p_p <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = p_value_12, y = p_value_1), alpha = 0.5) +
  geom_smooth(aes(x = p_value_12, y = p_value_1), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "P value (12 hr steps)", y = "P value (1 hr steps)")
p_p


p_se <- dt_comp %>% 
  ggplot() +
  geom_abline(linetype = "dashed") +
  geom_point(aes(x = std_error_12, y = std_error_1), alpha = 0.5) +
  geom_smooth(aes(x = std_error_12, y = std_error_1), method = "lm", color = "olivedrab") +
  facet_wrap(~clean_term, scales = "free", ncol = 6) +
  labs(x = "Std. Error (12 hr steps)", y = "Std. Error (1 hr steps)")
p_se


p_comp <- gridExtra::grid.arrange(p_est, p_p, p_se)
ggsave(plot =p_comp,
       "builds/plots/supplement/compare_model_results_1hr_and_12hr_steps.png",
       dpi = 600, height = 6.5, width = 12)
