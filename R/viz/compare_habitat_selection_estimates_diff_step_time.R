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
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_12, dt_comp$estimate_1), 2))) +
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
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_12, dt_comp$estimate_3), 2))) +
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
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_12, dt_comp$estimate_24), 2))) +
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
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_3, dt_comp$estimate_1), 2))) +
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
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_3, dt_comp$estimate_24), 2))) +
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
       subtitle = paste0("corr = ", round(cor(dt_comp$estimate_1, dt_comp$estimate_24), 2))) +
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
