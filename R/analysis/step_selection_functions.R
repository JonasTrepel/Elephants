library(amt)
library(data.table)
library(tidyverse)
library(MuMIn)
library(broom)
library(MetBrewer)

#param = "1hr"
param = "12hrs"

if(param == "1hr"){
  
dt <- fread("data/processed_data/data_fragments/steps_1hr_habitat_covariates.csv")

} else if(param == "12hrs"){
  
  dt <- fread("data/processed_data/data_fragments/steps_12hrs_habitat_covariates.csv")
  
}

# Create correlation matrix
corr_vars <- dt %>%
  select(
    evi_mean,
    evi_dry_season, 
    evi_wet_season,
    distance_to_water_km,
    distance_to_settlement_km,
    human_modification,
    enerscape, 
    slope, 
    elevation
  )

corr_matrix <- cor(corr_vars, use = "complete.obs")

# Plot with ggcorrplot
ggcorrplot::ggcorrplot(corr_matrix, 
                       lab = TRUE, 
                       type = "lower", 
                       colors = c("blue", "white", "red"))


i = 0
dt_est <- data.frame()
for(id in unique(dt$individual_id)){
  
  dt_sub <- dt %>% filter(individual_id == id) 
  
  # Means
  evi_mean_mean <- mean(dt_sub$evi_mean, na.rm = TRUE)
  distance_to_water_km_mean <- mean(dt_sub$distance_to_water_km, na.rm = TRUE)
  distance_to_settlement_km_mean <- mean(dt_sub$distance_to_settlement_km, na.rm = TRUE)
  human_modification_mean <- mean(dt_sub$human_modification, na.rm = TRUE)
  enerscape_mean <- mean(dt_sub$enerscape, na.rm = TRUE)
  slope_mean <- mean(dt_sub$slope, na.rm = TRUE)
  
  # Ranges
  evi_mean_range <- max(dt_sub$evi_mean, na.rm = TRUE) - min(dt_sub$evi_mean, na.rm = TRUE)
  distance_to_water_km_range <- max(dt_sub$distance_to_water_km, na.rm = TRUE) - min(dt_sub$distance_to_water_km, na.rm = TRUE)
  distance_to_settlement_km_range <- max(dt_sub$distance_to_settlement_km, na.rm = TRUE) - min(dt_sub$distance_to_settlement_km, na.rm = TRUE)
  human_modification_range <- max(dt_sub$human_modification, na.rm = TRUE) - min(dt_sub$human_modification, na.rm = TRUE)
  enerscape_range <- max(dt_sub$enerscape, na.rm = TRUE) - min(dt_sub$enerscape, na.rm = TRUE)
  slope_range <- max(dt_sub$slope, na.rm = TRUE) - min(dt_sub$slope, na.rm = TRUE)

  dt_sub <- dt_sub %>%
    mutate(
      evi_mean = as.numeric(scale(evi_mean)),
      evi_dry_season = as.numeric(scale(evi_dry_season)),
      evi_wet_season = as.numeric(scale(evi_wet_season)),
      distance_to_water_km = as.numeric(scale(distance_to_water_km)),
      distance_to_settlement_km = as.numeric(scale(distance_to_settlement_km)),
      human_modification = as.numeric(scale(human_modification)),
      slope = as.numeric(scale(slope)),
      elevation = as.numeric(scale(elevation)),
      enerscape = as.numeric(scale(log(enerscape)))
    )
  
  issf <- amt::fit_issf(case_ ~ 
                          evi_mean +
                          distance_to_water_km +
                          distance_to_settlement_km +
                          human_modification +
                          enerscape +
                          slope +
                          #elevation:season + 
                          strata(step_id_),
                          data = dt_sub)
  summary(issf)

  m_tidy <- tidy(issf$model) %>% 
    rename(std_error = std.error, 
           p_value = p.value) %>% 
    mutate(individual_id = id, 
           sex = unique(dt_sub$sex),
           n_true = nrow(dt_sub[case_ == TRUE, ]), 
           n_total = nrow(dt_sub), 
           ci_lb = estimate - 1.96*std_error, 
           ci_ub = estimate + 1.96*std_error,
           # means
           evi_mean_mean = evi_mean_mean,
           distance_to_water_km_mean = distance_to_water_km_mean,
           distance_to_settlement_km_mean = distance_to_settlement_km_mean,
           human_modification_mean = human_modification_mean,
           enerscape_mean = enerscape_mean,
           slope_mean = slope_mean,
           # ranges
           evi_mean_range = evi_mean_range,
           distance_to_water_km_range = distance_to_water_km_range,
           distance_to_settlement_km_range = distance_to_settlement_km_range,
           human_modification_range = human_modification_range,
           enerscape_range = enerscape_range,
           slope_range = slope_range
           )

  dt_est <- rbind(m_tidy, dt_est)
  i = i+1
  print(paste0(id, " done (", i, "/", n_distinct(dt$individual_id), ")"))
  
}


dt_me <- dt_est %>% 
  group_by(term) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), 
            std_error = sd(estimate)/sqrt(n()), 
            ci_lb = mean_estimate - 1.96*std_error,
            ci_ub = mean_estimate + 1.96*std_error, 
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
  sig_me = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant"))


p_est <- dt_est %>% 
  left_join(dt_me[, c("term", "sig")]) %>% 
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
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = clean_term, y = estimate, fill = "all", alpha = sig), color = "grey") +
  geom_pointrange(data = dt_me,
                  aes(x = clean_term, y = mean_estimate, ymin = ci_lb, ymax = ci_ub,
                      shape = sig_me, color = "all", fill = "all"), 
                  position = position_dodge(width = 0.75),
                  size = 0.4, linewidth = 1.1
  ) + 
  scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
  scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
  theme_classic() +
  scale_color_manual(values = c("all" = "#fab255")) +
  scale_fill_manual(values = c("all" = "#fab255")) +
  labs(x = "Covariate", y = "Estimate", color = "Sex", fill = "Sex", 
       subtitle = paste0("n = ", n_distinct(dt_est$individual_id))) +
  theme(
    legend.position = "bottom",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip()
p_est

c(met.brewer(name = "Egypt"))


#Group by sex -----------
dt_me_sex <- dt_est %>% 
  group_by(term, sex) %>% 
  summarise(mean_estimate = mean(estimate, na.rm = T), 
         std_error = sd(estimate)/sqrt(n()), 
         ci_lb = mean_estimate - 1.96*std_error,
         ci_ub = mean_estimate + 1.96*std_error,
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
  sig_me = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant"))



p_est_sex <- dt_est %>% 
  left_join(dt_me_sex[, c("term", "sex", "sig")]) %>% 
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
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(aes(x = clean_term, y = estimate, fill = sex, alpha = sig), color = "grey") +
  geom_pointrange(data = dt_me_sex,
                  aes(x = clean_term, y = mean_estimate, ymin = ci_lb, ymax = ci_ub,
                      color = sex, fill = sex, shape = sig_me), 
                  position = position_dodge(width = 0.75),
                  size = 0.4, linewidth = 1.1
  ) + 
  theme_classic() +
  scale_color_met_d(name = "Egypt") +
  scale_fill_met_d(name = "Egypt") +
  scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
  scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
  labs(x = "Covariate", y = "Estimate", color = "Sex", fill = "Sex", 
       subtitle = paste0("n (M) = ", n_distinct(dt_est[dt_est$sex == "M", ]$individual_id),
       "; n (F) = ",  n_distinct(dt_est[dt_est$sex == "F", ]$individual_id),
       "; n (U) = ",  n_distinct(dt_est[dt_est$sex == "U", ]$individual_id))) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
   # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip()
p_est_sex

p_comb <- gridExtra::grid.arrange(p_est, p_est_sex, ncol = 2, widths = c(1.5, 1))


# Look at p value vs range and estimate vs term mean 
p_p <- dt_est %>% 
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
    term == "enerscape" ~ enerscape_range,
    term == "slope" ~ slope_range
  )) %>% 
  ggplot() + 
  geom_point(aes(x = term_range, y = p_value)) +
  geom_smooth(aes(x = term_range, y = p_value), method = "lm") +
  facet_wrap(~clean_term, scales = "free", ncol = 3)
  
p_p
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
    term == "enerscape" ~ enerscape_mean,
    term == "slope" ~ slope_mean
  )) %>% 
  ggplot() + 
  geom_point(aes(x = term_mean, y = estimate)) +
  geom_smooth(aes(x = term_mean, y = estimate), method = "lm") +
  facet_wrap(~clean_term, scales = "free", ncol = 3)

p_est_tm

p_meta <- gridExtra::grid.arrange(p_p, p_est_tm)

if(param == "1hr"){
  
  fwrite(dt_est, "builds/model_outputs/issf_estimates_1hr_steps.csv")
  ggsave(plot = p_comb, "builds/plots/elephants_issf_estimates_1hr_steps.png",
         dpi = 600, height = 4.5, width = 9)
  
  ggsave(plot = p_meta, "builds/plots/exploratory/elephants_issf_stats_1hr_steps.png",
         dpi = 600, height = 10, width = 9)
  
} else if(param == "12hrs"){
  
  fwrite(dt_est, "builds/model_outputs/issf_estimates_12hrs_steps.csv")
  ggsave(plot = p_comb, "builds/plots/elephants_issf_estimates_12hrs_steps.png",
         dpi = 600, height = 4.5, width = 9)
  
  ggsave(plot = p_meta, "builds/plots/exploratory/elephants_issf_stats_12hrs_steps.png",
         dpi = 600, height = 10, width = 9)
  
}

      