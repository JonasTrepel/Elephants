library(tidylog)
library(MASS)
library(data.table)
library(tidyverse)
library(MuMIn)
library(broom)
library(MetBrewer)
library(performance)



dt <- fread("data/processed_data/data_fragments/individual_grids_with_covariates.csv") %>%
  filter(!individual_id %in% c("EF0108"))

quantile(dt$rel_occ)

# Create correlation matrix
corr_vars <- dt %>%
  dplyr::select(
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
  colors = c("blue", "white", "red")
)



dt %>% ggplot() +
  geom_density(aes(x = rel_occ, color = park_id)) +
  theme(legend.position = "none") +
  xlim(0, 1)


dt_est <- data.table()
for (id in unique(dt$individual_id)) {
  dt_sub <- dt %>%
    filter(individual_id == id) %>%
    filter(rel_occ > 0)

  # Means
  evi_mean_mean <- mean(dt_sub$evi_mean, na.rm = TRUE)
  distance_to_water_km_mean <- mean(dt_sub$distance_to_water_km, na.rm = TRUE)
  distance_to_settlement_km_mean <- mean(dt_sub$distance_to_settlement_km, na.rm = TRUE)
  human_modification_mean <- mean(dt_sub$human_modification, na.rm = TRUE)
  slope_mean <- mean(dt_sub$slope, na.rm = TRUE)

  # Ranges
  evi_mean_range <- max(dt_sub$evi_mean, na.rm = TRUE) - min(dt_sub$evi_mean, na.rm = TRUE)
  distance_to_water_km_range <- max(dt_sub$distance_to_water_km, na.rm = TRUE) - min(dt_sub$distance_to_water_km, na.rm = TRUE)
  distance_to_settlement_km_range <- max(dt_sub$distance_to_settlement_km, na.rm = TRUE) - min(dt_sub$distance_to_settlement_km, na.rm = TRUE)
  human_modification_range <- max(dt_sub$human_modification, na.rm = TRUE) - min(dt_sub$human_modification, na.rm = TRUE)
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

  # modeling time!
  m_wy <- tryCatch(
    {
      glm.nb(
    n_points ~
      evi_mean +
      distance_to_water_km +
      distance_to_settlement_km +
      human_modification +
      slope,
    data = dt_sub
  )
      },
  error = function(e) {
    message("An error occurred while fitting the model wy: ", e$message)
    return(NULL)  # or you could return a default object or custom error object
  }
  )
  summary(m_wy)
  r2_wy <- performance::r2(m_wy)
  r2_wy$R2_Nagelkerke
  
  quantile(dt_sub$n_points_dry)


  m_dry <- tryCatch(
    {
      glm.nb(
        n_points_dry ~
          evi_mean +
          distance_to_water_km +
          distance_to_settlement_km +
          human_modification +
          slope,
        data = dt_sub
      )
    },
    error = function(e) {
      message("An error occurred while fitting the model dry: ", e$message)
      return(NULL)  # or you could return a default object or custom error object
    }
  )
  summary(m_dry)
  r2_dry <- performance::r2(m_dry)
  r2_dry$R2_Nagelkerke

  m_wet <- tryCatch(
    {
      glm.nb(
        n_points_wet ~
          evi_mean +
          distance_to_water_km +
          distance_to_settlement_km +
          human_modification +
          slope,
        data = dt_sub
      )
    },
    error = function(e) {
      message("An error occurred while fitting the model wet: ", e$message)
      return(NULL)  # or you could return a default object or custom error object
    }
  )
  summary(m_wet)
  r2_wet <- performance::r2(m_wet)
  r2_wet$R2_Nagelkerke

  plot(dt_sub$rel_occ_dry, dt_sub$rel_occ_wet)

  if(!is.null(m_wy)){
  m_tidy_wy <- tidy(m_wy) %>%
    rename(
      std_error = std.error,
      p_value = p.value
    ) %>%
    mutate(season = "whole_year", 
           r_sq = r2_wy$R2_Nagelkerke)}else{m_tidy_dry <- data.frame()}

  if(!is.null(m_dry)){
  m_tidy_dry <- tidy(m_dry) %>%
    rename(
      std_error = std.error,
      p_value = p.value
    ) %>%
    mutate(season = "dry_season", 
           r_sq = r2_dry$R2_Nagelkerke)}else{m_tidy_dry <- data.frame()}

  if(!is.null(m_wet)){
    m_tidy_wet <- tidy(m_wet) %>%
      rename(
        std_error = std.error,
        p_value = p.value
      ) %>%
      mutate(season = "wet_season", 
             r_sq = r2_wet$R2_Nagelkerke)}else{m_tidy_wet <- data.frame()}


  m_tidy <- rbind(m_tidy_wy, m_tidy_dry, m_tidy_wet) %>%
    mutate(
      individual_id = id,
      sex = unique(dt_sub$sex),
      n_total = nrow(dt_sub),
      ci_lb = estimate - 1.96 * std_error,
      ci_ub = estimate + 1.96 * std_error,
      # means
      evi_mean_mean = evi_mean_mean,
      distance_to_water_km_mean = distance_to_water_km_mean,
      distance_to_settlement_km_mean = distance_to_settlement_km_mean,
      human_modification_mean = human_modification_mean,
      slope_mean = slope_mean,
      # ranges
      evi_mean_range = evi_mean_range,
      distance_to_water_km_range = distance_to_water_km_range,
      distance_to_settlement_km_range = distance_to_settlement_km_range,
      human_modification_range = human_modification_range,
      slope_range = slope_range,
    )

  i <- i + 1
  print(paste0(id, " done (", i, "/", n_distinct(dt$individual_id), ")"))


  dt_est <- rbind(dt_est, m_tidy)
}

dt_est_2 <- dt_est %>% filter(!grepl("Intercept", term))

dt_me <- dt_est_2 %>% 
  group_by(term, season) %>% 
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


p_est <- dt_est_2 %>% 
  left_join(dt_me[, c("term", "sig", "season")]) %>% 
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
  geom_boxplot(aes(x = clean_term, y = estimate, color = season, fill = season, alpha = sig)) +
  geom_pointrange(data = dt_me,
                  aes(x = clean_term, y = mean_estimate, ymin = ci_lb, ymax = ci_ub,
                      shape = sig_me, color = season, fill = season), 
                  position = position_dodge(width = 0.75),
                  size = 0.4, linewidth = 1.1
  ) + 
  scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
  scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
  theme_classic() +
  scale_color_met_d(name = "Egypt", direction = 1) +
  scale_fill_met_d(name = "Egypt", direction = 1) +
  labs(x = "Covariate", y = "Estimate", color = "Season", fill = "Season", 
       subtitle = paste0("n = ", n_distinct(dt_est_2$individual_id))) +
  guides(
    fill = guide_legend(nrow = 2),
    color = guide_legend(nrow = 2)
  ) +
  theme(
    legend.position = "bottom",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip()
p_est

c(met.brewer(name = "Egypt"))


#Group by sex -----------
dt_me_sex <- dt_est_2 %>% 
  mutate(sex_season = paste0(sex, " in ", season)) %>%
  group_by(term, sex_season, sex, season) %>% 
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



p_est_sex <- dt_est_2 %>% 
  left_join(dt_me_sex[, c("term", "sex", "sig", "season", "sex_season")]) %>% 
  filter(!sex == "U") %>% 
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
  geom_boxplot(aes(x = clean_term, y = estimate, color = sex_season, fill = sex_season, alpha = sig)) +
  geom_pointrange(data = dt_me_sex %>% filter(!sex == "U") ,
                  aes(x = clean_term, y = mean_estimate, ymin = ci_lb, ymax = ci_ub,
                      color = sex_season, fill = sex_season, shape = sig_me), 
                  position = position_dodge(width = 0.75),
                  size = 0.4, linewidth = 1.1
  ) + 
  theme_classic() +
  scale_color_met_d(name = "Cassatt2", direction = -1) +
  scale_fill_met_d(name = "Cassatt2", direction = -1) +
  scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
  scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
  labs(x = "Covariate", y = "Estimate", color = "Sex / Season", fill = "Sex / Season", 
       subtitle = paste0("n (M) = ", n_distinct(dt_est_2[dt_est_2$sex == "M", ]$individual_id),
                         "; n (F) = ",  n_distinct(dt_est_2[dt_est_2$sex == "F", ]$individual_id),
                         "; n (U) = ",  n_distinct(dt_est_2[dt_est_2$sex == "U", ]$individual_id))) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
    # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip()
p_est_sex

p_comb <- gridExtra::grid.arrange(p_est, p_est_sex, ncol = 2, widths = c(1.4, 1))


# Look at p value vs range and estimate vs term mean
p_p <- dt_est_2 %>%
  mutate(
    clean_term = case_when(
      .default = term,
      term == "evi_mean" ~ "EVI",
      term == "distance_to_water_km" ~ "Distance to Water",
      term == "distance_to_settlement_km" ~ "Distance to Settlement",
      term == "human_modification" ~ "Human Modification Index",
      term == "slope" ~ "Slope",
    ),
    term_range = case_when(
      term == "evi_mean" ~ evi_mean_range,
      term == "distance_to_water_km" ~ distance_to_water_km_range,
      term == "distance_to_settlement_km" ~ distance_to_settlement_km_range,
      term == "human_modification" ~ human_modification_range,
      term == "slope" ~ slope_range
    )
  ) %>%
  ggplot() +
  geom_point(aes(x = term_range, y = p_value)) +
  geom_smooth(aes(x = term_range, y = p_value), method = "lm") +
  facet_wrap(~clean_term, scales = "free", ncol = 3)

p_p
p_est_tm <- dt_est_2 %>%
  mutate(
    clean_term = case_when(
      .default = term,
      term == "evi_mean" ~ "EVI",
      term == "distance_to_water_km" ~ "Distance to Water",
      term == "distance_to_settlement_km" ~ "Distance to Settlement",
      term == "human_modification" ~ "Human Modification Index",
      term == "slope" ~ "Slope",
    ),
    term_mean = case_when(
      term == "evi_mean" ~ evi_mean_mean,
      term == "distance_to_water_km" ~ distance_to_water_km_mean,
      term == "distance_to_settlement_km" ~ distance_to_settlement_km_mean,
      term == "human_modification" ~ human_modification_mean,
      term == "slope" ~ slope_mean
    )
  ) %>%
  ggplot() +
  geom_point(aes(x = term_mean, y = estimate)) +
  geom_smooth(aes(x = term_mean, y = estimate), method = "lm") +
  facet_wrap(~clean_term, scales = "free", ncol = 3)

p_est_tm

p_meta <- gridExtra::grid.arrange(p_p, p_est_tm)

  fwrite(dt_est_2, "builds/model_outputs/elephants_individual_grid_estimates_glm_nb.csv")
  ggsave(
    plot = p_comb, "builds/plots/elephants_individual_grid_estimates_glm_nb.png",
    dpi = 600, height = 6, width = 12
  )
