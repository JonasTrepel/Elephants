  library(amt)
  library(data.table)
  library(tidyverse)
  library(MuMIn)
  library(broom)
  library(MetBrewer)
  library(survival)
  #param = "1hr"
  #param = "3hrs"
  param = "12hrs"
  #param = "24hrs"
  
  
  params <- c("1hr", 
               "3hrs", 
               "12hrs",
               "24hrs"
               )
  
  for(param in unique(params)){ 
  
  if(param == "1hr"){
    
  dt <- fread("data/processed_data/data_fragments/steps_1hr_habitat_covariates.csv")
  mean(dt$sl_) 
  quantile(dt$sl_)
  
  } else if(param == "3hrs"){
    
    dt <- fread("data/processed_data/data_fragments/steps_3hrs_habitat_covariates.csv")
    mean(dt$sl_) 
    quantile(dt$sl_)
    
  } else if(param == "12hrs"){
    
  dt <- fread("data/processed_data/data_fragments/steps_12hrs_habitat_covariates.csv")
  mean(dt$sl_) #2718.854
  quantile(dt$sl_)
  # 0%      25%      50%      75%     100% 
  #0.000 1254.603 2334.965 3865.864 8661.769
  } else if(param == "24hrs"){
    
    dt <- fread("data/processed_data/data_fragments/steps_24hrs_habitat_covariates.csv")
    mean(dt$sl_) 
    quantile(dt$sl_)
    
  }
  
  n_distinct(dt$individual_id)
  dt = dt %>% group_by(individual_id) %>% filter(sum(case_) > 365) %>% ungroup()
  n_distinct(dt$individual_id)
  
  # Create correlation matrix
  corr_vars <- dt %>%
    select(
      evi_mean,
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
                         colors = c("skyblue", "white", "darkorange"))
  
  
  
  dt_seas <- data.frame(season = c("wet_season", "dry_season", "whole_year"))
  
  dt_est <- data.frame()
  
  for(season in unique(dt_seas$season)){
    
  if(season == "whole_year"){
    dt_se <- dt
  }else if(season == "dry_season"){
    dt_se <- dt %>% filter(season == "dry_season") #%>% filter(month %in% c(8, 9))
  }else if(season == "wet_season"){
    dt_se <- dt %>% filter(season == "wet_season")
  }else
  
  
  print(paste0("Now running models for: ", season))
    
  i = 0
  dt_est_se <- data.frame()
  for(id in unique(dt$individual_id)){
    
    dt_sub <- dt_se %>% filter(individual_id == id) 
    
    if(nrow(dt_sub[dt_sub$case_ == TRUE, ]) < 200){next}
    
    # Means
    evi_mean_mean <- mean(dt_sub$evi_mean, na.rm = TRUE)
    distance_to_water_km_mean <- mean(dt_sub$distance_to_water_km, na.rm = TRUE)
    distance_to_settlement_km_mean <- mean(dt_sub$distance_to_settlement_km, na.rm = TRUE)
    human_modification_mean <- mean(dt_sub$human_modification, na.rm = TRUE)
   # enerscape_mean <- mean(dt_sub$enerscape, na.rm = TRUE)
    slope_mean <- mean(dt_sub$slope, na.rm = TRUE)
    
    # Ranges
    evi_mean_range <- max(dt_sub$evi_mean, na.rm = TRUE) - min(dt_sub$evi_mean, na.rm = TRUE)
    distance_to_water_km_range <- max(dt_sub$distance_to_water_km, na.rm = TRUE) - min(dt_sub$distance_to_water_km, na.rm = TRUE)
    distance_to_settlement_km_range <- max(dt_sub$distance_to_settlement_km, na.rm = TRUE) - min(dt_sub$distance_to_settlement_km, na.rm = TRUE)
    human_modification_range <- max(dt_sub$human_modification, na.rm = TRUE) - min(dt_sub$human_modification, na.rm = TRUE)
   # enerscape_range <- max(dt_sub$enerscape, na.rm = TRUE) - min(dt_sub$enerscape, na.rm = TRUE)
    slope_range <- max(dt_sub$slope, na.rm = TRUE) - min(dt_sub$slope, na.rm = TRUE)
  
    dt_sub <- dt_sub %>%
      mutate(
        evi_mean = as.numeric(scale(evi_mean)),
        distance_to_water_km = as.numeric(scale(distance_to_water_km)),
        distance_to_settlement_km = as.numeric(scale(distance_to_settlement_km)),
        human_modification = as.numeric(scale(human_modification)),
        slope = as.numeric(scale(slope)),
        elevation = as.numeric(scale(elevation))#,  enerscape = as.numeric(scale(log(enerscape)))
      )
    
    issf <- tryCatch(
      {
        amt::fit_issf(case_ ~
                        evi_mean +
                        distance_to_water_km +
                        distance_to_settlement_km +
                        human_modification +
                        # enerscape +
                        slope +
                        strata(step_id_),
                      data = dt_sub)
      },
      error = function(e) {
        message("An error occurred while fitting the ISSF model: ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(issf)){next}
    
    summary(issf)
  
    m_tidy <- tidy(issf$model) %>% 
      rename(std_error = std.error, 
             p_value = p.value) %>% 
      mutate(individual_id = id, 
             sex = unique(dt_sub$sex),
             n_true = nrow(dt_sub[dt_sub$case_ == TRUE, ]), 
             n_total = nrow(dt_sub), 
             ci_lb = estimate - 1.96*std_error, 
             ci_ub = estimate + 1.96*std_error,
             # means
             evi_mean_mean = evi_mean_mean,
             distance_to_water_km_mean = distance_to_water_km_mean,
             distance_to_settlement_km_mean = distance_to_settlement_km_mean,
             human_modification_mean = human_modification_mean,
            # enerscape_mean = enerscape_mean,
             slope_mean = slope_mean,
             # ranges
             evi_mean_range = evi_mean_range,
             distance_to_water_km_range = distance_to_water_km_range,
             distance_to_settlement_km_range = distance_to_settlement_km_range,
             human_modification_range = human_modification_range,
          #   enerscape_range = enerscape_range,
             slope_range = slope_range, 
             season = season
             )
  
    dt_est_se <- rbind(m_tidy, dt_est_se)
    i = i+1
    print(paste0(id, " done (", i, "/", n_distinct(dt$individual_id), ")"))
    
  }
  
  dt_est <- rbind(dt_est, dt_est_se)
  
  }
  
  
  dt_me <- dt_est %>% 
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
  
  
  p_est_spread <- dt_est %>% 
    mutate(season = case_when(
      season == "whole_year" ~ "Both", 
      season == "dry_season" ~ "Dry", 
      season == "wet_season" ~ "Wet"
    )) %>% 
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
    geom_jitter(aes(x = clean_term, y = estimate, color = season, alpha = sig), 
                alpha = 0.1, position = position_dodge(width = 0.75)) +
    geom_pointrange(data = dt_me,
                    aes(x = clean_term, y = median_estimate, ymin = ci_lb, ymax = ci_ub,
                        shape = sig_me, fill = season), 
                    position = position_dodge(width = 0.75),
                    size = 0.8, linewidth = 1.3, stroke = 1.3 
    ) + 
    scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
    scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
    theme_classic() +
    scale_color_met_d(name = "Egypt", direction = 1) +
    scale_fill_met_d(name = "Egypt", direction = 1) +
    labs(x = "Covariate", y = "Estimate", color = "Season", fill = "Season", 
         subtitle = paste0("n = ", n_distinct(dt_est$individual_id))) +
    guides(
      fill = guide_legend(override.aes = list(shape = 21), nrow = 2),
      color = guide_legend(nrow = 2)
    ) +
    theme(
      legend.position = "bottom",
      # axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    coord_flip()
  p_est_spread
  
  
  p_est <- dt_est %>% 
    mutate(season = case_when(
      season == "whole_year" ~ "Both", 
      season == "dry_season" ~ "Dry", 
      season == "wet_season" ~ "Wet"
    )) %>% 
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
    geom_pointrange(data = dt_me,
                    aes(x = clean_term, y = median_estimate, ymin = ci_lb, ymax = ci_ub,
                        shape = sig_me, color = season, fill = season), 
                    position = position_dodge(width = 0.75),
                    size = 0.8, linewidth = 1.3
    ) + 
    scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
    scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
    theme_classic() +
    scale_color_met_d(name = "Egypt", direction = 1) +
    scale_fill_met_d(name = "Egypt", direction = 1) +
    labs(x = "Covariate", y = "Estimate", color = "Season", fill = "Season", 
         subtitle = paste0("n = ", n_distinct(dt_est$individual_id))) +
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
  
  # Group by sex ---------
  dt_me_sex <- dt_est %>% 
    filter(season == "whole_year") %>% 
    group_by(term, sex) %>% 
    summarise(median_estimate = median(estimate, na.rm = T), 
              std_error = sd(estimate)/sqrt(n()), 
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
    sig_me = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant"))
  
  
  
  p_est_sex_spread <- dt_est %>% 
    left_join(dt_me_sex[, c("term", "sex", "sig")]) %>% 
    filter(!sex == "U" & season == "whole_year") %>% 
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
    geom_jitter(aes(x = clean_term, y = estimate, color = sex, alpha = sig),
                alpha = 0.1, position = position_dodge(width = 0.75)) +
    geom_pointrange(data = dt_me_sex %>% filter(!sex == "U") ,
                    aes(x = clean_term, y = median_estimate,
                        ymin = ci_lb, ymax = ci_ub, fill = sex, shape = sig_me), 
                    position = position_dodge(width = 0.75),
                    size = 0.7, linewidth = 1.1, stroke = 1.2 
    ) + 
    theme_classic() +
    scale_color_met_d(name = "Isfahan1", direction = -1) +
    scale_fill_met_d(name = "Isfahan1", direction = -1) +
    scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
    scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
    labs(x = "Covariate", y = "Estimate", color = "Sex", fill = "Sex", 
         subtitle = paste0("Male n = ", n_distinct(dt_est[dt_est$sex == "M", ]$individual_id),
                           "; Female n = ",  n_distinct(dt_est[dt_est$sex == "F", ]$individual_id))) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
      # axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(
      fill = guide_legend(override.aes = list(shape = 21), nrow = 2), 
      color = guide_legend(nrow = 2)
    ) +
    coord_flip()
  p_est_sex_spread
  
  
  p_est_sex <- dt_est %>% 
    left_join(dt_me_sex[, c("term", "sex", "sig")]) %>% 
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
    geom_pointrange(data = dt_me_sex %>% filter(!sex == "U") ,
                    aes(x = clean_term, y = median_estimate,
                        ymin = ci_lb, ymax = ci_ub,
                        fill = sex, color = sex, shape = sig_me), 
                    position = position_dodge(width = 0.75),
                    size = 0.7, linewidth = 1.1
    ) + 
    theme_classic() +
    scale_color_met_d(name = "Isfahan1", direction = -1) +
    scale_fill_met_d(name = "Isfahan1", direction = -1) +
    scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
    scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
    labs(x = "Covariate", y = "Estimate", color = "Sex", fill = "Sex", 
         subtitle = paste0("Male n = ", n_distinct(dt_est[dt_est$sex == "M", ]$individual_id),
                           "; Female n = ",  n_distinct(dt_est[dt_est$sex == "F", ]$individual_id))) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
      # axis.text.x = element_text(angle = 45, hjust = 1) 
    ) +
    guides(
      color = guide_legend(nrow = 2)
    ) +
    coord_flip()
  p_est_sex
  
  #Group by sex & season -----------
  dt_me_sex_season <- dt_est %>% 
    mutate(season = case_when(
      season == "whole_year" ~ "Both", 
      season == "dry_season" ~ "Dry", 
      season == "wet_season" ~ "Wet"
    )) %>% 
    mutate(sex_season = paste0(sex, " in ", season)) %>%
    group_by(term, sex_season, sex, season) %>% 
    summarise(median_estimate = median(estimate, na.rm = T), 
           std_error = sd(estimate)/sqrt(n()), 
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
    sig_me = ifelse(ci_lb > 0 | ci_ub < 0, "significant", "non-significant"))
  
  
  
  p_est_sex_season_spread <- dt_est %>% 
    mutate(season = case_when(
      season == "whole_year" ~ "Both", 
      season == "dry_season" ~ "Dry", 
      season == "wet_season" ~ "Wet"
    )) %>% 
    left_join(dt_me_sex_season[, c("term", "sex", "sig", "season", "sex_season")]) %>% 
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
    geom_jitter(aes(x = clean_term, y = estimate, color = sex_season, alpha = sig),
                alpha = 0.1, position = position_dodge(width = 0.75)) +
    geom_pointrange(data = dt_me_sex_season %>% filter(!sex == "U") ,
                    aes(x = clean_term, y = median_estimate, ymin = ci_lb, ymax = ci_ub, fill = sex_season, shape = sig_me), 
                    position = position_dodge(width = 0.75),
                    size = 0.7, linewidth = 1.1, stroke = 1.2 
    ) + 
    theme_classic() +
    scale_color_met_d(name = "Cassatt2", direction = -1) +
    scale_fill_met_d(name = "Cassatt2", direction = -1) +
    scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
    scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
    labs(x = "Covariate", y = "Estimate", color = "Sex / Season", fill = "Sex / Season", 
         subtitle = paste0("Male n) = ", n_distinct(dt_est[dt_est$sex == "M", ]$individual_id),
         "; Female n = ",  n_distinct(dt_est[dt_est$sex == "F", ]$individual_id))) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
     # axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(
      fill = guide_legend(override.aes = list(shape = 21), nrow = 2)
    ) +
    coord_flip()
  p_est_sex_season_spread
  
  
  p_est_sex_season <- dt_est %>% 
    mutate(season = case_when(
      season == "whole_year" ~ "Both", 
      season == "dry_season" ~ "Dry", 
      season == "wet_season" ~ "Wet"
    )) %>% 
    left_join(dt_me_sex_season[, c("term", "sex", "sig", "season", "sex_season")]) %>% 
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
    geom_pointrange(data = dt_me_sex_season %>% filter(!sex == "U") ,
                    aes(x = clean_term, y = median_estimate,
                        ymin = ci_lb, ymax = ci_ub,
                        fill = sex_season, color = sex_season, shape = sig_me), 
                    position = position_dodge(width = 0.75),
                    size = 0.7, linewidth = 1.1
    ) + 
    theme_classic() +
    scale_color_met_d(name = "Cassatt2", direction = -1) +
    scale_fill_met_d(name = "Cassatt2", direction = -1) +
    scale_alpha_manual(values = c("significant" = 0.5, "non-significant" = 0.1), guide = "none") +
    scale_shape_manual(values = c("significant" = 23, "non-significant" = 21), guide = "none") +
    labs(x = "Covariate", y = "Estimate", color = "Sex / Season", fill = "Sex / Season", 
         subtitle = paste0("Male n = ", n_distinct(dt_est[dt_est$sex == "M", ]$individual_id),
                           "; Female n = ",  n_distinct(dt_est[dt_est$sex == "F", ]$individual_id))) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
      # axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    coord_flip()
  p_est_sex_season
  
  
  # Combine 
  p_comb_spread <- gridExtra::grid.arrange(p_est_spread,
                                           p_est_sex_spread,
                                           p_est_sex_season_spread,
                                           ncol = 3,
                                           widths = c(1.4, 1, 1))
  p_comb <- gridExtra::grid.arrange(p_est,
                                    p_est_sex,
                                    p_est_sex_season,
                                    ncol = 3, 
                                    widths = c(1.4, 1, 1))
  
  
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
   #   term == "enerscape" ~ enerscape_range,
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
   #   term == "enerscape" ~ enerscape_mean,
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
           dpi = 600, height = 6, width = 13)
    
    ggsave(plot = p_comb_spread, "builds/plots/elephants_issf_estimates_1hr_steps_spread.png",
           dpi = 600, height = 6, width = 13)
  
    
    ggsave(plot = p_meta, "builds/plots/exploratory/elephants_issf_stats_1hr_steps.png",
           dpi = 600, height = 10, width = 9)
    
  } else if(param == "3hrs"){
    
    fwrite(dt_est, "builds/model_outputs/issf_estimates_3hr_steps.csv")
    
    ggsave(plot = p_comb, "builds/plots/elephants_issf_estimates_3hr_steps.png",
           dpi = 600, height = 6, width = 13)
    
    ggsave(plot = p_comb_spread, "builds/plots/elephants_issf_estimates_3hr_steps_spread.png",
           dpi = 600, height = 6, width = 13)
    
    ggsave(plot = p_meta, "builds/plots/exploratory/elephants_issf_stats_3hr_steps.png",
           dpi = 600, height = 10, width = 9)
    
  } else if(param == "12hrs"){
    
    fwrite(dt_est, "builds/model_outputs/issf_estimates_12hr_steps.csv")
    
    ggsave(plot = p_comb, "builds/plots/elephants_issf_estimates_12hr_steps.png",
           dpi = 600, height = 6, width = 13)
    
    ggsave(plot = p_comb_spread, "builds/plots/elephants_issf_estimates_12hr_steps_spread.png",
           dpi = 600, height = 6, width = 13)
    
    ggsave(plot = p_meta, "builds/plots/exploratory/elephants_issf_stats_12hr_steps.png",
           dpi = 600, height = 10, width = 9)
    
  } else if(param == "24hrs"){
    
    fwrite(dt_est, "builds/model_outputs/issf_estimates_24hr_steps.csv")
    
    ggsave(plot = p_comb, "builds/plots/elephants_issf_estimates_24hr_steps.png",
           dpi = 600, height = 6, width = 13)
    
    ggsave(plot = p_comb_spread, "builds/plots/elephants_issf_estimates_24hr_steps_spread.png",
           dpi = 600, height = 6, width = 13)
    
    ggsave(plot = p_meta, "builds/plots/exploratory/elephants_issf_stats_24hr_steps.png",
           dpi = 600, height = 10, width = 9)
    
  }
  
  print(paste0(param, " done at: ", Sys.time()))
  
  }