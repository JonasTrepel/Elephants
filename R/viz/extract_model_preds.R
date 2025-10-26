# Compare best models 
library(data.table)
library(sjPlot)
library(tidyverse)
library(sdmTMB)

#### Smooth ----------

dt_bm_smooth <- fread("builds/model_outputs/sdmtmb_results_1000m_local_density_smoothed.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
    "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
  # filter(sanity_checks == TRUE) %>% 
  filter(!grepl("Intercept", term)) %>% 
  group_by(response, term) %>% 
  slice_max(log_cpo_approx) %>% 
  ungroup() %>% 
  dplyr::select(model_id, model_path, response) %>% 
  unique()


vars <- c("local_density_km2_scaled",
          "density_trend_estimate_scaled",
          "months_severe_drought_scaled",
          "fire_frequency_scaled", 
          "mat_change_scaled", 
          "n_deposition_scaled")

dt_pred_smooth <- data.frame()

for(i in 1:nrow(dt_bm_smooth)){
  
  response <- dt_bm_smooth[i,]$response
  
  m <- readRDS(dt_bm_smooth[i,]$model_path)
  
  print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
  
  
  for(var in unique(vars)){
    
    aic = AIC(m)
    
    dat <- m$data
    
    var_us = gsub("_scaled", "", var)
    
    mean_x <- mean(dat[[var_us]])
    sd_x   <- sd(dat[[var_us]])
    
    term_call = paste0(var, " [all]")
    
    m_plot <- plot_model(m, type = "pred", terms= term_call)
    
    
    plot_data = m_plot$data %>%
      as.data.table() %>% 
      mutate(x_unscaled = round(x * sd_x + mean_x, 3), 
             var_name = paste0(var), 
             response_name = paste0(response), 
             aic = aic)
    
    if(!any(grepl("conf", names(plot_data)))){
      
      plot_data = plot_data %>% 
        mutate(conf.low = NA, 
               conf.high = NA, 
               std.error = NA)
    }
    
    (p <- plot_data %>% ggplot() +
      geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
      geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
      labs(x = paste0(var_us), y = paste0(response)) +
      theme_minimal())
    
    print(p)
    
    dt_pred_smooth <- rbind(dt_pred_smooth, plot_data)
    
    print(paste0(var, " done"))
    
  }
  

  print(paste0(response, " done at: ", Sys.time()))
  
  
}


dt_pred_smooth %>% 
 # filter(response_name %in% c("evi_900m_coef")) %>% 
  ggplot() +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
  facet_grid(rows = vars(response_name), cols = vars(var_name), scales = "free") +
  theme_minimal()

#### simple ----------

dt_bm_simple <- fread("builds/model_outputs/sdmtmb_results_1000m_local_density.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
    "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
  # filter(sanity_checks == TRUE) %>% 
  filter(!grepl("Intercept", term)) %>% 
  group_by(response, term) %>% 
  slice_max(log_cpo_approx) %>% 
  ungroup() %>% 
  dplyr::select(model_id, model_path, response) %>% 
  unique()


vars <- c("local_density_km2_scaled",
          "density_trend_estimate_scaled",
          "months_severe_drought_scaled",
          "fire_frequency_scaled", 
          "mat_change_scaled", 
          "n_deposition_scaled")

dt_pred_simple <- data.frame()

for(i in 1:nrow(dt_bm_simple)){
  
  response <- dt_bm_simple[i,]$response
  
  m <- readRDS(dt_bm_simple[i,]$model_path)
  
  print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
  
  
  for(var in unique(vars)){
    
    aic = AIC(m)
    
    dat <- m$data
    
    var_us = gsub("_scaled", "", var)
    
    mean_x <- mean(dat[[var_us]])
    sd_x   <- sd(dat[[var_us]])
    
    term_call = paste0(var)
    
    m_plot <- plot_model(m, type = "pred", terms= term_call)
    
    
    plot_data = m_plot$data %>%
      as.data.table() %>% 
      mutate(x_unscaled = round(x * sd_x + mean_x, 3), 
             var_name = paste0(var), 
             response_name = paste0(response), 
             aic = aic)
    
    if(!any(grepl("conf", names(plot_data)))){
      
      plot_data = plot_data %>% 
        mutate(conf.low = NA, 
               conf.high = NA, 
               std.error = NA)
    }
    
    (p <- plot_data %>% ggplot() +
        geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
        geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
        labs(x = paste0(var_us), y = paste0(response)) +
        theme_minimal())
    
    print(p)
    
    dt_pred_simple <- rbind(dt_pred_simple, plot_data)
    
    print(paste0(var, " done"))
    
  }
  
  
  print(paste0(response, " done at: ", Sys.time()))
  
  
}


dt_pred_simple %>% 
  # filter(response_name %in% c("evi_900m_coef")) %>% 
  ggplot() +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
  facet_grid(rows = vars(response_name), cols = vars(var_name), scales = "free") +
  theme_minimal()


#### interaction ----------

dt_bm_interaction <- fread("builds/model_outputs/sdmtmb_results_1000m_local_density_with_interactions.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
    "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
  # filter(sanity_checks == TRUE) %>% 
  filter(!grepl("Intercept", term)) %>% 
  group_by(response, term) %>% 
  slice_max(log_cpo_approx) %>% 
  ungroup() %>% 
  dplyr::select(model_id, model_path, response) %>% 
  unique()


vars <- c("local_density_km2_scaled",
          "density_trend_estimate_scaled",
          "months_severe_drought_scaled",
          "fire_frequency_scaled", 
          "mat_change_scaled", 
          "n_deposition_scaled")

dt_pred_interaction <- data.frame()

for(i in 1:nrow(dt_bm_interaction)){
  
  response <- dt_bm_interaction[i,]$response
  
  m <- readRDS(dt_bm_interaction[i,]$model_path)
  
  print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
  
  
  for(var in unique(vars)){
    
    aic = AIC(m)
    
    dat <- m$data
    
    var_us = gsub("_scaled", "", var)
    
    mean_x <- mean(dat[[var_us]])
    sd_x   <- sd(dat[[var_us]])
    
    term_call = paste0(var)
    
    m_plot <- plot_model(m, type = "pred", terms= term_call)
    
    
    plot_data = m_plot$data %>%
      as.data.table() %>% 
      mutate(x_unscaled = round(x * sd_x + mean_x, 3), 
             var_name = paste0(var), 
             response_name = paste0(response), 
             aic = aic)
    
    if(!any(grepl("conf", names(plot_data)))){
      
      plot_data = plot_data %>% 
        mutate(conf.low = NA, 
               conf.high = NA, 
               std.error = NA)
    }
    
    (p <- plot_data %>% ggplot() +
        geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
        geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
        labs(x = paste0(var_us), y = paste0(response)) +
        theme_minimal())
    
    print(p)
    
    dt_pred_interaction <- rbind(dt_pred_interaction, plot_data)
    
    print(paste0(var, " done"))
    
  }
  
  
  print(paste0(response, " done at: ", Sys.time()))
  
  
}


dt_pred_interaction %>% 
  # filter(response_name %in% c("evi_900m_coef")) %>% 
  ggplot() +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
  facet_grid(rows = vars(response_name), cols = vars(var_name), scales = "free") +
  theme_minimal()

dt_1000m <- rbind(dt_pred_simple %>% mutate(tier = "Simple"),
                  dt_pred_interaction %>% mutate(tier = "With Interactions"),
                  dt_pred_smooth %>% mutate(tier = "Smooth")) %>% 
  mutate(scale = "km2", 
         response_clean = case_when(
           response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend)",
           response_name == "canopy_height_sd_900m_coef" ~ "Canopy Height SD Trend)",
           response_name == "evi_900m_coef" ~ "EVI Trend",
           response_name == "evi_sd_900m_coef" ~ "EVI SD Trend",
           response_name == "tree_cover_1000m_coef" ~ "Tree Cover Trend",
           response_name == "tree_cover_sd_1000m_coef" ~ "Tree Cover SD Trend",
         ),
         var_clean = case_when(
           var_name == "local_density_km2_scaled" ~ "Local Elephant Density",
           var_name == "density_trend_estimate_scaled" ~ "Elephant Density Trend",
           var_name == "months_severe_drought_scaled" ~ "Severe Drought Months",
           var_name == "fire_frequency_scaled" ~ "Fire Frequency",
           var_name == "mat_change_scaled" ~ "Temperature Change",
           var_name == "n_deposition_scaled" ~ "N Deposition",
         ))
unique(dt_1000m$response_name)
unique(dt_1000m$var_name)
### plot 

dt_1000m %>% 
  filter(response_name %in% c("evi_900m_coef")) %>% 
  ggplot() +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
  facet_grid(rows = vars(tier), cols = vars(var_clean), scales = "free") +
  theme_bw()


#### simple 100m ----------

dt_bm_simple_100m <- fread("builds/model_outputs/sdmtmb_results_100m_local_density.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
    "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
  # filter(sanity_checks == TRUE) %>% 
  filter(!grepl("Intercept", term)) %>% 
  group_by(response, term) %>% 
  slice_max(log_cpo_approx) %>% 
  ungroup() %>% 
  dplyr::select(model_id, model_path, response) %>% 
  unique()


vars <- c("local_density_km2_scaled",
          "density_trend_estimate_scaled",
          "months_severe_drought_scaled",
          "fire_frequency_scaled", 
          "mat_change_scaled", 
          "n_deposition_scaled")

dt_pred_simple_100m <- data.frame()

for(i in 1:nrow(dt_bm_simple_100m)){
  
  response <- dt_bm_simple_100m[i,]$response
  
  m <- readRDS(dt_bm_simple_100m[i,]$model_path)
  
  print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
  
  
  for(var in unique(vars)){
    
    aic = AIC(m)
    
    dat <- m$data
    
    var_us = gsub("_scaled", "", var)
    
    mean_x <- mean(dat[[var_us]])
    sd_x   <- sd(dat[[var_us]])
    
    term_call = paste0(var)
    
    m_plot <- plot_model(m, type = "pred", terms= term_call)
    
    
    plot_data = m_plot$data %>%
      as.data.table() %>% 
      mutate(x_unscaled = round(x * sd_x + mean_x, 3), 
             var_name = paste0(var), 
             response_name = paste0(response), 
             aic = aic)
    
    if(!any(grepl("conf", names(plot_data)))){
      
      plot_data = plot_data %>% 
        mutate(conf.low = NA, 
               conf.high = NA, 
               std.error = NA)
    }
    
    (p <- plot_data %>% ggplot() +
        geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
        geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
        labs(x = paste0(var_us), y = paste0(response)) +
        theme_minimal())
    
    print(p)
    
    dt_pred_simple_100m <- rbind(dt_pred_simple_100m, plot_data)
    
    print(paste0(var, " done"))
    
  }
  
  
  print(paste0(response, " done at: ", Sys.time()))
  
  
}


dt_pred_simple_100m %>% 
  # filter(response_name %in% c("evi_900m_coef")) %>% 
  ggplot() +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
  facet_grid(rows = vars(response_name), cols = vars(var_name), scales = "free") +
  theme_minimal()

