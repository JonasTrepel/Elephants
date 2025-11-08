library(data.table)
library(patchwork)
library(tidyverse)
library(sdmTMB)
library(patchwork)
library(groupdata2)
library(future)
library(furrr)
library(ggeffects)



#### loo ----------

dt_bm_loo <- fread("builds/model_outputs/loo_results_1000m_local_density_smoothed.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend"))) %>% 
  dplyr::select(excluded_park, model_id, model_path, response, dev_explained_full, dev_explained_var, n_park) %>% 
  unique()


vars <- c("local_density_km2_scaled",
          "months_extreme_drought_scaled",
          "fire_frequency_scaled", 
          "mat_coef_scaled", 
          "n_deposition_scaled")

dt_pred_loo_sub <- data.frame()

plan(multisession, workers = 35)

for_results <- future_map(
  1:nrow(dt_bm_loo),
  .progress = TRUE,
  .options = furrr_options(seed = TRUE),
  function(i) {
    
    response <- dt_bm_loo[i, ]$response
    
    #print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
    
    # 
    m <- readRDS(dt_bm_loo[i, ]$model_path)
    park <- unique(dt_bm_loo[i, ]$excluded_park)
    aic <- AIC(m)
    dat <- m$data
    
    dt_pred_sub <- predict(m) %>% 
      dplyr::select(grid_id, est) %>% 
      mutate(excluded_park = park, 
             response = response,
             n = nrow(dat), 
             dev_explained_full = dt_bm_loo[i,]$dev_explained_full, 
             dev_explained_var = dt_bm_loo[i,]$dev_explained_var, 
             n_park = dt_bm_loo[i,]$n_park
             )
    

   
    # Clean up that mess
    rm(m)
    gc()
    
    return(dt_pred_sub)
  }
)

plan(sequential)
dt_pred_loo <- rbindlist(for_results)
Sys.time()


dt_full <- dt_pred_loo %>% 
  filter(excluded_park == "No Park") %>% 
  rename(pred_full = est, 
         dev_explained_full_full = dev_explained_full, 
         dev_explained_var_full = dev_explained_var) %>% 
  dplyr::select(-n_park)

dt_diff <- data.frame()
for(park in unique(dt_pred_loo$excluded_park)){
  
  dt_park <- dt_pred_loo %>% 
    filter(excluded_park == park) %>% 
    dplyr::select(est, grid_id, response, dev_explained_full, dev_explained_var, n_park)
  
  
  dt_sub <- dt_full %>% 
    left_join(dt_park) %>% 
    filter(!is.na(est)) %>% 
    mutate(diff = pred_full - est) %>% 
    group_by(response) %>% 
    summarize(n = n(),
              pred_mad = mean(abs(diff), na.rm = T), 
              pred_mad_sd = sd(diff, na.rm = T), 
              diff_dev_exp_var = unique(dev_explained_var_full - dev_explained_var), 
              diff_dev_exp_full = unique(dev_explained_var_full - dev_explained_full_full),
              dev_explained_var = unique(dev_explained_var)) %>% 
    group_by(response) %>% 
    mutate(pred_mad_ci_lb = pred_mad - 1.96 * (pred_mad_sd / sqrt(n)),
              pred_mad_ci_ub = pred_mad + 1.96 * (pred_mad_sd / sqrt(n))) %>% 
    mutate(excluded_park = park, 
           n_park = unique(dt_park$n_park)) %>% 
    ungroup()
  
  
  dt_diff = rbind(dt_diff, dt_sub)
  
  print(paste0(park, " done"))
  
}


dt_diff_p <- dt_diff %>%
  mutate(park_label = paste0(excluded_park, " (n = ", n_park, ")"), 
         clean_response = case_when(
           response == "tree_cover_1000m_coef" ~ "Tree Cover Trend", 
           response == "canopy_height_900m_coef" ~ "Canopy Heightr Trend", 
           response == "evi_900m_coef" ~ "EVI Trend"),
         park_label = fct_reorder(park_label, n_park)
  )
  

p_mad <- dt_diff_p %>% 
  filter(excluded_park != "No Park") %>% 
  ggplot() +
  geom_pointrange(aes(x = pred_mad, y = park_label, xmin = pred_mad_ci_lb, xmax = pred_mad_ci_ub), 
                  shape = 18, linewidth = 1.1) +
  facet_wrap(~clean_response, scales = "free_x") +
  labs(y = "Excluded Park (and park specific sample size)", title = "", x = "Mean Absolute Difference (±95%CI)") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_mad

dt_line <- dt_diff_p %>% 
  filter(excluded_park == "No Park") %>% 
  unique()

p_dev <- dt_diff_p %>% 
  filter(excluded_park != "No Park") %>% 
  ggplot() +
  geom_vline(data = dt_line, aes(xintercept = dev_explained_var), linetype = "dashed", color = "grey25") +
  geom_point(aes(x = dev_explained_var, y = park_label)) +
  facet_wrap(~clean_response, scales = "free_x") +
  labs(y = "Excluded Park (and park specific sample size)", title = "", x = "Deviance Explained (Fixed Effects Only)") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_dev

(p_loo <- p_mad / p_dev)
ggsave(plot = p_loo, "builds/plots/supplement/loo_results_1000m.png", dpi = 600, height = 10, width = 12)


#### Extract variable specific predictions -----

vars <- c("local_density_km2_scaled",
          "months_extreme_drought_scaled",
          "fire_frequency_scaled", 
          "mat_coef_scaled", 
          "n_deposition_scaled")

responses <- c("tree_cover_1000m_coef", "canopy_height_900m_coef")

parks <- c("No Park", "Mavinga National Park", "Luengue-Luiana National Park", "Hwange", 
           "South Luangwa", "Kruger National Park", "Bwabwata")



dt_bm_loo <- fread("builds/model_outputs/loo_results_1000m_local_density_smoothed.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend"))) %>% 
  dplyr::select(excluded_park, model_id, model_path, response, dev_explained_full, dev_explained_var, n_park) %>% 
  unique()

extr_guide <- CJ(var = vars, 
                 response = responses, 
                 excluded_park = parks) %>% 
  left_join(dt_bm_loo)



plan(multisession, workers = 35)

for_results_pred <- future_map(
  1:nrow(extr_guide),
  .progress = TRUE,
  .options = furrr_options(seed = TRUE),
  function(i) {
    
    response <- extr_guide[i, ]$response
    park <- unique(extr_guide[i, ]$excluded_park)
    var <- unique(extr_guide[i, ]$var)
    
    
    m <- readRDS(extr_guide[i, ]$model_path)
    dat <- m$data
    

      var_us <- gsub("_scaled", "", var)
      mean_x <- mean(dat[[var_us]], na.rm = TRUE)
      sd_x   <- sd(dat[[var_us]], na.rm = TRUE)

      term_call <- paste0(var, " [all]")
      
      m_plot <- ggeffects::ggpredict(m, terms = term_call)

      plot_data <- as.data.table(m_plot) %>%
        mutate(
          x_unscaled = round(x * sd_x + mean_x, 3),
          var_name = var,
          response_name = response,
          excluded_park = park,
          dev_explained_full = extr_guide[i,]$dev_explained_full,
          dev_explained_var = extr_guide[i,]$dev_explained_var,
          n = nrow(dat),
          q95_unscaled = as.numeric(quantile(dat[[var_us]], .95, na.rm = T)), 
          q05_unscaled = as.numeric(quantile(dat[[var_us]], .05, na.rm = T)), 
          q95 = as.numeric(quantile(dat[[var]], .95, na.rm = T)), 
          q05 = as.numeric(quantile(dat[[var]], .05, na.rm = T))
        )

      # Ensure confidence interval columns exist
      if (!any(grepl("conf", names(plot_data)))) {
        plot_data <- plot_data %>%
          mutate(conf.low = NA,
                 conf.high = NA,
                 std.error = NA)
      }

    
    #print(paste0(response, " done at: ", Sys.time()))
    
    # Clean up that mess
    rm(m)
    gc()
    
    return(plot_data)
  }
)

plan(sequential)
print(paste0("loop done ", Sys.time()))

dt_pred_comp <- rbindlist(for_results_pred) %>% 
  mutate(scale = "km2", 
         response_clean = case_when(
           response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend",
           response_name == "tree_cover_1000m_coef" ~ "Tree Cover Trend",
         ),
         var_clean = case_when(
           var_name == "local_density_km2_scaled" ~ "Local Elephant Density",
           var_name == "months_extreme_drought_scaled" ~ "N Drought Months",
           var_name == "fire_frequency_scaled" ~ "Fire Frequency",
           var_name == "mat_coef_scaled" ~ "Temperature Change",
           var_name == "n_deposition_scaled" ~ "N Deposition",
         ))
unique(dt_pred_comp$response_name)
unique(dt_pred_comp$var_name)

fwrite(dt_pred_comp, "builds/model_outputs/loo_sdmtmb_predictions_1000m.csv")


p_smooth <- dt_pred_comp %>% 
  filter(!excluded_park %in% c("No Park")) %>% 
  ggplot() +
  # geom_point(data = dt_long, aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high, fill = excluded_park ), alpha = 0.1) +
  geom_line(aes(x = x_unscaled, y = predicted, color = excluded_park), linewidth = 1) +
  geom_ribbon(data = dt_pred_comp %>% 
                filter(excluded_park %in% c("No Park")), 
              aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, fill = "black") +
  geom_line(data = dt_pred_comp %>% 
              filter(excluded_park %in% c("No Park")),
            aes(x = x_unscaled, y = predicted, color = excluded_park), color = "black", linewidth = 1) +
  facet_grid(rows = vars(response_clean), cols = vars(var_clean), scales = "free") +
  labs(y = "Response Value", title = "", x = "Predictor Value",
       color = "Excluded Park", fill = "Excluded Park") +
  scico::scale_color_scico_d(begin = .1, end = .9, palette = "batlow") +
  scico::scale_fill_scico_d(begin = .1, end = .9, palette = "batlow") +
  theme_bw() +
  theme(legend.position = "bottom", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth

ggsave(plot = p_smooth, "builds/plots/supplement/loo_1000m_model_predictions.png", dpi = 900, height = 5, width = 8)
