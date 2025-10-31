# Compare best models 
library(data.table)
library(sjPlot)
library(tidyverse)
library(sdmTMB)
library(patchwork)
library(groupdata2)
library(future)
library(furrr)
library(ggeffects)


### get raw data 

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100, 
         evi_900m_coef = evi_900m_coef/100
  ) %>% 
  filter(park_id != "Thornybush Nature Reserve")

names(dt)

# get dataframe with comlete and clean data fro mdoeling 

quantile(dt$dw_min_median_mode_fraction, na.rm = T)

names(dt)

dt_mod <- dt %>% 
  filter(dw_min_median_mode_fraction >= 50) %>% 
  dplyr::select(
    #mean values /habitat characteristics 
    mean_tree_cover_1000m, mean_evi_900m, mean_canopy_height_900m, 
    mean_habitat_diversity_1000m, mean_evi_sd_900m, mean_canopy_height_sd_900m, 
    
    #starting conditions
    tree_cover_1000m_2015_2016, evi_900m_2013_2014, canopy_height_900m_2000,
    habitat_diversity_1000m_2015_2016, evi_sd_900m_2013_2014, canopy_height_sd_900m_2000,
    
    # environmental predictors
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, months_severe_drought, months_extreme_drought, mat_change, prec_change,
    mat_coef, prec_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2, density_trend_estimate, density_trend_estimate,
    
    #Trends - Responses 
    tree_cover_1000m_coef, evi_900m_coef, canopy_height_900m_coef, 
    habitat_diversity_1000m_coef, tree_cover_sd_1000m_coef, evi_sd_900m_coef, canopy_height_sd_900m_coef, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, 
    
    #identifieres 
    park_id, cluster_id, grid_id
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    x_moll_km = x_mollweide/1000, 
    y_moll_km = y_mollweide/1000,
  ) %>%
  group_by(park_id) %>% 
  filter(n() >= 10) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  fold(., #make sure to stratify folds in a way that each park is present in each fold
       k = 5,
       cat_col = "park_id") %>% 
  rename(fold_id = `.folds`) %>% 
  as.data.table() %>% 
  mutate(park_id = factor(park_id)) %>% 
  mutate(
    local_density_km2_scaled       = as.numeric(scale(local_density_km2)),
    mean_density_km2_scaled       = as.numeric(scale(mean_density_km2)),
    density_trend_estimate_scaled = as.numeric(scale(density_trend_estimate)),
    months_severe_drought_scaled   = as.numeric(scale(months_severe_drought)),
    months_extreme_drought_scaled = as.numeric(scale(months_extreme_drought)),
    fire_frequency_scaled          = as.numeric(scale(fire_frequency)),
    mat_change_scaled                = as.numeric(scale(mat_change)),
    prec_change_scaled               = as.numeric(scale(prec_change)),
    n_deposition_scaled            = as.numeric(scale(n_deposition)), 
    mat_scaled = as.numeric(scale(mat)), 
    map_scaled = as.numeric(scale(map))
  )

#### Smooth ----------

dt_bm_smooth <- fread("builds/model_outputs/sdmtmb_results_1000m_local_density_smoothed.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend"))) %>% 
  dplyr::select(model_id, model_path, response) %>% 
  unique()


vars <- c("local_density_km2_scaled",
          "months_extreme_drought_scaled",
          "fire_frequency_scaled", 
          "mat_coef_scaled", 
          "n_deposition_scaled")

dt_pred_smooth <- data.frame()

plan(multisession, workers = 5)

for(i in 1:nrow(dt_bm_smooth)){
  
  
  print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
  
res_list <- future_map(unique(vars),
                       .progress = TRUE,
                       .options = furrr_options(seed = TRUE),
                       function(var) {
 # for(var in unique(vars)){
   response <- dt_bm_smooth[i,]$response
                         
   m <- readRDS(dt_bm_smooth[i,]$model_path)
                         
  aic <- AIC(m)
  dat <- m$data
  
  var_us <- gsub("_scaled", "", var)
  
  mean_x <- mean(dat[[var_us]], na.rm = TRUE)
  sd_x   <- sd(dat[[var_us]], na.rm = TRUE)
  
  term_call <- paste0(var, " [all]")
  
  m_plot <- ggeffects::ggpredict(m, terms = term_call)
  
  plot_data <- as.data.table(m_plot) %>%
    as.data.table() %>% 
    mutate(
      x_unscaled = round(x * sd_x + mean_x, 3), 
      var_name = var, 
      response_name = response, 
      aic = aic
    )
  
  # Ensure confidence interval columns exist - some hickup w/o in prevuous version
  if (!any(grepl("conf", names(plot_data)))) {
    plot_data <- plot_data %>% 
      mutate(conf.low = NA,
             conf.high = NA,
             std.error = NA)
  }
  

  
  return(plot_data)
  rm(m)
  gc()
    
  })
  

 dt_pred_smooth_sub <- rbindlist(res_list)
 
 dt_pred_smooth <- rbind(dt_pred_smooth, dt_pred_smooth_sub)
 
 print(paste0(response, " done at: ", Sys.time()))
  
  
}


dt_1000m <- dt_pred_smooth %>% 
  mutate(scale = "km2", 
         response_clean = case_when(
           response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend",
           response_name == "tree_cover_1000m_coef" ~ "Tree Cover Trend",
           response_name == "evi_900m_coef" ~ "EVI Trend"
         ),
         var_clean = case_when(
           var_name == "local_density_km2_scaled" ~ "Local Elephant Density",
           var_name == "months_extreme_drought_scaled" ~ "N Drought Months",
           var_name == "fire_frequency_scaled" ~ "Fire Frequency",
           var_name == "mat_coef_scaled" ~ "Temperature Change",
           var_name == "n_deposition_scaled" ~ "N Deposition",
         ))
unique(dt_1000m$response_name)
unique(dt_1000m$var_name)

fwrite(dt_1000m, "builds/model_outputs/sdmtmb_1000m_predictions.csv")

dt_long <- dt_mod %>% pivot_longer(
  cols = c("local_density_km2",
           "months_extreme_drought", "fire_frequency", 
           "mat_coef", "n_deposition"), 
  names_to = "var_name", 
  values_to = "var_value") %>% 
  mutate(var_clean = case_when(
    var_name == "local_density_km2" ~ "Local Elephant Density",
    var_name == "months_extreme_drought" ~ "N Drought Months",
    var_name == "fire_frequency" ~ "Fire Frequency",
    var_name == "mat_coef" ~ "Temperature Change",
    var_name == "n_deposition" ~ "N Deposition",
  )) %>% 
  pivot_longer(
    cols = c("canopy_height_900m_coef", "tree_cover_1000m_coef", 
             "evi_900m_coef"), 
    names_to = "response_name", 
    values_to = "response_value") %>% 
  mutate(response_clean = case_when(
      response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend",
      response_name == "tree_cover_1000m_coef" ~ "Tree Cover Trend",
      response_name == "evi_900m_coef" ~ "EVI Trend"
         ))

rects <- dt_long %>%
  group_by(var_clean) %>%
  mutate(
    lower_quantile_x = quantile(var_value, 0.05),
    upper_quantile_x = quantile(var_value, 0.95),
  ) %>%
  ungroup() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf,
    ymax = Inf,
    xmin1 = -Inf,
    xmax1 = first(lower_quantile_x),
    xmin2 = first(upper_quantile_x),
    xmax2 = Inf
  ) %>%
  ungroup()

### plot -----------------------------

p_smooth_points <- dt_1000m %>% 
 # filter(response_name %in% c("evi_900m_coef") & tier == "Simple") %>% 
  ggplot() +
  geom_point(data = dt_long, aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey5") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
  facet_grid(rows = vars(response_clean), cols = vars(var_clean), scales = "free") +
 # labs(y = "Evi Trend", title = "Simple", x = "") +
  theme_bw() +
  labs(y = "Response Value", title = "", x = "Predictor Value") +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_points
ggsave(plot = p_smooth_points, "builds/plots/1000m_model_predictions_and_points_smooth.png", dpi = 900, height = 6, width = 9)


p_smooth <- dt_1000m %>% 
  # filter(response_name %in% c("evi_900m_coef") & tier == "Simple") %>% 
  ggplot() +
 # geom_point(data = dt_long, aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
  facet_grid(rows = vars(response_clean), cols = vars(var_clean), scales = "free") +
  geom_rect(data = rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Response Value", title = "", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth

ggsave(plot = p_smooth, "builds/plots/1000m_model_predictions_smooth.png", dpi = 900, height = 6, width = 9)

# dt_pred_smooth %>% 
#  # filter(response_name %in% c("evi_900m_coef")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
#   facet_grid(rows = vars(response_name), cols = vars(var_name), scales = "free") +
#   theme_minimal()
# 
# #------------
# 
# 
# #### simple ----------
# 
# dt_bm_simple <- fread("builds/model_outputs/sdmtmb_results_1000m_local_density.csv") %>% 
#   mutate(clean_response = factor(clean_response, levels = c(
#     "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
#     "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
#   # filter(sanity_checks == TRUE) %>% 
#   filter(!grepl("Intercept", term)) %>% 
#   group_by(response, term) %>% 
#   slice_max(log_cpo_approx) %>% 
#   ungroup() %>% 
#   dplyr::select(model_id, model_path, response) %>% 
#   unique()
# 
# 
# vars <- c("local_density_km2_scaled",
#           "density_trend_estimate_scaled",
#           "months_severe_drought_scaled",
#           "fire_frequency_scaled", 
#           "mat_change_scaled", 
#           "n_deposition_scaled")
# 
# dt_pred_simple <- data.frame()
# 
# for(i in 1:nrow(dt_bm_simple)){
#   
#   response <- dt_bm_simple[i,]$response
#   
#   m <- readRDS(dt_bm_simple[i,]$model_path)
#   
#   print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
#   
#   
#   for(var in unique(vars)){
#     
#     aic = AIC(m)
#     
#     dat <- m$data
#     
#     var_us = gsub("_scaled", "", var)
#     
#     mean_x <- mean(dat[[var_us]])
#     sd_x   <- sd(dat[[var_us]])
#     
#     term_call = paste0(var)
#     
#     m_plot <- plot_model(m, type = "pred", terms= term_call)
#     
#     
#     plot_data = m_plot$data %>%
#       as.data.table() %>% 
#       mutate(x_unscaled = round(x * sd_x + mean_x, 3), 
#              var_name = paste0(var), 
#              response_name = paste0(response), 
#              aic = aic)
#     
#     if(!any(grepl("conf", names(plot_data)))){
#       
#       plot_data = plot_data %>% 
#         mutate(conf.low = NA, 
#                conf.high = NA, 
#                std.error = NA)
#     }
#     
#     (p <- plot_data %>% ggplot() +
#         geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
#         geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
#         labs(x = paste0(var_us), y = paste0(response)) +
#         theme_minimal())
#     
#     print(p)
#     
#     dt_pred_simple <- rbind(dt_pred_simple, plot_data)
#     
#     print(paste0(var, " done"))
#     
#   }
#   
#   
#   print(paste0(response, " done at: ", Sys.time()))
#   
#   
# }
# 
# 
# dt_pred_simple %>% 
#   # filter(response_name %in% c("evi_900m_coef")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
#   facet_grid(rows = vars(response_name), cols = vars(var_name), scales = "free") +
#   theme_minimal()
# 
# 
# #### interaction ----------
# 
# dt_bm_interaction <- fread("builds/model_outputs/sdmtmb_results_1000m_local_density_with_interactions.csv") %>% 
#   mutate(clean_response = factor(clean_response, levels = c(
#     "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
#     "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
#   # filter(sanity_checks == TRUE) %>% 
#   filter(!grepl("Intercept", term)) %>% 
#   group_by(response, term) %>% 
#   slice_max(log_cpo_approx) %>% 
#   ungroup() %>% 
#   dplyr::select(model_id, model_path, response) %>% 
#   unique()
# 
# 
# vars <- c("local_density_km2_scaled",
#           "density_trend_estimate_scaled",
#           "months_severe_drought_scaled",
#           "fire_frequency_scaled", 
#           "mat_change_scaled", 
#           "n_deposition_scaled")
# 
# dt_pred_interaction <- data.frame()
# 
# for(i in 1:nrow(dt_bm_interaction)){
#   
#   response <- dt_bm_interaction[i,]$response
#   
#   m <- readRDS(dt_bm_interaction[i,]$model_path)
#   
#   print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
#   
#   
#   for(var in unique(vars)){
#     
#     aic = AIC(m)
#     
#     dat <- m$data
#     
#     var_us = gsub("_scaled", "", var)
#     
#     mean_x <- mean(dat[[var_us]])
#     sd_x   <- sd(dat[[var_us]])
#     
#     term_call = paste0(var)
#     
#     m_plot <- plot_model(m, type = "pred", terms= term_call)
#     
#     
#     plot_data = m_plot$data %>%
#       as.data.table() %>% 
#       mutate(x_unscaled = round(x * sd_x + mean_x, 3), 
#              var_name = paste0(var), 
#              response_name = paste0(response), 
#              aic = aic)
#     
#     if(!any(grepl("conf", names(plot_data)))){
#       
#       plot_data = plot_data %>% 
#         mutate(conf.low = NA, 
#                conf.high = NA, 
#                std.error = NA)
#     }
#     
#     (p <- plot_data %>% ggplot() +
#         geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
#         geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
#         labs(x = paste0(var_us), y = paste0(response)) +
#         theme_minimal())
#     
#     print(p)
#     
#     dt_pred_interaction <- rbind(dt_pred_interaction, plot_data)
#     
#     print(paste0(var, " done"))
#     
#   }
#   
#   
#   print(paste0(response, " done at: ", Sys.time()))
#   
#   
# }
# 
# 
# dt_pred_interaction %>% 
#   # filter(response_name %in% c("evi_900m_coef")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
#   facet_grid(rows = vars(response_name), cols = vars(var_name), scales = "free") +
#   theme_minimal()
# 
# dt_1000m <- rbind(dt_pred_simple %>% mutate(tier = "Simple"),
#                   dt_pred_interaction %>% mutate(tier = "With Interactions"),
#                   dt_pred_smooth %>% mutate(tier = "Smooth")) %>% 
#   mutate(scale = "km2", 
#          response_clean = case_when(
#            response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend)",
#            response_name == "canopy_height_sd_900m_coef" ~ "Canopy Height SD Trend)",
#            response_name == "evi_900m_coef" ~ "EVI Trend",
#            response_name == "evi_sd_900m_coef" ~ "EVI SD Trend",
#            response_name == "tree_cover_1000m_coef" ~ "Tree Cover Trend",
#            response_name == "tree_cover_sd_1000m_coef" ~ "Tree Cover SD Trend",
#          ),
#          var_clean = case_when(
#            var_name == "local_density_km2_scaled" ~ "Local Elephant Density",
#            var_name == "density_trend_estimate_scaled" ~ "Elephant Density Trend",
#            var_name == "months_severe_drought_scaled" ~ "Severe Drought Months",
#            var_name == "fire_frequency_scaled" ~ "Fire Frequency",
#            var_name == "mat_change_scaled" ~ "Temperature Change",
#            var_name == "n_deposition_scaled" ~ "N Deposition",
#          ))
# unique(dt_1000m$response_name)
# unique(dt_1000m$var_name)
# 
# fwrite(dt_1000m, "builds/model_outputs/sdmtmb_1000m_predictions.csv")
# 
# dt_long <- dt_mod %>% pivot_longer(
#   cols = c("local_density_km2", "density_trend_estimate", 
#            "months_severe_drought", "fire_frequency", 
#            "mat_change", "n_deposition"), 
#            names_to = "var_name", 
#            values_to = "var_value") %>% 
#   mutate(var_clean = case_when(
#     var_name == "local_density_km2" ~ "Local Elephant Density",
#     var_name == "density_trend_estimate" ~ "Elephant Density Trend",
#     var_name == "months_severe_drought" ~ "Severe Drought Months",
#     var_name == "fire_frequency" ~ "Fire Frequency",
#     var_name == "mat_change" ~ "Temperature Change",
#     var_name == "n_deposition" ~ "N Deposition",
#   ))
# 
# ### plot -----------------------------
# 
# p_evi_simple <- dt_1000m %>% 
#   filter(response_name %in% c("evi_900m_coef") & tier == "Simple") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = evi_900m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Evi Trend", title = "Simple", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_evi_simple
# 
# p_evi_interaction <- dt_1000m %>% 
#   filter(response_name %in% c("evi_900m_coef") & tier == "With Interactions") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = evi_900m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Evi Trend", title = "With Interactions", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_evi_interaction
# 
# 
# p_evi_smooth <- dt_1000m %>% 
#   filter(response_name %in% c("evi_900m_coef") & tier == "Smooth") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = evi_900m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Evi Trend", title = "Smooth", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_evi_smooth
# 
# (p_evi <- p_evi_simple / p_evi_interaction / p_evi_smooth)
# ggsave(plot = p_evi, "builds/plots/exploratory/evi_trend_estimates_1000m.png", width = 10, height = 8)
# 
# # Tree cover change 
# p_tree_simple <- dt_1000m %>% 
#   filter(response_name %in% c("tree_cover_1000m_coef") & tier == "Simple") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = tree_cover_1000m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Tree Cover Trend", title = "Simple", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_tree_simple
# 
# p_tree_interaction <- dt_1000m %>% 
#   filter(response_name %in% c("tree_cover_1000m_coef") & tier == "With Interactions") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = tree_cover_1000m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Tree Cover", title = "With Interactions", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_tree_interaction
# 
# 
# p_tree_smooth <- dt_1000m %>% 
#   filter(response_name %in% c("tree_cover_1000m_coef") & tier == "Smooth") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = tree_cover_1000m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Tree Cover Trend", title = "Smooth", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_tree_smooth
# 
# (p_tree <- p_tree_simple / p_tree_interaction / p_tree_smooth)
# ggsave(plot = p_tree, "builds/plots/exploratory/tree_cover_trend_estimates_1000m.png", width = 10, height = 8)
# 
# # Canopy Height change 
# p_canopy_simple <- dt_1000m %>% 
#   filter(response_name %in% c("canopy_height_900m_coef") & tier == "Simple") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = canopy_height_900m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Canopy Height Trend", title = "Simple", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_canopy_simple
# 
# p_canopy_interaction <- dt_1000m %>% 
#   filter(response_name %in% c("canopy_height_900m_coef") & tier == "With Interactions") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = canopy_height_900m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Canopy Height", title = "With Interactions", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_canopy_interaction
# 
# 
# p_canopy_smooth <- dt_1000m %>% 
#   filter(response_name %in% c("canopy_height_900m_coef") & tier == "Smooth") %>% 
#   ggplot() +
#   geom_point(data = dt_long, aes(x = var_value, y = canopy_height_900m_coef), alpha = 0.1, size = 0.1, color = "grey25") +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, color = "darkred") +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "darkred") +
#   facet_wrap(~var_clean, scales = "free_x", ncol = 6) +
#   labs(y = "Canopy Height Trend", title = "Smooth", x = "") +
#   theme_bw() +
#   theme(legend.position = "right", 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         panel.border = element_blank(), 
#         panel.background = element_rect(fill = "snow"), 
#         strip.background = element_rect(fill = "linen", color = "linen"))
# 
# p_canopy_smooth
# 
# (p_canopy <- p_canopy_simple / p_canopy_interaction / p_canopy_smooth)
# ggsave(plot = p_canopy, "builds/plots/exploratory/canopy_height_trend_estimates_1000m.png", width = 10, height = 8)
# 
# 
# 
# 
# #### simple 100m ----------
# 
# dt_bm_simple_100m <- fread("builds/model_outputs/sdmtmb_results_100m_local_density.csv") %>% 
#   mutate(clean_response = factor(clean_response, levels = c(
#     "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
#     "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
#   # filter(sanity_checks == TRUE) %>% 
#   filter(!grepl("Intercept", term)) %>% 
#   group_by(response, term) %>% 
#   slice_max(log_cpo_approx) %>% 
#   ungroup() %>% 
#   dplyr::select(model_id, model_path, response) %>% 
#   unique()
# 
# 
# vars <- c("local_density_km2_scaled",
#           "density_trend_estimate_scaled",
#           "months_severe_drought_scaled",
#           "fire_frequency_scaled", 
#           "mat_change_scaled", 
#           "n_deposition_scaled")
# 
# dt_pred_simple_100m <- data.frame()
# 
# for(i in 1:nrow(dt_bm_simple_100m)){
#   
#   response <- dt_bm_simple_100m[i,]$response
#   
#   m <- readRDS(dt_bm_simple_100m[i,]$model_path)
#   
#   print(paste0("Start extracting predictions for response ", response, " at: ", Sys.time()))
#   
#   
#   for(var in unique(vars)){
#     
#     aic = AIC(m)
#     
#     dat <- m$data
#     
#     var_us = gsub("_scaled", "", var)
#     
#     mean_x <- mean(dat[[var_us]])
#     sd_x   <- sd(dat[[var_us]])
#     
#     term_call = paste0(var)
#     
#     m_plot <- plot_model(m, type = "pred", terms= term_call)
#     
#     library(ggeffects)
#     ggpredict(m, terms = "local_density_km2_scaled")
#     
#     
#     plot_data = m_plot$data %>%
#       as.data.table() %>% 
#       mutate(x_unscaled = round(x * sd_x + mean_x, 3), 
#              var_name = paste0(var), 
#              response_name = paste0(response), 
#              aic = aic)
#     
#     if(!any(grepl("conf", names(plot_data)))){
#       
#       plot_data = plot_data %>% 
#         mutate(conf.low = NA, 
#                conf.high = NA, 
#                std.error = NA)
#     }
#     
#     (p <- plot_data %>% ggplot() +
#         geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
#         geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
#         labs(x = paste0(var_us), y = paste0(response)) +
#         theme_minimal())
#     
#     print(p)
#     
#     dt_pred_simple_100m <- rbind(dt_pred_simple_100m, plot_data)
#     
#     print(paste0(var, " done"))
#     
#   }
#   
#   
#   print(paste0(response, " done at: ", Sys.time()))
#   
#   
# }
# 
# 
# dt_pred_simple_100m %>% 
#   # filter(response_name %in% c("evi_900m_coef")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
#   geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
#   facet_grid(rows = vars(response_name), cols = vars(var_name), scales = "free") +
#   theme_minimal()
# 
