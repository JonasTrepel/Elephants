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

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_100m.csv") %>% 
  mutate(tree_cover_100m_coef = tree_cover_100m_coef*100, 
  ) %>% 
  filter(park_id != "Thornybush Nature Reserve")

names(dt)

# get dataframe with comlete and clean data fro mdoeling 

quantile(dt$dw_min_median_mode_fraction, na.rm = T)

names(dt)

acceptable_numbers = seq(1, 10000000, 10)
dt_mod <- dt %>% 
  filter(dw_min_median_mode_fraction >= 50) %>% 
  dplyr::select(
    #mean values /habitat characteristics 
    mean_tree_cover_100m, mean_evi_90m, mean_canopy_height_90m, 

    #starting conditions
    tree_cover_100m_2015_2016, evi_90m_2013_2014, canopy_height_90m_2000,

    # environmental predictors
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, months_severe_drought, months_extreme_drought,
    mat_coef, prec_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2,
    
    #Trends - Responses 
    tree_cover_100m_coef, evi_90m_coef, canopy_height_90m_coef, 

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
  mutate(park_row_nr = 1:n()) %>% 
  filter(park_row_nr %in% acceptable_numbers) %>% 
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
    months_severe_drought_scaled   = as.numeric(scale(months_severe_drought)),
    months_extreme_drought_scaled = as.numeric(scale(months_extreme_drought)),
    fire_frequency_scaled          = as.numeric(scale(fire_frequency)),
    n_deposition_scaled            = as.numeric(scale(n_deposition)), 
    mat_scaled = as.numeric(scale(mat)), 
    map_scaled = as.numeric(scale(map))
  )

#### Smooth ----------

dt_bm_smooth <- fread("builds/model_outputs/sdmtmb_results_100m_local_density_smoothed.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend"))) %>% 
  dplyr::select(model_id, model_path, response, dev_explained_var, dev_explained_full) %>% 
  unique()


vars <- c("local_density_km2_scaled",
          "months_extreme_drought_scaled",
          "fire_frequency_scaled", 
          "mat_coef_scaled", 
          "n_deposition_scaled")

dt_pred_smooth <- data.frame()

plan(multisession, workers = 5)

for(i in 1:nrow(dt_bm_smooth)){
  response <- dt_bm_smooth[i,]$response
  
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
                               x_unscaled = round(x * sd_x + mean_x, 6), 
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


dt_100m <- dt_pred_smooth %>% 
  mutate(scale = "km2", 
         response_clean = case_when(
           response_name == "canopy_height_90m_coef" ~ "Canopy Height Trend",
           response_name == "tree_cover_100m_coef" ~ "Tree Cover Trend",
           response_name == "evi_90m_coef" ~ "EVI Trend"
         ),
         var_clean = case_when(
           var_name == "local_density_km2_scaled" ~ "Local Elephant Density",
           var_name == "months_extreme_drought_scaled" ~ "N Drought Months",
           var_name == "fire_frequency_scaled" ~ "Fire Frequency",
           var_name == "mat_coef_scaled" ~ "Temperature Change",
           var_name == "n_deposition_scaled" ~ "N Deposition",
         ))
unique(dt_100m$response_name)
unique(dt_100m$var_name)

fwrite(dt_100m, "builds/model_outputs/sdmtmb_100m_predictions.csv")

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
    cols = c("canopy_height_90m_coef", "tree_cover_100m_coef", 
             "evi_90m_coef"), 
    names_to = "response_name", 
    values_to = "response_value") %>% 
  mutate(response_clean = case_when(
    response_name == "canopy_height_90m_coef" ~ "Canopy Height Trend",
    response_name == "tree_cover_100m_coef" ~ "Tree Cover Trend",
    response_name == "evi_90m_coef" ~ "EVI Trend"
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

p_smooth_points <- dt_100m %>% 
  # filter(response_name %in% c("evi_90m_coef") & tier == "Simple") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               group_by(response_clean, var_clean) %>% 
               sample_n(100000) %>% 
               ungroup(), aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey5") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1) +
  scale_color_manual(values = c("#40631F", "#0F443E")) +
  scale_fill_manual(values = c("#40631F", "#0F443E")) + 
  facet_grid(rows = vars(response_clean), cols = vars(var_clean), scales = "free") +
  # labs(y = "Evi Trend", title = "Simple", x = "") +
  theme_bw() +
  labs(y = "Response Value", title = "", x = "Predictor Value") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_points
ggsave(plot = p_smooth_points, "builds/plots/100m_model_predictions_and_points_smooth.png", dpi = 900, height = 6, width = 9)


p_smooth <- dt_100m %>% 
  # filter(response_name %in% c("evi_90m_coef") & tier == "Simple") %>% 
  ggplot() +
  # geom_point(data = dt_long, aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey5") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high, fill = response_clean), alpha = 0.4) +
  geom_line(aes(x = x_unscaled, y = predicted, color = response_clean), linewidth = 1.1) +
  scale_color_manual(values = c("#40631F", "#0F443E")) +
  scale_fill_manual(values = c("#40631F", "#0F443E")) + 
  facet_grid(rows = vars(response_clean), cols = vars(var_clean), scales = "free") +
  geom_rect(data = rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Response Value", title = "", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth

ggsave(plot = p_smooth, "builds/plots/100m_model_predictions_smooth.png", dpi = 900, height = 6, width = 9)

