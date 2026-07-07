library(data.table)
library(patchwork)
library(tidyverse)
library(sdmTMB)
library(patchwork)
library(groupdata2)
library(future)
library(furrr)
library(ggeffects)

#### park ----------

dt_bm_park <- fread("builds/model_outputs/park_results_1000m_local_density.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend"))) %>% 
  dplyr::select(park, model_id, model_path, response, dev_explained_full, dev_explained_var) %>% 
  unique()

dt_mod %>% filter(!is.na(local_density_km2)) %>% dplyr::select(park_id) %>% pull() %>% unique()

parks = fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100) %>% 
  filter(!is.na(local_density_km2)) %>% 
  dplyr::select(park_id, area_km2) %>% 
  unique() %>% 
  arrange(-area_km2) %>% 
  slice_max(area_km2, n = 5) %>% 
  pull(park_id)

#### Extract variable specific predictions -----

vars <- c("local_density_km2_scaled",
          "months_extreme_drought_scaled",
          "fire_frequency_scaled", 
          "prec_coef_scaled")

responses <- c("tree_cover_1000m_coef", "canopy_height_900m_coef")

parks <- c(parks)


extr_guide <- CJ(var = vars, 
                 response = responses, 
                 park = parks) %>% 
  left_join(dt_bm_park)



plan(multisession, workers = 32)

for_results_pred <- future_map(
  1:nrow(extr_guide),
  .progress = TRUE,
  .options = furrr_options(seed = TRUE),
  function(i) {
    
    response <- extr_guide[i, ]$response
    park <- unique(extr_guide[i, ]$park)
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
        x_unscaled = round(x * sd_x + mean_x, 6),
        var_name = var,
        response_name = response,
        park = park,
        dev_explained_full = extr_guide[i,]$dev_explained_full,
        dev_explained_var = extr_guide[i,]$dev_explained_var,
        n = nrow(dat),
        q975_unscaled = as.numeric(quantile(dat[[var_us]], .975, na.rm = T)), 
        q025_unscaled = as.numeric(quantile(dat[[var_us]], .025, na.rm = T)), 
        q975 = as.numeric(quantile(dat[[var]], .975, na.rm = T)), 
        q025 = as.numeric(quantile(dat[[var]], .025, na.rm = T))
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
print(paste0("park done ", Sys.time()))

dt_pred_comp <- rbindlist(for_results_pred) %>% 
  mutate(scale = "km2", 
         response_clean = case_when(
           response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend",
           response_name == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
         ),
         var_clean = case_when(
           var_name == "local_density_km2_scaled" ~ "Local Elephant Density",
           var_name == "months_extreme_drought_scaled" ~ "N Drought Months",
           var_name == "fire_frequency_scaled" ~ "Fire Frequency",
           var_name == "prec_coef_scaled" ~ "Rainfall Change",
           var_name == "n_deposition_scaled" ~ "N Deposition",
         ))
unique(dt_pred_comp$response_name)
unique(dt_pred_comp$var_name)

fwrite(dt_pred_comp, "builds/model_outputs/park_level_predictions_1000m.csv")


## get point data in there


dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100)

setDT(dt)

dt_mod <- dt %>% 
  filter(dw_min_median_mode_fraction >= 50) %>% 
  dplyr::select(
    #mean values /habitat characteristics 
    mean_tree_cover_1000m, mean_canopy_height_900m, 
    
    #starting conditions
    tree_cover_1000m_2015_2016, canopy_height_900m_2000,
    
    # environmental predictors
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, months_extreme_drought,
    mat_coef, prec_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2,# density_trend_estimate, density_trend_estimate,
    
    #Trends - Responses 
    tree_cover_1000m_coef, canopy_height_900m_coef, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, country_code_iso3,
    
    #identifieres 
    park_id, cluster_id, grid_id
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    x_moll_km = x_mollweide/1000, 
    y_moll_km = y_mollweide/1000,
  ) %>%
  group_by(park_id) %>% 
  # mutate(park_row_nr = 1:n()) %>% 
  #  filter(park_row_nr %in% acceptable_numbers) %>% 
  filter(n() >= 500) %>% #only include parks more than 500 obs
  ungroup() %>% 
  as.data.table() 


dt_long <- dt_mod %>% pivot_longer(
  cols = c("local_density_km2",
           "months_extreme_drought", "fire_frequency", 
           "prec_coef"), 
  names_to = "var_name", 
  values_to = "var_value") %>% 
  mutate(var_clean = case_when(
    var_name == "local_density_km2" ~ "Local Elephant Density",
    var_name == "months_extreme_drought" ~ "N Drought Months",
    var_name == "fire_frequency" ~ "Fire Frequency",
    var_name == "prec_coef" ~ "Rainfall Change",
  )) %>% 
  pivot_longer(
    cols = c("canopy_height_900m_coef", "tree_cover_1000m_coef"), 
    names_to = "response_name", 
    values_to = "response_value") %>% 
  mutate(response_clean = case_when(
    response_name == "canopy_height_900m_coef" ~ "Vegetation Height Trend",
    response_name == "tree_cover_1000m_coef" ~ "Woody Cover Trend"  ))


### Plot all park / response combinations separately
p_smooth_tc_1 <- dt_pred_comp %>% 
  filter(park %in% c("Chobe")) %>% 
  filter(response_clean == "Woody Cover Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Chobe") & response_clean == "Woody Cover Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#262600") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#262600") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Woody Cover Change", title = "Chobe National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_tc_1

p_smooth_tc_2 <- dt_pred_comp %>% 
  filter(park %in% c("Hwange")) %>% 
  filter(response_clean == "Woody Cover Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Hwange") & response_clean == "Woody Cover Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#262600") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#262600") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Woody Cover Change", title = "Hwange National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_tc_2

p_smooth_tc_3 <- dt_pred_comp %>% 
  filter(park %in% c("Kruger National Park")) %>% 
  filter(response_clean == "Woody Cover Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Kruger National Park") & response_clean == "Woody Cover Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#262600") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#262600") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Woody Cover Change", title = "Kruger National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_tc_3

p_smooth_tc_4 <- dt_pred_comp %>% 
  filter(park %in% c("Limpopo")) %>% 
  filter(response_clean == "Woody Cover Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Limpopo") & response_clean == "Woody Cover Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#262600") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#262600") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Woody Cover Change", title = "Limpopo National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_tc_4


p_smooth_tc_5 <- dt_pred_comp %>% 
  filter(park %in% c("Luengue-Luiana National Park")) %>% 
  filter(response_clean == "Woody Cover Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Luengue-Luiana National Park") & response_clean == "Woody Cover Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#262600") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#262600") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Woody Cover Change", title = "Luengue-Luiana National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_tc_5

library(patchwork)

p_tc <- (p_smooth_tc_1 / 
  p_smooth_tc_2 / 
  p_smooth_tc_3 / 
  p_smooth_tc_4 / 
  p_smooth_tc_5)



ggsave(plot = p_tc, "builds/plots/supplement/park_level_tc_1000m_model_predictions.png", dpi = 900, height = 10, width = 8)

#Canopy Height
p_smooth_ch_1 <- dt_pred_comp %>% 
  filter(park %in% c("Chobe")) %>% 
  filter(response_clean == "Canopy Height Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Chobe") & response_clean == "Canopy Height Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#0C4C00") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#0C4C00") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Canopy Height Change", title = "Chobe National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_ch_1

p_smooth_ch_2 <- dt_pred_comp %>% 
  filter(park %in% c("Hwange")) %>% 
  filter(response_clean == "Canopy Height Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Hwange") & response_clean == "Canopy Height Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#0C4C00") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#0C4C00") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Canopy Height Change", title = "Hwange National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_ch_2

p_smooth_ch_3 <- dt_pred_comp %>% 
  filter(park %in% c("Kruger National Park")) %>% 
  filter(response_clean == "Canopy Height Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Kruger National Park") & response_clean == "Canopy Height Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#0C4C00") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#0C4C00") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Canopy Height Change", title = "Kruger National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_ch_3

p_smooth_ch_4 <- dt_pred_comp %>% 
  filter(park %in% c("Limpopo")) %>% 
  filter(response_clean == "Canopy Height Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Limpopo") & response_clean == "Canopy Height Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#0C4C00") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#0C4C00") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Canopy Height Change", title = "Limpopo National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_ch_4


p_smooth_ch_5 <- dt_pred_comp %>% 
  filter(park %in% c("Luengue-Luiana National Park")) %>% 
  filter(response_clean == "Canopy Height Trend") %>% 
  ggplot() +
  geom_point(data = dt_long %>% 
               filter(park_id %in% c("Luengue-Luiana National Park") & response_clean == "Canopy Height Trend"),
             aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#0C4C00") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1,color = "#0C4C00") +
  facet_wrap(~var_clean, scales = "free", ncol = 4) +
  labs(y = "Canopy Height Change", title = "Luengue-Luiana National Park", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_ch_5

library(patchwork)

p_ch <- (p_smooth_ch_1 / 
           p_smooth_ch_2 / 
           p_smooth_ch_3 / 
           p_smooth_ch_4 / 
           p_smooth_ch_5)



ggsave(plot = p_ch, "builds/plots/supplement/park_level_ch_1000m_model_predictions.png", dpi = 900, height = 10, width = 8)

