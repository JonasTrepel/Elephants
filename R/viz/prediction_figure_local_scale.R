library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(broom)
library("sdmTMB")
library(rnaturalearth)
library(future)
library(ggspatial)
library(groupdata2)
library(GGally)
library(scico)
library(sf)
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

#load data 
#sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100) %>% 
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
  as.data.table() 

n_distinct(dt$park_id)
(park_counts <- dt[, .N, by = park_id] %>% arrange(N))
median(park_counts$N)
sd(park_counts$N)
nrow(dt)
cor.test(dt$prec_coef, dt$mat_coef)

dt_pred <- fread("builds/model_outputs/sdmtmb_1000m_predictions.csv")


dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100) %>% 
  filter(include = TRUE)

names(dt)

# get dataframe with comlete and clean data fro mdoeling 

quantile(dt$dw_min_median_mode_fraction, na.rm = T)

names(dt)

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
    prec_coef, prec_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2,
    
    #Trends - Responses 
    tree_cover_1000m_coef, canopy_height_900m_coef, 
    
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
    months_extreme_drought_scaled = as.numeric(scale(months_extreme_drought)),
    fire_frequency_scaled          = as.numeric(scale(fire_frequency)),
    n_deposition_scaled            = as.numeric(scale(n_deposition)), 
    mat_scaled = as.numeric(scale(mat)), 
    map_scaled = as.numeric(scale(map))
  )

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
    var_name == "n_deposition" ~ "N Deposition",
  )) %>% 
  pivot_longer(
    cols = c("canopy_height_900m_coef", "tree_cover_1000m_coef"), 
    names_to = "response_name", 
    values_to = "response_value") %>% 
  mutate(response_clean = case_when(
    response_name == "canopy_height_900m_coef" ~ "Vegetation Height Trend",
    response_name == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
    response_name == "evi_900m_coef" ~ "EVI Trend"
  ))


dt_rects <- dt_pred %>%
  dplyr::select(q975_unscaled, q025_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf,
    ymax = Inf,
    xmin1 = -Inf,
    xmax1 = first(q025_unscaled),
    xmin2 = first(q975_unscaled),
    xmax2 = Inf
  ) %>%
  ungroup()


scico(palette = "batlow", n = 9)
c("#003A46", "#0F443E", "#245131", "#40631F", "#61790A", "#898800", "#B4A022", "#DDC464", "#FFE5AC")
c("#0019759", "#114260", "#215F61", "#4C724D", "#818231", "#BE9035", "#F19D6B", "#FDB4B4", "#F9CCF9")

p_tc <- dt_pred %>% 
  filter(response_name == "tree_cover_1000m_coef") %>% 
  mutate(response_clean = case_when(
    .default = response_clean, 
    response_clean == "Tree Cover Trend" ~ "Woody Cover Trend"
  )) %>% 
  ggplot() +
  geom_hex(data = dt_long %>% 
             filter(response_name == "tree_cover_1000m_coef"),
           aes(x = var_value, y = response_value), alpha = 0.5) +
  scale_fill_scico(palette = "batlow", trans = "log10", name = "Number of\nObservations") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#262600") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#262600") +
  facet_grid(rows = vars(response_clean), cols = vars(var_clean), scales = "free") +
  # geom_rect(data = dt_rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
  #           fill = "snow", alpha = 0.6, inherit.aes = FALSE) +
  # geom_rect(data = dt_rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
  #           fill = "snow", alpha = 0.6, inherit.aes = FALSE) +
  labs(y = "Woody Cover Trend", title = "", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_tc


p_ch <- dt_pred %>% 
  filter(response_name == "canopy_height_900m_coef") %>% 
  ggplot() +
  geom_hex(data = dt_long %>% 
             filter(response_name == "canopy_height_900m_coef"),
           aes(x = var_value, y = response_value), alpha = 0.5) +
  scale_fill_scico(palette = "batlow", trans = "log10", name = "Number of\nObservations") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#0C4C00") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#0C4C00") +
  facet_grid(rows = vars(response_clean), cols = vars(var_clean), scales = "free") +
  # geom_rect(data = dt_rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
  #           fill = "snow", alpha = 0.6, inherit.aes = FALSE) +
  # geom_rect(data = dt_rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
  #           fill = "snow", alpha = 0.6, inherit.aes = FALSE) +
  labs(y = "Vegetation Height Trend", title = "", x = "Predictor Value") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_ch

library(patchwork)
p_comb <- (p_ch / p_tc)
p_comb
ggsave(plot = p_comb, "builds/plots/local_scale_predictions_main_figures.png", 
       height = 5, width = 8, dpi = 900)
