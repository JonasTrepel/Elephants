library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(broom)
library("sdmTMB")
library(future)
library(furrr)
library(groupdata2)
library(GGally)
library(glmmTMB)
library(ggridges)
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

#load data 
#sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100, 
  ) %>% 
  filter(park_id != "Thornybush Nature Reserve") #counts likely wrong - mean density of 6.3 elephants seems unrealistic



# get dataframe with comlete and clean data fro mdoeling 

quantile(dt$dw_min_median_mode_fraction, na.rm = T)
acceptable_numbers = seq(1, 10000000, 5)
table(dt$population_trend_n)
dt_mod <- dt %>% 
  filter(dw_min_median_mode_fraction >= 50) %>% 
  dplyr::select(
    #mean values /habitat characteristics 
    mean_tree_cover_1000m, mean_evi_900m, mean_canopy_height_900m, 
    
    #starting conditions
    tree_cover_1000m_2015_2016, evi_900m_2013_2014, canopy_height_900m_2000,
    
    # environmental predictors
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, months_severe_drought, months_extreme_drought,
    mat_coef, prec_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2,# density_trend_estimate, density_trend_estimate,
    
    #Trends - Responses 
    tree_cover_1000m_coef, evi_900m_coef, canopy_height_900m_coef, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, country_code_iso3,
    
    distance_to_water_km, distance_to_settlement_km, slope, human_modification, mean_evi_900m,
    
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
    local_density_km2_scaled = as.numeric(scale(local_density_km2)),
    mean_density_km2_scaled = as.numeric(scale(mean_density_km2)),
    months_extreme_drought_scaled = as.numeric(scale(months_extreme_drought)),
    fire_frequency_scaled = as.numeric(scale(fire_frequency)),
    mat_coef_scaled = as.numeric(scale(mat_coef)),
    prec_coef_scaled = as.numeric(scale(prec_coef)),
    n_deposition_scaled = as.numeric(scale(n_deposition)), 
    mat_scaled = as.numeric(scale(mat)), 
    map_scaled = as.numeric(scale(map))
  ) %>%
  mutate(fenced = ifelse(country_code_iso3 == "ZAF", "fenced", "unfenced"))



dt_corr = dt_mod %>% 
  select(distance_to_water_km, distance_to_settlement_km, slope, human_modification, mean_evi_900m,
         tree_cover_1000m_coef, canopy_height_900m_coef)

corr <- round(cor(dt_corr), 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

names(dt_mod)

p_vars = dt_mod %>% 
  pivot_longer(
    cols = c(
      elevation, mat, map, n_deposition, slope,
      months_extreme_drought, mat_coef, prec_coef,
      mean_evi_900m, mean_density_km2
    ),
    names_to = "var_name",
    values_to = "var_value"
  ) %>% 
  mutate(
    clean_name = case_when(
      var_name == "elevation" ~ "Elevation (m)",
      var_name == "mat" ~ "Mean Annual Temperature (°C)",
      var_name == "map" ~ "Mean Annual Precipitation (mm)",
      var_name == "n_deposition" ~ "N Deposition (kg/km2/year)",
      var_name == "slope" ~ "Slope",
      var_name == "months_extreme_drought" ~ "Drought (months)",
      var_name == "mat_coef" ~ "Temp Trend",
      var_name == "prec_coef" ~ "Precip Trend",
      var_name == "mean_evi_900m" ~ "Mean EVI",
      var_name == "mean_density_km2" ~ "Elephant Density"
    )
  ) %>% 
  ggplot(aes(x = fenced, y = var_value, fill= fenced)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~clean_name, scales = "free_y", ncol = 5) +
  labs(y = "Variable Value", x = "") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle  = 90))
p_vars
ggsave(plot = p_vars, "builds/plots/supplement/variables_fenced_vs_unfenced.png", 
       dpi = 900, height = 5, width = 10)


#1 HOUSEKEEPING -------------------------------------

dt_pred <- fread("builds/model_outputs/subset_predictions_1000m.csv")  %>% 
  filter(tier_clean %in% c("Fenced", "Unfenced")) %>% 
  mutate(response_clean = case_when(
    .default = response_clean, 
    response_clean == "Tree Cover Trend" ~ "Woody Cover Trend"
  ))


dt_dev <- dt_pred %>% 
  dplyr::select(dev_explained_full, dev_explained_var, tier_clean, response_clean) %>% 
  unique()

####### Get rectangles ready: 
dt_rect_ch_uf <- dt_pred %>%
  filter(tier_clean %in% c("Unfenced") & response_clean %in% c("Canopy Height Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_ch_f <- dt_pred %>%
  filter(tier_clean %in% c("Fenced") & response_clean %in% c("Canopy Height Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()


dt_rect_tc_uf <- dt_pred %>%
  filter(tier_clean %in% c("Unfenced") & response_clean %in% c("Woody Cover Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_tc_f <- dt_pred %>%
  filter(tier_clean %in% c("Fenced") & response_clean %in% c("Woody Cover Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

##### Plot different tiers -------------

p_ch_uf <- dt_pred %>% 
  filter(tier_clean %in% c("Unfenced") & response_clean %in% c("Canopy Height Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#40631F") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#40631F") +
  geom_rect(data = dt_rect_ch_uf,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_ch_uf,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Canopy Height Trend", x = "Variable Value", title = "Unfenced") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_ch_uf

p_ch_f <- dt_pred %>% 
  filter(tier_clean %in% c("Fenced") & response_clean %in% c("Canopy Height Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#40631F") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#40631F") +
  geom_rect(data = dt_rect_ch_f,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_ch_f,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Canopy Height Trend", x = "Variable Value", title = "Fenced") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_ch_f

p_tc_uf <- dt_pred %>% 
  filter(tier_clean %in% c("Unfenced") & response_clean %in% c("Woody Cover Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#215F61") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#215F61") +
  geom_rect(data = dt_rect_tc_uf,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_tc_uf,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Woody Cover Trend", x = "Variable Value", title = "Unfenced") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_tc_uf

p_tc_f <- dt_pred %>% 
  filter(tier_clean %in% c("Fenced") & response_clean %in% c("Woody Cover Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#215F61") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#215F61") +
  geom_rect(data = dt_rect_tc_f,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_tc_f,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Woody Cover Trend", x = "Variable Value", title = "Fenced") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_tc_f


library(patchwork)
p <- p_ch_uf / p_ch_f / p_tc_uf / p_tc_f
p
ggsave(plot = p, "builds/plots/supplement/subset_fenced_vs_unfenced_predictions_1000m.png", 
       dpi = 900, height = 8, width = 9)
