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
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100, 
  ) %>% filter(park_id != "Thornybush Nature Reserve") %>% 
  mutate(canopy_height_900m_coef_na = ifelse(
    dw_min_median_mode_fraction >= 50, 
    NA, 
    1), 
    tree_cover_1000m_coef_na = ifelse(
      dw_min_median_mode_fraction >= 50, 
      NA, 
      1)) %>% 
  dplyr::select(
    canopy_height_900m_coef_na, tree_cover_1000m_coef_na,
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
    x_mollweide, y_mollweide, lon, lat, 
    #identifieres 
    park_id, cluster_id, grid_id
  ) %>% 
 # filter(complete.cases(.)) %>% 
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


dt_pred <- fread("builds/model_outputs/park_level_predictions_1000m.csv")

dt_dev <- dt_pred %>% 
  dplyr::select(dev_explained_full, dev_explained_var, park, response_clean) %>% 
  unique()

####### Get rectangles ready: 

dt_rect_hip <- dt_pred %>%
  filter(park %in% c("Hluhluwe – iMfolozi Park")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_knp <- dt_pred %>%
  filter(park %in% c("Kruger National Park")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_hwange <- dt_pred %>%
  filter(park %in% c("Hwange")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_chobe <- dt_pred %>%
  filter(park %in% c("Chobe")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect <- dt_pred %>%
  filter(var_clean %in% c("Local Elephant Density")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean, park) %>% 
  unique() %>% 
  group_by(park) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

#### general map ------

### Map ---------

sf_world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(., crs = "ESRI:54009")

# Africa 
sf_africa <- sf_world %>% filter(region_un == "Africa") %>% 
  filter(!name == "Madagascar") %>% 
  st_transform(., crs = "ESRI:54009")


sf_pas <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  filter(NAME %in% unique(dt$park_id)) %>% 
  left_join(dt %>% 
              dplyr::select(NAME = park_id, mean_density_km2) %>%
              unique()) %>% 
  st_transform(., crs = "ESRI:54009") 

x <- st_coordinates(st_centroid(sf_pas))[,1]
y <- st_coordinates(st_centroid(sf_pas))[,2]

range(x)
range(y)
p_pa <- sf_pas %>% 
  ggplot() +
  ylim(-4060000, -1100000) +
  xlim(1200000, 4000000) +
  geom_sf(data = sf_africa, fill = "linen", color = "grey10") +
  scale_color_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  geom_sf(aes(), color = "white", fill = "ivory4", alpha = 0.75) +
  geom_sf(data = sf_pas %>% 
            filter(NAME %in% c("Hluhluwe – iMfolozi Park", 
                                  "Kruger National Park",
                                  "Hwange", 
                                  "Chobe"
                                  )), color = "white", fill = "black") +
  theme_void() +
  labs(fill = "Mean Elephant Density\n(individuals/km²)") +
  theme(legend.position = "none")
p_pa

ggsave(plot = p_pa, "builds/plots/park_level_trends/ssa_and_parks.png", dpi = 900, 
       height = 4, width = 4)


################# Canopy height -------------------------

## 

m_ch_hip <- dt %>% 
  filter(park_id %in% c("Hluhluwe – iMfolozi Park")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_900m_coef)) +
  geom_tile(data = dt %>% 
              filter(park_id %in% c("Hluhluwe – iMfolozi Park")) %>% 
              filter(!is.na(canopy_height_900m_coef_na)),
            aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_900m_coef_na),
            fill = "grey25", alpha = 0.25) +
  scale_color_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  labs(title = "Hluhluwe – iMfolozi Park", fill = "Canopy\nHeight\nTrend") +
  annotation_scale(location = "br", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tl",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "left") 
m_ch_hip

ggsave(plot = m_ch_hip, "builds/plots/park_level_trends/hip_ch_map.png", dpi = 900, 
       height = 4, width = 4)


m_ch_knp <- dt %>% 
  filter(park_id %in% c("Kruger National Park")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_900m_coef)) +
  scale_color_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  geom_tile(data = dt %>% 
              filter(park_id %in% c("Kruger National Park")) %>% 
              filter(!is.na(canopy_height_900m_coef_na)),
            aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_900m_coef_na),
            fill = "grey25", alpha = 0.25) +
  labs(title = "Kruger National Park", fill = "Canopy\nHeight\nTrend") +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tr",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "right") 
m_ch_knp

ggsave(plot = m_ch_knp, "builds/plots/park_level_trends/knp_ch_map.png", dpi = 900, 
       height = 4, width = 4)


m_ch_hwange <- dt %>% 
  filter(park_id %in% c("Hwange")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_900m_coef)) +
  geom_tile(data = dt %>% 
              filter(park_id %in% c("Hwange")) %>% 
              filter(!is.na(canopy_height_900m_coef_na)),
            aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_900m_coef_na),
            fill = "grey25", alpha = 0.25) +
  scale_color_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  labs(title = "Hwange", fill = "Canopy\nHeight\nTrend") +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tr",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "right") 
m_ch_hwange

ggsave(plot = m_ch_hwange, "builds/plots/park_level_trends/hwange_ch_map.png", dpi = 900, 
       height = 4, width = 4)

m_ch_chobe <- dt %>% 
  filter(park_id %in% c("Chobe")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_900m_coef)) +
  scale_color_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  geom_tile(data = dt %>% 
              filter(park_id %in% c("Chobe")) %>% 
              filter(!is.na(canopy_height_900m_coef_na)),
            aes(x = x_mollweide, y = y_mollweide, fill = canopy_height_900m_coef_na),
            fill = "grey25", alpha = 0.25) +
  labs(title = "Chobe", fill = "Canopy\nHeight\nTrend") +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tl",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "left") 
m_ch_chobe

ggsave(plot = m_ch_chobe, "builds/plots/park_level_trends/chobe_ch_map.png", dpi = 900, 
       height = 4, width = 4)


scico(palette = "batlow", n = 9)
c("#003A46", "#0F443E", "#245131", "#40631F", "#61790A", "#898800", "#B4A022", "#DDC464", "#FFE5AC")
c("#001959", "#114260", "#215F61", "#4C724D", "#818231", "#BE9035", "#F19D6B", "#FDB4B4", "#F9CCF9")

p_ch <- dt_pred %>% 
  #filter(park %in% c("Hluhluwe – iMfolozi Park")) %>% 
  filter(response_name == "canopy_height_900m_coef" & var_clean == "Local Elephant Density") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#40631F") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#40631F") +
  geom_rect(data = dt_rect,
           aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
           fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
           fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Canopy Height Trend", x = "Local Elephant Density") +
  theme_bw() +
  facet_wrap(~park, scales = "free_x", ncol = 4) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_ch

ggsave(plot = p_ch, "builds/plots/park_level_trends/canopy_height_predictions.png", 
       height = 3, width = 12, dpi = 900)


################# Tree Cover Trend  -------------------------

## 

m_tc_hip <- dt %>% 
  filter(park_id %in% c("Hluhluwe – iMfolozi Park")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_1000m_coef)) +
  geom_tile(data = dt %>% 
              filter(park_id %in% c("Hluhluwe – iMfolozi Park")) %>% 
              filter(!is.na(tree_cover_1000m_coef_na)),
            aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_1000m_coef_na),
            fill = "grey25", alpha = 0.25) +
  scale_color_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  scale_fill_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  labs(title = "Hluhluwe – iMfolozi Park", fill = "Tree\nCover\nTrend") +
  annotation_scale(location = "br", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tl",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "left") 
m_tc_hip

ggsave(plot = m_tc_hip, "builds/plots/park_level_trends/hip_tc_map.png", dpi = 900, 
       height = 4, width = 4)


m_tc_knp <- dt %>% 
  filter(park_id %in% c("Kruger National Park")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_1000m_coef)) +
  scale_color_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  scale_fill_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  geom_tile(data = dt %>% 
              filter(park_id %in% c("Kruger National Park")) %>% 
              filter(!is.na(tree_cover_1000m_coef_na)),
            aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_1000m_coef_na),
            fill = "grey25", alpha = 0.25) +
  labs(title = "Kruger National Park", fill = "Tree\nCover\nTrend") +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tr",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "right") 
m_tc_knp

ggsave(plot = m_tc_knp, "builds/plots/park_level_trends/knp_tc_map.png", dpi = 900, 
       height = 4, width = 4)


m_tc_hwange <- dt %>% 
  filter(park_id %in% c("Hwange")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_1000m_coef)) +
  geom_tile(data = dt %>% 
              filter(park_id %in% c("Hwange")) %>% 
              filter(!is.na(tree_cover_1000m_coef_na)),
            aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_1000m_coef_na),
            fill = "grey25", alpha = 0.25) +
  scale_color_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  scale_fill_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  labs(title = "Hwange", fill = "Tree\nCover\nTrend") +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tr",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "right") 
m_tc_hwange

ggsave(plot = m_tc_hwange, "builds/plots/park_level_trends/hwange_tc_map.png", dpi = 900, 
       height = 4, width = 4)

m_tc_tcobe <- dt %>% 
  filter(park_id %in% c("Chobe")) %>% 
  ggplot() +
  geom_tile(aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_1000m_coef)) +
  scale_color_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  scale_fill_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  geom_tile(data = dt %>% 
              filter(park_id %in% c("Chobe")) %>% 
              filter(!is.na(tree_cover_1000m_coef_na)),
            aes(x = x_mollweide, y = y_mollweide, fill = tree_cover_1000m_coef_na),
            fill = "grey25", alpha = 0.25) +
  labs(title = "Chobe", fill = "Tree\nCover\nTrend") +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tl",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "left") 
m_tc_tcobe

ggsave(plot = m_tc_tcobe, "builds/plots/park_level_trends/chobe_tc_map.png", dpi = 900, 
       height = 4, width = 4)


scico(palette = "batlow", n = 9)
c("#001959", "#114260", "#215F61", "#4C724D", "#818231", "#BE9035", "#F19D6B", "#FDB4B4", "#F9CCF9")

p_tc <- dt_pred %>% 
  #filter(park %in% c("Hluhluwe – iMfolozi Park")) %>% 
  filter(response_name == "tree_cover_1000m_coef" & var_clean == "Local Elephant Density") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#215F61") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#215F61") +
  geom_rect(data = dt_rect,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Tree Cover Trend", x = "Local Elephant Density") +
  theme_bw() +
  facet_wrap(~park, scales = "free_x", ncol = 4) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_tc

ggsave(plot = p_tc, "builds/plots/park_level_trends/tree_cover_predictions.png", 
       height = 3, width = 12, dpi = 900)

##### all preds -------------------------

c("#003A46", "#0F443E", "#245131", "#40631F", "#61790A", "#898800", "#B4A022", "#DDC464", "#FFE5AC")

p_hip <- dt_pred %>% 
  filter(park %in% c("Hluhluwe – iMfolozi Park")) %>% 
  #filter(response_name == "tree_cover_1000m_coef") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#B4A022") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#B4A022") +
  geom_rect(data = dt_rect_hip,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_hip,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Response Value", x = "Variable Value", title = "Hluhluwe – iMfolozi Park") +
  theme_bw() +
  facet_grid(cols = vars(var_clean), rows = vars(response_clean),  scales = "free") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_hip

p_knp <- dt_pred %>% 
  filter(park %in% c("Kruger National Park")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#61790A") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#61790A") +
  geom_rect(data = dt_rect_knp,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_knp,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Response Value", x = "Variable Value", title = "Kruger National Park") +
  theme_bw() +
  facet_grid(cols = vars(var_clean), rows = vars(response_clean),  scales = "free") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_knp

p_chobe <- dt_pred %>% 
  filter(park %in% c("Chobe")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#245131") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#245131") +
  geom_rect(data = dt_rect_chobe,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_chobe,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Response Value", x = "Variable Value", title = "Chobe") +
  theme_bw() +
  facet_grid(cols = vars(var_clean), rows = vars(response_clean),  scales = "free") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_chobe

p_hwange <- dt_pred %>% 
  filter(park %in% c("Hwange")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#003A46") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#003A46") +
  geom_rect(data = dt_rect_hwange,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_hwange,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Response Value", x = "Variable Value", title = "Hwange") +
  theme_bw() +
  facet_grid(cols = vars(var_clean), rows = vars(response_clean),  scales = "free") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_hwange

p_all <- p_hip / p_knp / p_chobe / p_hwange
p_all
ggsave(plot = p_all, "builds/plots/supplement/park_level_predictions.png", 
       dpi = 900, height = 14, width = 8)
