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
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100) 



# get dataframe with comlete and clean data fro mdoeling 

quantile(dt$dw_min_median_mode_fraction, na.rm = T)
table(dt$population_trend_n)
dt_pad <- dt %>% 
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
    x_mollweide, y_mollweide, lon, lat, 
    
    #identifieres 
    park_id, cluster_id, grid_id
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    x_moll_km = x_mollweide/1000, 
    y_moll_km = y_mollweide/1000,
  ) %>%
  group_by(cluster_id, park_id) %>% 
  summarize(tree_cover_1000m_coef = mean(tree_cover_1000m_coef), 
            canopy_height_900m_coef = mean(canopy_height_900m_coef), 
            mean_density_km2 = mean(mean_density_km2),
            elevation = mean(elevation), 
            mat  = mean(mat),
            map = mean(map),
            n_deposition = mean(n_deposition), 
            human_modification = mean(human_modification), 
            fire_frequency = mean(fire_frequency),
            months_extreme_drought = mean(months_extreme_drought),
            mat_coef = mean(mat_coef),
            prec_coef = mean(prec_coef), 
            sd_local_density_km2 = sd(local_density_km2), 
            local_density_km2 = mean(local_density_km2),
            n = n()
  ) %>% 
  mutate(cv_local_density_km2 = (sd_local_density_km2/local_density_km2)*100) %>% 
  ungroup() %>% 
  mutate(mean_density_km2_scaled = as.numeric(scale(mean_density_km2))) #%>% mutate(mean_density_km2_scaled = mean_density_km2)


dt_pred <- fread("builds/model_outputs/gam_park_average_density_predictions.csv")


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


scico(palette = "broc", n = 9)
c("#65014B", "#A4428B", "#D07EBB", "#EBC5E1", "#F5F0F0", "#D7E7C0", "#8CB464", "#4B802E", "#0C4C00")
c("#2C194C", "#294A7C", "#5A81A8", "#A3BACF", "#EAEDEB", "#D3D3A8", "#9A9A61", "#5C5C2C", "#262600")

p_pred <- dt_pred %>% 
 # filter(response_name == "tree_cover_1000m_coef") %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high, 
                  fill = response_clean, color= response_clean), alpha = 0.3, linetype = "dashed") +
  geom_line(aes(x = x_unscaled, y = predicted, response_clean), linewidth = 1.1) +
  facet_wrap(~response_clean, scales = "free") +
  scale_color_manual(values = c("#0C4C00", "#262600")) +
  scale_fill_manual(values = c("#0C4C00", "#262600")) + 
  geom_rect(data = dt_rects, aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax), 
            fill = "snow", alpha = 0.5, inherit.aes = FALSE) +
  geom_rect(data = dt_rects, aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax), 
            fill = "snow", alpha = 0.5, inherit.aes = FALSE) +
  labs(y = "Response Value", title = "", x = "Elephant Density (Individuals/km²)", 
       subtitle = "D") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_pred


### Map ---------

sf_world <- ne_countries(scale = "medium", returnclass = "sf")

# Africa 
sf_africa <- sf_world %>% filter(region_un == "Africa") %>% 
  filter(!name == "Madagascar")


sf_pas <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  st_transform(., 4326) %>% 
  filter(NAME %in% unique(dt$park_id)) %>% 
  left_join(dt_pad %>% 
              dplyr::select(NAME = park_id, mean_density_km2, tree_cover_1000m_coef, canopy_height_900m_coef) %>%
              unique()) 

x <- st_coordinates(st_centroid(sf_pas))[,1]
y <- st_coordinates(st_centroid(sf_pas))[,2]

range(x)
range(y)
p_pa_dens <- sf_pas %>% 
  ggplot() +
  ylim(min(y) - 2 , max(y) + 2) +
  xlim(min(x) - 2 , max(x) + 2) +
  geom_sf(data = sf_africa, fill = "linen", color = "ivory3", alpha = .25) +
  scale_color_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico(palette = "batlow", begin = 0.2, end = 0.8) +
  geom_sf(aes(fill = mean_density_km2, color = mean_density_km2)) +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tl",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  labs(fill = "Mean Elephant Density\n(individuals/km²)", 
       color = "Mean Elephant Density\n(individuals/km²)",
       subtitle = "A") +
  theme(legend.position = "bottom")
p_pa_dens


p_pa_ch <- sf_pas %>% 
  ggplot() +
  ylim(min(y) - 2 , max(y) + 2) +
  xlim(min(x) - 2 , max(x) + 2) +
  geom_sf(data = sf_africa, fill = "linen", color = "ivory3", alpha = .25) +
  scale_color_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  scale_fill_scico(palette = "bam", midpoint = 0, begin = 0.05, end = 0.95) +
  geom_sf(aes(fill = canopy_height_900m_coef, color = canopy_height_900m_coef)) +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tl",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  labs(fill = "Canopy Height Trend", 
       color = "Canopy Height Trend", 
       subtitle = "B") +
  theme(legend.position = "bottom")
p_pa_ch

p_pa_tc <- sf_pas %>% 
  ggplot() +
  ylim(min(y) - 2 , max(y) + 2) +
  xlim(min(x) - 2 , max(x) + 2) +
  geom_sf(data = sf_africa, fill = "linen", color = "ivory3", alpha = .25) +
  scale_color_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  scale_fill_scico(palette = "broc", midpoint = 0, begin = 0.05, end = 0.95, direction = 1) +
  geom_sf(aes(fill = tree_cover_1000m_coef, color = tree_cover_1000m_coef)) +
  annotation_scale(location = "bl", bar_cols = c("ivory4", "white")) +
  annotation_north_arrow(location = "tl",
                         height = unit(0.8, "cm"),  # smaller arrow
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  labs(fill = "Woody Cover Trend", 
       color = "Woody Cover Trend", 
       subtitle = "C") +
  theme(legend.position = "bottom")
p_pa_tc


library(cowplot)

p_empty = ggplot() +theme_void()
top_row <- plot_grid(
  p_pa_dens,
  p_empty,
  p_pa_ch,
  p_empty,
  p_pa_tc,
  nrow = 1,
  rel_widths = c(1, 0.03, 1, 0.03, 1)
)

p_maps <- plot_grid(
  top_row,
  p_pred,
  ncol = 1,
  rel_heights = c(1.3, 1)
)

p_maps
ggsave(plot = p_maps, "builds/plots/reserve_density_figure.png", 
       dpi = 900, height = 10, width = 10)

