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
library(sf)
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

cents <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  st_centroid(.) %>% 
  mutate(x_moll = (st_coordinates(.)[,1]), 
         y_moll = (st_coordinates(.)[,2]), 
         x_moll_km = x_moll/1000, 
         y_moll_km = y_moll/1000) %>% 
  as.data.table() %>% 
  mutate(geom = NULL) %>% 
  dplyr::select(park_id = NAME, area_km2, x_moll, y_moll, x_moll_km, y_moll_km)

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
  left_join(cents) %>% 
  ungroup() %>% 
  mutate(cv_local_density_km2_scaled = as.numeric(scale(cv_local_density_km2)),
         mean_density_km2_scaled = as.numeric(scale(mean_density_km2)),
         x_moll_scaled = as.numeric(scale(x_moll)), 
         y_moll_scaled = as.numeric(scale(y_moll))) #%>% mutate(cv_local_density_km2_scaled = mean_density_km2)


#nothing seems to correlate with mean elephant density too badly...   

#### Models -------
#WOody coer 
m_pad_tc = gam(tree_cover_1000m_coef ~ s(cv_local_density_km2_scaled, k = 3), 
               select=TRUE,
               data = dt_pad,
               method = "REML")
summary(m_pad_tc)

m_pad_tc_sp = gam(tree_cover_1000m_coef ~ cv_local_density_km2_scaled*mean_density_km2_scaled +
                    s(y_moll_scaled),  
                  select=TRUE,
                  data = dt_pad,
                  method = "REML")
summary(m_pad_tc_sp)

# Canopy height
m_pad_ch = gam(canopy_height_900m_coef ~ s(cv_local_density_km2_scaled, k = 3), 
               select=TRUE,
               data = dt_pad,
               method = "REML")
summary(m_pad_ch)

m_pad_ch_sp = gam(canopy_height_900m_coef ~ cv_local_density_km2_scaled*mean_density_km2_scaled +
                    s(y_moll_scaled),  
                  select=TRUE,
                  data = dt_pad,
                  method = "REML")
summary(m_pad_ch_sp)



### Check SP
dt_pad_sf <- st_as_sf(dt_pad, coords = c("x_moll", "y_moll"), crs = "ESRI:54009") %>% 
  mutate(resid_ch = residuals(m_pad_ch), 
         resid_ch_sp = residuals(m_pad_ch_sp), 
         resid_tc = residuals(m_pad_tc), 
         resid_tc_sp = residuals(m_pad_tc_sp))


library(spdep)
coords <- st_coordinates(dt_pad_sf)
knn <- knearneigh(coords, k = 5)
nb_knn <- knn2nb(knn)

lw <- nb2listw(nb_knn, style = "W")

(mi_tc <- moran.test(dt_pad_sf$resid_tc, lw))
(mi_tc_sp <- moran.test(dt_pad_sf$resid_tc_sp, lw))

(mi_ch <- moran.test(dt_pad_sf$resid_ch, lw))
(mi_ch_sp <- moran.test(dt_pad_sf$resid_ch_sp, lw))


## extract ---------------
mean_x <- mean(dt_pad$mean_density_km2, na.rm = TRUE)
sd_x   <- sd(dt_pad$mean_density_km2, na.rm = TRUE)

pred_tc = ggeffects::ggpredict(m_pad_tc_sp, term = c("cv_local_density_km2_scaled", "mean_density_km2_scaled"))
plot(pred_tc)


p_tc = ggplot(pred_tc, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.15, color = NA) +
  scale_color_brewer(palette = "Dark2", name = "Mean density (scaled)") +
  scale_fill_brewer(palette = "Dark2", name = "Mean density (scaled)") +
  labs(
    x = "Variability in Elephant Density\n(Coefficient of Variation, scaled)",
    y = "Woody Cover Change"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank())

pred_ch = ggeffects::ggpredict(m_pad_ch_sp, term = c("cv_local_density_km2_scaled", "mean_density_km2_scaled"))
plot(pred_ch)


p_ch = ggplot(pred_ch, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.15, color = NA) +
  scale_color_brewer(palette = "Accent", name = "Mean density (scaled)") +
  scale_fill_brewer(palette = "Accent", name = "Mean density (scaled)") +
  labs(
    x = "Variability in Elephant Density\n(Coefficient of Variation; scaled)",
    y = "Canopy Height Change"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank())
p_ch
library(patchwork)

p = p_ch | p_tc
p

ggsave(plot = p, "builds/plots/supplement/variability_exploratory.png", 
       dpi = 900, height = 4, width = 8)
