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
library(mgcv)
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
  mutate(mean_density_km2_scaled = as.numeric(scale(mean_density_km2)),
         x_moll_scaled = as.numeric(scale(x_moll)), 
         y_moll_scaled = as.numeric(scale(y_moll))) #%>% mutate(mean_density_km2_scaled = mean_density_km2)

glimpse(dt_pad)

dt_pad$sd_local_density_km2
dt_pad$sd_local_density_km2


dt_pad %>% ggplot() + 
  geom_point(aes(x = mean_density_km2_scaled, y = tree_cover_1000m_coef))


dt_cor <- dt_pad %>% 
  
  dplyr::select(
    `Woody cover trend` = tree_cover_1000m_coef,
    `Canopy height Trend` = canopy_height_900m_coef,
    `Mean elephant density` = mean_density_km2,
    `Elevation` = elevation,
    `MAT` = mat,
    `MAP`  = map,
    `N deposition` = n_deposition,
    `Fire frequency` = fire_frequency,
    `Months of extreme drought` = months_extreme_drought,
    `Temperature Trend`= mat_coef,
    `Precipitation Trend` = prec_coef,
    `SD local density` = sd_local_density_km2,
    `Local elephant density `= local_density_km2,
    `Sample size` = n,
    `CV local density`= cv_local_density_km2,
    `Park area` = area_km2
  ) %>% 
  filter(complete.cases(.))

corr <- round(cor(dt_cor), 2)

ggcorrplot(corr, type = "lower", lab = TRUE)

#nothing seems to correlate with mean elephant density too badly...   

#### Models -------
#WOody coer 
m_pad_tc = gam(tree_cover_1000m_coef ~ s(mean_density_km2_scaled, k = 3), 
               select=TRUE,
               data = dt_pad,
               method = "REML")
summary(m_pad_tc)

m_pad_tc_sp = gam(tree_cover_1000m_coef ~ s(mean_density_km2_scaled, k = 3) +
                    s(y_moll_scaled),  
                  select=TRUE,
                  data = dt_pad,
                  method = "REML")
summary(m_pad_tc_sp)

m_pad_tc_sp_lin = gam(tree_cover_1000m_coef ~ mean_density_km2_scaled +
                    s(y_moll_scaled),  
                  select=TRUE,
                  data = dt_pad,
                  method = "REML")
summary(m_pad_tc_sp_lin)

AIC(m_pad_tc_sp) - AIC(m_pad_tc_sp_lin)
# Canopy height
m_pad_ch = gam(canopy_height_900m_coef ~ s(mean_density_km2_scaled, k = 3), 
               select=TRUE,
               data = dt_pad,
               method = "REML")
summary(m_pad_ch)

m_pad_ch_sp = gam(canopy_height_900m_coef ~ s(mean_density_km2_scaled, k = 3) +
                    s(y_moll_scaled),  
                  select=TRUE,
                  data = dt_pad,
                  method = "REML")
summary(m_pad_ch_sp)

m_pad_ch_sp_lin = gam(canopy_height_900m_coef ~ mean_density_km2_scaled +
                    s(y_moll_scaled),  
                  select=TRUE,
                  data = dt_pad,
                  method = "REML")
summary(m_pad_ch_sp_lin)

AIC(m_pad_ch_sp) - AIC(m_pad_ch_sp_lin)




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

plot(ggeffects::ggpredict(m_pad_tc, term = "mean_density_km2_scaled [all]"))

pred_tc = ggeffects::ggpredict(m_pad_tc_sp_lin, term = "mean_density_km2_scaled [all]")
plot(pred_tc)

(dt_pred_tc <- as.data.frame(pred_tc) %>%
    mutate(
      x_unscaled = round(x * sd_x + mean_x, 6), 
      var_name = "mean_density_km2_scaled", 
      response_name = "tree_cover_1000m_coef", 
      q975_unscaled = as.numeric(quantile(dt_pad$mean_density_km2, .975, na.rm = T)), 
      q025_unscaled = as.numeric(quantile(dt_pad$mean_density_km2, .025, na.rm = T)), 
      q975 = as.numeric(quantile(dt_pad$mean_density_km2_scaled, .975, na.rm = T)), 
      q025 = as.numeric(quantile(dt_pad$mean_density_km2_scaled, .025, na.rm = T))
    ))


pred_ch = ggeffects::ggpredict(m_pad_ch_sp_lin, term = "mean_density_km2_scaled [all]")
plot(pred_ch)


(dt_pred_ch <- as.data.frame(pred_ch) %>%
    mutate(
      x_unscaled = round(x * sd_x + mean_x, 6), 
      var_name = "mean_density_km2_scaled", 
      response_name = "canopy_height_900m_coef", 
      q975_unscaled = as.numeric(quantile(dt_pad$mean_density_km2, .975, na.rm = T)), 
      q025_unscaled = as.numeric(quantile(dt_pad$mean_density_km2, .025, na.rm = T)), 
      q975 = as.numeric(quantile(dt_pad$mean_density_km2_scaled, .975, na.rm = T)), 
      q025 = as.numeric(quantile(dt_pad$mean_density_km2_scaled, .025, na.rm = T))
    ))


dt_pred = rbind(dt_pred_tc, dt_pred_ch) %>% 
  mutate(response_clean = case_when(
    response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend",
    response_name == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
  ),
  var_clean = case_when(
    var_name == "mean_density_km2_scaled" ~ "Elephant Density"))
unique(dt_pred$response_name)
unique(dt_pred$var_name)

#fwrite(dt_pred, "builds/model_outputs/gam_park_average_density_predictions.csv")



dt_long <- dt_pad %>% pivot_longer(
  cols = c("mean_density_km2"), 
  names_to = "var_name", 
  values_to = "var_value") %>% 
  mutate(var_clean = case_when(
    var_name == "mean_density_km2" ~ "Elephant Density"
  )) %>% 
  pivot_longer(
    cols = c("canopy_height_900m_coef", "tree_cover_1000m_coef"), 
    names_to = "response_name", 
    values_to = "response_value") %>% 
  mutate(response_clean = case_when(
    response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend",
    response_name == "tree_cover_1000m_coef" ~ "Woody Cover Trend"
  ))

rects <- dt_long %>%
  group_by(var_clean) %>%
  mutate(
    lower_quantile_x = quantile(var_value, 0.025),
    upper_quantile_x = quantile(var_value, 0.975),
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

p_smooth_points <- dt_pred %>% 
  # filter(response_name %in% c("evi_900m_coef") & tier == "Simple") %>% 
  ggplot() +
  geom_point(data = dt_long, aes(x = var_value, y = response_value), alpha = 0.75, size = 1, color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey5") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high, fill = response_clean), alpha = 0.4) +
  geom_line(aes(x = x_unscaled, y = predicted, color = response_clean), linewidth = 1.1) +
  scale_color_manual(values = c("#0C4C00", "#262600")) +
  scale_fill_manual(values = c("#0C4C00", "#262600")) + 
  facet_wrap(~response_clean, scales = "free") +
  # labs(y = "Evi Trend", title = "Simple", x = "") +
  theme_bw() +
  labs(y = "Response Value", title = "", x = "Elephant Density") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth_points
ggsave(plot = p_smooth_points, "builds/plots/supplement/linear_park_average_density_predictions.png", 
       dpi = 900, height = 2.9, width = 6)
