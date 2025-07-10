library(tidyverse)
library(data.table)
library(ggrepel)
library(ggridges)
library(mgcv)
library(tidylog)
library(ggcorrplot)
library(performance)
library(sjPlot)
library("gam.hp")
library(remotePARTS)
library(ctmm)
library(sf)
library(dsm)
library(DHARMa)
library(broom)

#1 HOUSEKEEPING -------------------------------------

#load data 
sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg")

dt <- fread("data/processed_data/clean_data/final_point_data.csv") %>% 
  mutate(grass_cover_coef = grass_cover_coef*100, 
         gr_n_cr_cover_coef = gr_n_cr_cover_coef*100, 
         tree_cover_coef = tree_cover_coef*100, 
         shrub_cover_coef = shrub_cover_coef*100, 
         bare_cover_coef = bare_cover_coef*100, 
         mean_evi_coef = mean_evi_coef/100,
         park_id = as.factor(park_id)
  ) %>% 
  filter(!park_id %in% c("Zambezi"))

# get dataframe with comlete and clean data fro mdoeling 
dt_mod <- dt %>% 
  filter(dw_min_mode_fraction >= 50) %>% 
  select(
    #mean values /habitat characteristics 
    mean_grass_cover, mean_gr_n_cr_cover, mean_tree_cover, mean_shrub_cover, mean_evi, 
    mean_habitat_diversity_100m, mean_habitat_diversity_1000m, 
    
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, 
    
    #Trends - Responses 
    grass_cover_coef, gr_n_cr_cover_coef, tree_cover_coef, shrub_cover_coef, bare_cover_coef, 
    habitat_diversity_100m_coef, habitat_diversity_1000m_coef, mean_evi_coef, 
    
    #Trends  - Predictors 
    mat_coef, prec_coef, burned_area_coef,
    
    #Elephant variables 
    mean_density_km2, local_density_km2, density_km2_estimate, population_trend_estimate, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, 
    
    #identifieres 
    park_id
    ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    local_density_km2_scaled = as.numeric(scale(log(local_density_km2 +0.0001))),
    density_km2_estimate_scaled = as.numeric(scale(density_km2_estimate)),
    mat_coef_scaled = as.numeric(scale(mat_coef)),
    prec_coef_scaled = as.numeric(scale(prec_coef)),
    n_deposition_scaled = as.numeric(scale(n_deposition)),
    fire_frequency_scaled = as.numeric(scale(fire_frequency)),
    burned_area_coef_scaled = as.numeric(scale(burned_area_coef)), 
    x_mollweide_scaled = as.numeric(scale(x_mollweide)), 
    y_mollweide_scaled = as.numeric(scale(y_mollweide))
    
  )

hist(dt_mod$local_density_km2_scaled)
range(dt_mod$mean_density_km2)


#check model data 
table(dt_mod$park_id)
n_distinct(dt_mod$park_id)
glimpse(dt_mod)
dt_mod %>% ggplot() +
  geom_sf(data = sf_parks) +
  geom_point(aes(x = x_mollweide, y = y_mollweide, color = park_id), size = 0.2) + 
  geom_sf(data = sf_parks, fill = "transparent") +
  theme_void() +
  theme(legend.position = "none")
  

#Check correlations 
dt_corr <- dt_mod %>% 
  select(-c(x_mollweide, y_mollweide, lon, lat, park_id), -contains("scaled"))

corr <- round(cor(dt_corr), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

hist(dt_mod$habitat_diversity_1000m_coef)

dt_corr2 <- dt_mod %>% 
  select(local_density_km2_scaled, density_km2_estimate_scaled, 
         mat_coef_scaled, prec_coef_scaled, n_deposition_scaled,
         fire_frequency_scaled, burned_area_coef_scaled) %>% 
  filter(complete.cases(.))

ggcorrplot(round(cor(dt_corr2), 2), hc.order = TRUE, type = "lower",
           lab = TRUE)

# get spatial data 
sf_mod<- st_as_sf(dt_mod, coords = c("x_mollweide", "y_mollweide"), crs = "ESRI:54009")

### try first remove the effect of covariates of interest from coords and then use the resids in a model 
x_y_resids <- gam(list(
  as.formula(paste0('x_mollweide ', "~ 
                      local_density_km2_scaled +
                      density_km2_estimate_scaled +
                      mat_coef_scaled +
                      prec_coef_scaled+ 
                      n_deposition_scaled +
                      fire_frequency_scaled +
                      burned_area_coef_scaled +
                      s(park_id, bs = 're')")),
  as.formula(paste0('y_mollweide ', "~ 
                      local_density_km2_scaled +
                      density_km2_estimate_scaled +
                      mat_coef_scaled +
                      prec_coef_scaled+ 
                      n_deposition_scaled +
                      fire_frequency_scaled +
                      burned_area_coef_scaled +
                      s(park_id, bs = 're')"))),
  data = dt_mod,
  family = mvn(d = 2),
  method = 'REML') %>%
  residuals(type = 'deviance')


dt_mod$x_resid <- x_y_resids[,1]
dt_mod$y_resid <- x_y_resids[,2]
dt_mod$x_resid_scaled <- scale(as.numeric(dt_mod$x_resid))
dt_mod$y_resid_scaled <- scale(as.numeric(dt_mod$y_resid))


#2 MODELING -------------------------------------

##### EVI 


# Base model 
m_evi <- bam(mean_evi_coef ~ 
           local_density_km2_scaled +
           density_km2_estimate_scaled +
           mat_coef_scaled +
           prec_coef_scaled+ 
           n_deposition_scaled +
           fire_frequency_scaled +
           burned_area_coef_scaled+
           s(park_id, bs = "re"),
           family= scat(), #to account for heavy tails https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/scat.html
           data = dt_mod, 
            method = "REML")
summary(m_evi)
AIC(m_evi)
plot_model(m_evi, type = "pred")
par(mfrow = c(2, 2)); gam.check(m_evi); par(mfrow = c(1, 1))
vis_concurvity(m_evi)
check_collinearity(m_evi)
# hp <- gam.hp(m)
# plot(hp)

#Check spatial autocorrelation 
sf_mod$resid_evi <- residuals(m_evi)

library(gstat)


p_var_evi <- gstat::variogram(resid_evi ~ 1, data = sf_mod)
plot(p_var_evi)

p_resid_evi <- ggplot(sf_mod, aes(color = resid_evi)) +
  geom_sf(size = 0.2) +
  scale_color_gradient2() +
  labs(title = "Base Model") +
  theme_minimal()
p_resid_evi

m_evi_tidy <- tidy(m_evi, parametric = TRUE) %>% 
  mutate(ci_lb = estimate - 1.96*std.error, 
         ci_ub = estimate + 1.96*std.error, 
         sig = case_when(
           .default = "non-significant", 
           ci_lb > 0 ~ "positive", 
           ci_ub < 0 ~ "negative"
         ))

p_est_evi <- m_evi_tidy %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(x = estimate, y = term, xmin = ci_lb, xmax = ci_ub, color = sig), linewidth = 1.1) +
  theme_classic() +
  labs(title = "Base Model") +
  theme(legend.position = "none")
p_est_evi

#### Spatial smooth 
m_evi_ss <- bam(mean_evi_coef ~ 
               local_density_km2_scaled +
               density_km2_estimate_scaled +
               mat_coef_scaled +
               prec_coef_scaled+ 
               n_deposition_scaled +
               fire_frequency_scaled +
               burned_area_coef_scaled +
              s(x_mollweide_scaled, y_mollweide_scaled, k = 100) +
               s(park_id, bs = "re"),
             family= scat(), #to account for heavy tails https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/scat.html
             data = dt_mod, 
             method = "REML")
summary(m_evi_ss)
AIC(m_evi_ss)
#plot_model(m_evi_ss, type = "pred")
par(mfrow = c(2, 2)); gam.check(m_evi_ss); par(mfrow = c(1, 1))
vis_concurvity(m_evi_ss)
check_collinearity(m_evi_ss)
# hp <- gam.hp(m)
# plot(hp)

#Check spatial autocorrelation 
sf_mod$resid_evi_ss <- residuals(m_evi_ss)

library(gstat)

p_var_evi_ss <- gstat::variogram(resid_evi_ss ~ 1, data = sf_mod)
plot(p_var_evi_ss)

p_resid_evi_ss <- ggplot(sf_mod, aes(color = resid_evi_ss)) +
  geom_sf(size = 0.2) +
  scale_color_gradient2() +
  labs(title = "Spatial Smoother") +
  theme_minimal()
p_resid_evi_ss

m_evi_ss_tidy <- tidy(m_evi_ss, parametric = TRUE) %>% 
  mutate(ci_lb = estimate - 1.96*std.error, 
         ci_ub = estimate + 1.96*std.error, 
         sig = case_when(
           .default = "non-significant", 
           ci_lb > 0 ~ "positive", 
           ci_ub < 0 ~ "negative"
         ))

p_est_evi_ss <- m_evi_ss_tidy %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(x = estimate, y = term, xmin = ci_lb, xmax = ci_ub, color = sig), 
                  linewidth = 1.1) +
  theme_classic() +
  labs(title = "Spatial Smoother") +
  theme(legend.position = "none")
p_est_evi_ss



p_est <- p_est_evi | p_est_evi_ss
print(p_est)

p_res <- p_resid_evi | p_resid_evi_ss
print(p_res)

plot(p_var_evi)
plot(p_var_evi_ss)


###### residual autocorrelation 
m_evi_rsa  <- bam(mean_evi_coef ~ 
                  local_density_km2_scaled +
                  density_km2_estimate_scaled +
                  mat_coef_scaled +
                  prec_coef_scaled+ 
                  n_deposition_scaled +
                  fire_frequency_scaled +
                  burned_area_coef_scaled +
                  s(x_resid_scaled, y_resid_scaled, k = 100) +
                  s(park_id, bs = "re"),
                family= scat(), #to account for heavy tails https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/scat.html
                data = dt_mod, 
                method = "REML")
summary(m_evi_rsa )
AIC(m_evi_rsa )
plot_model(m_evi_rsa , type = "pred")
par(mfrow = c(2, 2)); gam.check(m_evi_rsa ); par(mfrow = c(1, 1))
vis_concurvity(m_evi_rsa )
check_collinearity(m_evi_rsa )
# hp <- gam.hp(m)
# plot(hp)

#Check spatial autocorrelation 
sf_mod$resid_evi_rsa  <- residuals(m_evi_rsa )

library(gstat)

p_var_evi_rsa  <- gstat::variogram(resid_evi_rsa  ~ 1, data = sf_mod)
plot(p_var_evi_rsa )

p_resid_evi_rsa  <- ggplot(sf_mod, aes(color = resid_evi_rsa)) +
  geom_sf(size = 0.2) +
  scale_color_gradient2() +
  labs(title = "Residual spatial autocorrelation") +
  theme_minimal()
p_resid_evi_rsa 

m_evi_rsa_tidy <- tidy(m_evi_rsa , parametric = TRUE) %>% 
  mutate(ci_lb = estimate - 1.96*std.error, 
         ci_ub = estimate + 1.96*std.error, 
         sig = case_when(
           .default = "non-significant", 
           ci_lb > 0 ~ "positive", 
           ci_ub < 0 ~ "negative"
         ))

p_est_evi_rsa  <- m_evi_rsa_tidy %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(x = estimate, y = term, xmin = ci_lb, xmax = ci_ub, color = sig), linewidth = 1.1) +
  theme_classic() +
  labs(title = "Residual spatial autocorrelation") +
  theme(legend.position = "none")
p_est_evi_rsa 


p_est <- p_est_evi | p_est_evi_ss | p_est_evi_rsa
print(p_est)

p_res <- p_resid_evi | p_resid_evi_ss | p_resid_evi_rsa
print(p_res)

plot(p_var_evi)
plot(p_var_evi_ss)
plot(p_var_evi_rsa)
