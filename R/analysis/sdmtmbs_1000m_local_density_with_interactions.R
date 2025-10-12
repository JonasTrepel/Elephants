library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(performance)
library(sjPlot)
library(sf)
library(DHARMa)
library(broom)
library("sdmTMB")
library(sdmTMBextra)
library(future)
library(furrr)
library(groupdata2)
library(rnaturalearth)
library(GGally)
library(glmmTMB)
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

#load data 
sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

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

n_distinct(dt_mod$park_id)
park_counts <- dt_mod[, .N, by = park_id] %>% arrange(N)
print(park_counts)



#check model data 
table(dt_mod$park_id)
n_distinct(dt_mod$park_id)
glimpse(dt_mod)




###### get the responses together
dt_corr_res <- dt_mod %>% 
  group_by(park_id) %>% 
  slice_sample(prop = .25) %>% 
  ungroup() %>% 
  dplyr::select(tree_cover_1000m_coef, evi_900m_coef, canopy_height_900m_coef,
                habitat_diversity_1000m_coef, tree_cover_sd_1000m_coef,
                evi_sd_900m_coef, canopy_height_sd_900m_coef)

ggpairs(dt_corr_res) # looks like habitat diversity and tree cover SD are kinda redundant

dt_corr_pred <- dt_mod %>% 
  dplyr::select(mat, map, distance_to_water_km, n_deposition, human_modification, 
                fire_frequency, months_severe_drought, months_extreme_drought, mat_change, prec_change, 
                mean_density_km2, local_density_km2, density_trend_estimate)

corr <- round(cor(dt_corr_pred), 2)
ggcorrplot(corr, hc.order = FALSE, type = "lower",
           lab = TRUE) #prec and mat change don't go well together...



#### Decide whether to include prec_change*map or mat_change*mat...
# Test covariate model improvement -------------------------------------

responses <- c("tree_cover_1000m_coef", "evi_900m_coef", "canopy_height_900m_coef",
               "tree_cover_sd_1000m_coef", "evi_sd_900m_coef", "canopy_height_sd_900m_coef")

vars <- c("local_density_km2_scaled",
          "density_trend_estimate_scaled",
          "months_severe_drought_scaled",
          "fire_frequency_scaled", 
          "mat_change_scaled", 
          "prec_change_scaled", 
          "n_deposition_scaled", 
          "density_trend_estimate_scaled*local_density_km2_scaled",
          "months_severe_drought_scaled*local_density_km2_scaled",
          "fire_frequency_scaled*local_density_km2_scaled", 
          "mat_change_scaled*local_density_km2_scaled", 
          "prec_change_scaled*local_density_km2_scaled", 
          "n_deposition_scaled*local_density_km2_scaled")

plan(multisession, workers = 6)
options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
start_time <- Sys.time()

var_res_list <- future_map(unique(responses),
                           .progress = TRUE,
                           .options = furrr_options(seed = TRUE),
                           function(resp) {
                             
                             library(tidyverse)
                             library(data.table)
                             library(sf)
                             library(DHARMa)
                             library(broom)
                             library("sdmTMB")
                             library(sdmTMBextra)
                             library(future)
                             library(furrr)
                             
                             
                             dt_var_res_sub <- data.frame()
                             
                             for (var in unique(vars)) {
                               
                               int_formula <- as.formula(paste0(resp, " ~ 1 + (1 | park_id)"))
                               formula <- as.formula(paste0(resp, " ~ ", var, " + (1 | park_id)"))
                               
                               
                               
                               fit0 <- sdmTMB::sdmTMB(int_formula,
                                                      data = dt_mod,
                                                      spatial = "off",
                                                      family = sdmTMB::student()
                               )
                               
                               fit <- sdmTMB::sdmTMB(formula,
                                                     data = dt_mod,
                                                     spatial = "off",
                                                     family = sdmTMB::student()
                               )
                               
                               
                               san <- sdmTMB::sanity(fit)
                               
                               aic_fit = AIC(fit); aic_fit0 = AIC(fit0)
                               
                               delta_aic = aic_fit0 - aic_fit #positive values indicate improvement
                               
                               dt_tmp = data.frame(
                                 resp = resp,
                                 var = var, 
                                 aic_fit = aic_fit, 
                                 aic_fit0 = aic_fit0, 
                                 delta_aic = delta_aic
                               )
                               
                               
                               dt_var_res_sub <- rbind(dt_tmp, dt_var_res_sub)
                               
                               print(paste0(var, " done"))
                               rm(fit0)
                               rm(fit)
                               gc()
                             }
                             
                             return(dt_var_res_sub)
                           }
)
plan(sequential)

print(paste0("Started loop at: ", start_time, " and finished at: ", Sys.time()))

## bind results 
unique(responses)
dt_var_res <- rbindlist(var_res_list) %>% 
  mutate(clean_response = case_when(
    .default = resp,
    resp == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
    resp == "evi_900m_coef" ~  "EVI Trend",
    resp == "canopy_height_900m_coef" ~  "Canopy Height Trend",
    resp == "tree_cover_sd_1000m_coef" ~ "Tree Cover SD Trend",
    resp == "evi_sd_900m_coef" ~ "EVI SD Trend", 
    resp == "canopy_height_sd_900m_coef" ~ "Canopy Height SD Trend"
  ), 
  clean_term = case_when(
    .default = var,
    var == "local_density_km2_scaled" ~ "Local Elephant Density",
    var == "mean_density_km2_scaled" ~ "Mean Elephant Density",
    var == "density_trend_estimate_scaled" ~ "Elephant Population Growth",
    var == "mat_change_scaled" ~ "MAT Trend",
    var == "prec_change_scaled" ~ "Precipitation Trend",
    var == "n_deposition_scaled" ~ "Nitrogen deposition",
    var == "fire_frequency_scaled" ~ "Fire frequency",
    var == "months_severe_drought_scaled" ~ "N Drought Months", 
    var == "density_trend_estimate_scaled*local_density_km2_scaled" ~ "Elephant Population Growth:Local Elephant Density",
    var == "mat_change_scaled*local_density_km2_scaled" ~ "MAT Trend:Local Elephant Density",
    var == "prec_change_scaled*local_density_km2_scaled" ~ "Precipitation Trend:Local Elephant Density",
    var == "n_deposition_scaled*local_density_km2_scaled" ~ "Nitrogen deposition:Local Elephant Density",
    var == "fire_frequency_scaled*local_density_km2_scaled" ~ "Fire frequency:Local Elephant Density",
    var == "months_severe_drought_scaled*local_density_km2_scaled" ~ "N Drought Months:Local Elephant Density", 
    var == "density_trend_estimate_scaled*mean_density_km2_scaled" ~ "Elephant Population Growth:Mean Elephant Density",
    var == "mat_change_scaled*mean_density_km2_scaled" ~ "MAT Trend:Mean Elephant Density",
    var == "prec_change_scaled*mean_density_km2_scaled" ~ "Precipitation Trend:Mean Elephant Density",
    var == "n_deposition_scaled*mean_density_km2_scaled" ~ "Nitrogen deposition:Mean Elephant Density",
    var == "fire_frequency_scaled*mean_density_km2_scaled" ~ "Fire frequency:Mean Elephant Density",
    var == "months_severe_drought_scaled*mean_density_km2_scaled" ~ "N Drought Months:Mean Elephant Density"), 
  point_col = case_when(
    abs(delta_aic) <= 2 ~ "no difference",
    delta_aic > 2 ~ "improved model", 
    delta_aic < 2 ~ "worsened model"
  ))



#plot 

p_aic <- dt_var_res %>% 
  ggplot() +
  geom_point(aes(x = delta_aic, y = clean_term, color = point_col)) +
  facet_wrap(~clean_response, scales = "free_x") +
  scale_color_manual(values = c("no difference" = "grey75", 
                                "improved model" = "#5F903D", 
                                "worsened model" = "#B5549C")) +
  facet_wrap(~clean_response, scales = "free_x", ncol = 3) +
  labs(y = "", x = "Delta AIC", color = "") +
  theme(legend.position = "bottom", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_aic
ggsave(plot = p_aic, "builds/plots/supplement/aic_univariate_models_1000m_local_density.png", dpi = 600, height = 6, width = 9)
# 

### 2 - Choose Mesh ------------------
#https://www.biorxiv.org/content/10.1101/2022.03.24.485545v4.full.pdf

#https://becarioprecario.bitbucket.io/spde-gitbook/ch-intro.html on how to construct meshs , chapter 2.6 and 2.7

#the following may take a good couple of hours to finish 
mesh_grid <- expand.grid(max_inner_edge = seq(50, 150, by = 50), cutoff = seq(2, 20, by = 2), loc_cpo = NA) %>% 
  mutate(mesh_id = paste0("mesh_", 1:nrow(.)))

responses <- c("tree_cover_1000m_coef", "evi_900m_coef", "canopy_height_900m_coef",
               "tree_cover_sd_1000m_coef", "evi_sd_900m_coef", "canopy_height_sd_900m_coef")

plan(multisession, workers = 6)
options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
start_time <- Sys.time()

mesh_res_list <- future_map(unique(responses),
                            .progress = TRUE,
                            .options = furrr_options(seed = TRUE),
                            function(resp) {
                              
                              library(tidyverse)
                              library(data.table)
                              library(sf)
                              library(DHARMa)
                              library(broom)
                              library("sdmTMB")
                              library(sdmTMBextra)
                              library(future)
                              library(furrr)
                              
                              dt_mesh_res_sub <- data.frame()
                              
                              
                              for (i in 1:nrow(mesh_grid)) {
                                
                                co <- mesh_grid[i, ]$cutoff
                                i_e <- mesh_grid[i, ]$max_inner_edge
                                mesh_id <- mesh_grid[i, ]$mesh_id
                                
                                inla_mesh <- fmesher::fm_mesh_2d_inla(
                                  loc = cbind(dt_mod$x_moll_km, dt_mod$y_moll_km),
                                  cutoff = co,
                                  max.edge = c(i_e, 10000)
                                )
                                
                                mesh <- make_mesh(
                                  data = dt_mod,
                                  xy_cols = c("x_moll_km", "y_moll_km"),
                                  mesh = inla_mesh
                                )
                                
                                nrow(mesh$mesh$loc)
                                
                                #plot(mesh)
                                
                                formula <- as.formula(paste0(resp, " ~ density_trend_estimate_scaled*local_density_km2_scaled +
          months_severe_drought_scaled*local_density_km2_scaled +,
          fire_frequency_scaled*local_density_km2_scaled + 
          mat_change_scaled*local_density_km2_scaled + 
          prec_change_scaled*local_density_km2_scaled + 
          n_deposition_scaled*local_density_km2_scaled"))
                                
                                
                                fit_cv <- sdmTMB::sdmTMB_cv(formula,
                                                            data = dt_mod,
                                                            mesh = mesh,
                                                            k_folds = 5,
                                                            spatial = "on",
                                                            fold_ids = "fold_id", # created above, folds are stratified to ensure each park is present in each fold 
                                                            family = sdmTMB::student()
                                )
                                
                                fit <- sdmTMB::sdmTMB(formula,
                                                      data = dt_mod,
                                                      mesh = mesh,
                                                      spatial = "on",
                                                      family = sdmTMB::student()
                                )
                                
                                
                                #gen_r2 <- performance::r2(fit)[1] #1-sum((y-y_hat)^2)/sum((y-y_bar)^2)
                                
                                san <- sdmTMB::sanity(fit)
                                
                                model_id = paste0(resp, "_", mesh_id, "_1000m_local_density_with_interactions")
                                
                                tmp_tidy <- broom::tidy(fit, conf.int = TRUE) %>%
                                  #dplyr::filter(!grepl("(Intercept)", term)) %>%
                                  dplyr::mutate(sig = case_when(
                                    .default = "non-significant",
                                    conf.low > 0 ~ "positive",
                                    conf.high < 0 ~ "negative"
                                  )) %>%
                                  dplyr::mutate(
                                    cutoff = co,
                                    max_inner_edge = i_e,
                                    mesh_id = mesh_id,
                                    n_vertices = nrow(mesh$mesh$loc),
                                    aic = AIC(fit),
                                    sanity_checks = all(san == TRUE),
                                    response = resp, 
                                    sum_loglik = fit_cv$sum_loglik,
                                    n = nrow(dt_mod), 
                                    log_cpo_approx = fit_cv$sum_loglik / nrow(dt_mod), 
                                    model_id = model_id, 
                                    model_path = paste0("builds/models/", model_id, ".Rds")
                                  )
                                
                                saveRDS(fit, file = paste0("builds/models/", model_id, ".Rds"))
                                
                                dt_mesh_res_sub <- rbind(tmp_tidy, dt_mesh_res_sub)
                                print(paste0(i, " done"))
                                rm(fit)
                                rm(fit_cv)
                                gc()
                              }
                              
                              return(dt_mesh_res_sub)
                            }
)
plan(sequential)

print(paste0("Started loop at: ", start_time, " and finished at: ", Sys.time()))

## bind results 
unique(responses)
dt_mesh_res <- rbindlist(mesh_res_list) %>% 
  mutate(clean_response = case_when(
    .default = response,
    response == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
    response == "evi_900m_coef" ~  "EVI Trend",
    response == "canopy_height_900m_coef" ~  "Canopy Height Trend",
    response == "tree_cover_sd_1000m_coef" ~ "Tree Cover SD Trend",
    response == "evi_sd_900m_coef" ~ "EVI SD Trend", 
    response == "canopy_height_sd_900m_coef" ~ "Canopy Height SD Trend"
  ), 
  clean_term = case_when(
    .default = term,
    var == "local_density_km2_scaled" ~ "Local Elephant Density",
    var == "mean_density_km2_scaled" ~ "Mean Elephant Density",
    var == "density_trend_estimate_scaled" ~ "Elephant Population Growth",
    var == "mat_change_scaled" ~ "MAT Trend",
    var == "prec_change_scaled" ~ "Precipitation Trend",
    var == "n_deposition_scaled" ~ "Nitrogen deposition",
    var == "fire_frequency_scaled" ~ "Fire frequency",
    var == "months_severe_drought_scaled" ~ "N Drought Months", 
    var == "density_trend_estimate_scaled:local_density_km2_scaled" ~ "Elephant Population Growth:Local Elephant Density",
    var == "mat_change_scaled:local_density_km2_scaled" ~ "MAT Trend:Local Elephant Density",
    var == "prec_change_scaled:local_density_km2_scaled" ~ "Precipitation Trend:Local Elephant Density",
    var == "n_deposition_scaled:local_density_km2_scaled" ~ "Nitrogen deposition:Local Elephant Density",
    var == "fire_frequency_scaled:local_density_km2_scaled" ~ "Fire frequency:Local Elephant Density",
    var == "months_severe_drought_scaled:local_density_km2_scaled" ~ "N Drought Months:Local Elephant Density", 
    var == "density_trend_estimate_scaled:mean_density_km2_scaled" ~ "Elephant Population Growth:Mean Elephant Density",
    var == "mat_change_scaled:mean_density_km2_scaled" ~ "MAT Trend:Mean Elephant Density",
    var == "prec_change_scaled:mean_density_km2_scaled" ~ "Precipitation Trend:Mean Elephant Density",
    var == "n_deposition_scaled:mean_density_km2_scaled" ~ "Nitrogen deposition:Mean Elephant Density",
    var == "fire_frequency_scaled:mean_density_km2_scaled" ~ "Fire frequency:Mean Elephant Density",
    var == "months_severe_drought_scaled:mean_density_km2_scaled" ~ "N Drought Months:Mean Elephant Density"))
unique(dt_mesh_res$clean_term)
summary(dt_mesh_res)
fwrite(dt_mesh_res, "builds/model_outputs/sdmtmb_results_1000m_local_density_with_interactions.csv")

p_covs <- dt_mesh_res %>% 
  filter(!grepl("Intercept", term)) %>% 
  ggplot() +
  geom_pointrange(aes(x = cutoff, y = estimate, ymin = conf.low, ymax = conf.high, color = sig)) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("non-significant" = "grey75", 
                                positive = "#5F903D", 
                                negative = "#B5549C")) +
  facet_grid(rows = vars(clean_response), cols = vars(clean_term), scales = "free") +
  theme_classic() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_covs
ggsave(plot = p_covs, "builds/plots/supplement/cov_estimates_different_meshs_1000m_local_density_with_interactions.png", dpi = 600, height = 12, width = 12)

p_est <- dt_mesh_res %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
    "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
  # filter(sanity_checks == TRUE) %>% 
  filter(!grepl("Intercept", term)) %>% 
  group_by(response, term) %>% 
  slice_max(log_cpo_approx) %>% 
  ungroup() %>% 
  ggplot() +
  geom_vline(xintercept = 0,linetype = "dashed") +
  geom_pointrange(aes(y = clean_term, x = estimate, xmin = conf.low, xmax = conf.high, color = sig), 
                  linewidth = 1.1, size = 1.1, alpha = 0.75, shape = 18) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("non-significant" = "grey75", 
                                positive = "#5F903D", 
                                negative = "#B5549C")) +
  facet_wrap(~clean_response, scales = "free_x", ncol = 3) +
  labs(y = "", title = "km² Scale") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_est
ggsave(plot = p_est, "builds/plots/cov_estimates_best_mesh_1000m_local_density_with_interactions.png.png", dpi = 600, height = 6.5, width = 10)

p_cpo <- dt_mesh_res %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
    "Tree Cover SD Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
  ggplot() +
  geom_point(aes(x = cutoff, y = log_cpo_approx, color = max_inner_edge)) +
  scale_color_viridis_c(option = "B") +
  labs(y = "Log CPO (Approx)", x = "Cutoff (km)", color = "Max\nInner\nEdge\n(km)", title = "Km² scale") +
  facet_wrap(~clean_response, scales = "free") +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_cpo
ggsave(plot = p_cpo, "builds/plots/supplement/log_cpo_approx_different_meshs_1000m_local_density_with_interactions.png.png", dpi = 600, height = 8, width = 8)

