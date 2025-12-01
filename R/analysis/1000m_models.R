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
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

#load data 
#sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100) #counts likely wrong - mean density of 6.3 elephants seems unrealistic



# get dataframe with comlete and clean data fro mdoeling 

quantile(dt$dw_min_median_mode_fraction, na.rm = T)
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
  )

n_distinct(dt_mod$park_id)
(park_counts <- dt_mod[, .N, by = park_id] %>% arrange(N))

#check model data 
glimpse(dt_mod)


dt_corr_pred <- dt_mod %>% 
  dplyr::select(mat, map, n_deposition, human_modification, 
                fire_frequency, months_extreme_drought,
                mat_coef, prec_coef, 
                mean_density_km2, local_density_km2)

corr <- round(cor(dt_corr_pred), 2)
ggcorrplot(corr, hc.order = FALSE, type = "lower",
           lab = TRUE) #prec and mat change don't go well together...



# 2. Test covariate model improvement -------------------------------------

responses <- c("tree_cover_1000m_coef", "canopy_height_900m_coef")

vars <- c("local_density_km2_scaled",
          "months_extreme_drought_scaled",
          "fire_frequency_scaled", 
          "mat_coef_scaled", 
          "prec_coef_scaled", 
          "n_deposition_scaled", 
          "mean_density_km2_scaled",
          "s(local_density_km2_scaled, k = 3)",
          "s(months_extreme_drought_scaled, k = 3)",
          "s(fire_frequency_scaled, k = 3)", 
          "s(mat_coef_scaled, k = 3)", 
          "s(prec_coef_scaled, k = 3)", 
          "s(n_deposition_scaled, k = 3)", 
          "s(mean_density_km2_scaled, k = 3)")

plan(multisession, workers = 6)
#options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
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
                             library(future)
                             library(furrr)
                             
                             
                             dt_var_res_sub <- data.frame()
                             
                             for (var in unique(vars)) {
                               
                               int_formula <- as.formula(paste0(resp, " ~ 1 + (1 | park_id)"))
                               formula <- as.formula(paste0(resp, " ~ ", var, " + (1 | park_id)"))
                               
                               
                               
                               fit_int <- sdmTMB::sdmTMB(int_formula,
                                                      data = dt_mod,
                                                      spatial = "off"
                               )
                               
                               fit <- sdmTMB::sdmTMB(formula,
                                                     data = dt_mod,
                                                     spatial = "off"
                               )
                               
                               
                               san <- sdmTMB::sanity(fit)
                               
                               aic_fit = AIC(fit); aic_fit_int = AIC(fit_int)
                               
                               delta_aic = aic_fit_int - aic_fit #positive values indicate improvement
                               
                               dt_tmp = data.frame(
                                 resp = resp,
                                 var = var, 
                                 aic_fit = aic_fit, 
                                 aic_fit_int = aic_fit_int, 
                                 delta_aic = delta_aic
                               )
                               
                               
                               dt_var_res_sub <- rbind(dt_tmp, dt_var_res_sub)
                               
                               print(paste0(var, " done"))
                               rm(fit_int)
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
    resp == "canopy_height_900m_coef" ~  "Canopy Height Trend"
  ), 
  clean_term = case_when(
    .default = var,
    var == "local_density_km2_scaled" ~ "Local Elephant Density",
    var == "mean_density_km2_scaled" ~ "Mean Elephant Density",
    var == "density_trend_estimate_scaled" ~ "Elephant Density Trend",
    var == "mat_coef_scaled" ~ "MAT Trend",
    var == "prec_coef_scaled" ~ "Precipitation Trend",
    var == "n_deposition_scaled" ~ "Nitrogen deposition",
    var == "fire_frequency_scaled" ~ "Fire frequency",
    var == "months_extreme_drought_scaled" ~ "N Drought Months", 
    var == "mat_scaled" ~ "MAT", 
    var == "map_scaled" ~ "MAP",
    var == "s(local_density_km2_scaled, k = 3)" ~ "Local Elephant Density Smoothed",
    var == "s(mean_density_km2_scaled, k = 3)" ~ "Mean Elephant Density Smoothed",
    var == "s(density_trend_estimate_scaled, k = 3)" ~ "Elephant Density Trend Smoothed",
    var == "s(mat_coef_scaled, k = 3)" ~ "MAT Trend Smoothed",
    var == "s(prec_coef_scaled, k = 3)" ~ "Precipitation Trend Smoothed",
    var == "s(n_deposition_scaled, k = 3)" ~ "Nitrogen deposition Smoothed",
    var == "s(fire_frequency_scaled, k = 3)" ~ "Fire frequency Smoothed",
    var == "s(months_extreme_drought_scaled, k = 3)" ~ "N Drought Months Smoothed", 
    var == "s(mat_scaled, k = 3)" ~ "MAT Smoothed", 
    var == "s(map_scaled, k = 3)" ~ "MAP Smoothed"), 
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
ggsave(plot = p_aic, "builds/plots/supplement/aic_univariate_models_1000m_local_density_smoothed.png", dpi = 600, height = 5, width = 7)

### 3 - Choose Mesh ------------------
#https://www.biorxiv.org/content/10.1101/2022.03.24.485545v4.full.pdf

#https://becarioprecario.bitbucket.io/spde-gitbook/ch-intro.html on how to construct meshs , chapter 2.6 and 2.7

#the following may take a good couple of hours to finish 
responses <- c("tree_cover_1000m_coef", "canopy_height_900m_coef")

mesh_grid <- expand.grid(max_inner_edge = seq(50, 150, by = 50),
                         cutoff = c(2, 4, 8, 16, 32), 
                         response = responses) %>% 
  mutate(mesh_id = paste0("mesh_", 1:nrow(.)))

plan(multisession, workers = 30)
#options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
start_time <- Sys.time()


list_mesh_res_sub <- future_map(
  1:nrow(mesh_grid),
  .progress = TRUE,
  .options = furrr_options(seed = TRUE),
  function(i) {
    
    co <- as.numeric(mesh_grid[i, ]$cutoff)
    i_e <- as.numeric(mesh_grid[i, ]$max_inner_edge)
    mesh_id <- mesh_grid[i, ]$mesh_id
    resp <- mesh_grid[i, ]$response
    
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
    
    formula <- as.formula(paste0(resp, " ~ s(mean_density_km2, k = 3)"))
    
    fit_cv <- tryCatch({
      sdmTMB::sdmTMB_cv(
        formula,
        data = dt_mod,
        mesh = mesh,
        k_folds = 5,
        #  family = sdmTMB::student(),
        spatial = "on",
        #fold_ids = clust, 
        parallel = FALSE,
        reml = T
      )
    }, error = function(e) {
      message("Skipping CV model due to error: ", e$message)
      return(NULL)
    })
    
    if (is.null(fit_cv)) return(NULL)
    
    
    cv_model_id <- paste0("cv_", resp, "_", mesh_id, "_park_average_density")
    
    tmp_tidy <- data.frame(
      cutoff = co,
      max_inner_edge = i_e,
      mesh_id = mesh_id,
      response = resp,
      n_vertices = nrow(mesh$mesh$loc),
      all_converged = fit_cv$converged,
      p_d_hessian = sum(fit_cv$pdHess),
      response = resp,
      sum_loglik = fit_cv$sum_loglik,
      n = nrow(dt_mod),
      log_cpo_approx = fit_cv$sum_loglik / nrow(dt_mod),
      model_id = cv_model_id,
      model_path = paste0("builds/cv_models/", cv_model_id, ".Rds")
    )
    
    saveRDS(fit_cv, file = tmp_tidy$model_path)
    
    rm(fit_cv)
    gc()
    
    tmp_tidy
  }
)

plan(sequential)


dt_mesh_res <- rbindlist(list_mesh_res_sub)

print(paste0("Started loop at: ", start_time, " and finished at: ", Sys.time()))
print(paste0("Estimate time for CV: ", round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2), " mins"))

##### REMOVED PARK ID AND CHANGED FAMILY TO STUDENT
## bind results 
unique(responses)
dt_mesh_res_fin <- dt_mesh_res  %>% 
  mutate(clean_response = case_when(
    .default = response,
    response == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
    response == "canopy_height_900m_coef" ~  "Canopy Height Trend"))
summary(dt_mesh_res_fin)

fwrite(dt_mesh_res_fin, "builds/model_outputs/cv_mesh_selection_sdmtmb_results_1000m_local_density_smoothed.csv")

p_loglik <- dt_mesh_res_fin %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend"))) %>% 
  ggplot() +
  geom_point(aes(x = cutoff, y = sum_loglik, color = max_inner_edge), size = 2, alpha = 0.8) +
  scale_color_viridis_c(option = "B", direction = - 1, begin = 0.2, end = 0.8) +
  labs(y = "Log Likelihood (sum)", x = "Cutoff (km)", color = "Max\nInner\nEdge\n(km)", title = "Km² scale") +
  facet_wrap(~clean_response, scales = "free") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_loglik
ggsave(plot = p_loglik, "builds/plots/supplement/sum_loglik_different_meshs_1000m_local_density_smoothed.png", dpi = 600, height = 4, width = 8)

### 4 - Best Mesh ------------------

dt_mesh_res_fin <- fread("builds/model_outputs/cv_mesh_selection_sdmtmb_results_1000m_local_density_smoothed.csv")

dt_best_mesh <- dt_mesh_res_fin %>% 
  filter(all_converged == TRUE) %>% 
  group_by(response) %>% 
  slice_max(sum_loglik) %>% 
  ungroup()


plan(multisession, workers = 2)
#options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
start_time <- Sys.time()

best_mesh_res_list <- future_map(1:nrow(dt_best_mesh),
                            .progress = TRUE,
                            .options = furrr_options(seed = TRUE),
                            function(i) {

                                resp <- dt_best_mesh[i, ]$response
                                co <- as.numeric(dt_best_mesh[i, ]$cutoff)
                                i_e <- as.numeric(dt_best_mesh[i, ]$max_inner_edge)
                                mesh_id <- dt_best_mesh[i, ]$mesh_id
                                
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
                                
                                int_formula <- as.formula(paste0(resp, "~ 1"))
                                
                                re_formula <- as.formula(paste0(resp, "~ 1 + (1 | park_id)"))
                                
                                ele_formula <- as.formula(paste0(resp, " ~ 
                                s(local_density_km2_scaled, k = 3)"))
                                
                                fixed_formula <- as.formula(paste0(resp, " ~ 
                                s(local_density_km2_scaled, k = 3) +
                                s(months_extreme_drought_scaled, k = 3) +
                                s(fire_frequency_scaled, k = 3) +
                                s(mat_coef_scaled, k = 3) + 
                                s(n_deposition_scaled, k = 3)"))

                                full_formula <- as.formula(paste0(resp, " ~ 
                                s(local_density_km2_scaled, k = 3) +
                                s(months_extreme_drought_scaled, k = 3) +
                                s(fire_frequency_scaled, k = 3) +
                                s(mat_coef_scaled, k = 3) + 
                                s(n_deposition_scaled, k = 3)"))
                                
                                #https://github.com/pbs-assess/sdmTMB/issues/466#issuecomment-3119589818
                                
                                #intercept only 
                                fit_int <- sdmTMB(int_formula,
                                               spatial = "off",
                                               data = dt_mod,
                                               mesh = mesh, 
                                               reml = T)
                                

                                #intercept and spatial  
                                fit_sp <- sdmTMB(int_formula, 
                                               spatial = "on", 
                                               data = dt_mod,
                                               mesh = mesh, 
                                               reml = T)
                                
                                fit_ele <- sdmTMB(ele_formula, 
                                                 spatial = "off", 
                                                 data = dt_mod,
                                                 mesh = mesh, 
                                                 reml = T)
                                
                                #full model 
                                fit_full <- sdmTMB(full_formula, 
                                               spatial = "on", 
                                               data = dt_mod,
                                               mesh = mesh, 
                                               reml = T)
                                
                                #fixed effects only 
                                fit_fixed <- sdmTMB(fixed_formula, 
                                               spatial = "off", 
                                               data = dt_mod,
                                               mesh = mesh, 
                                               reml = T)
                                
                                # total proportion deviance explained by our full model:
                                (dev_explained_full <- 1 - deviance(fit_full) / deviance(fit_int))

                                # proportion deviance explained by elephants:
                                (dev_explained_ele <- 1 - deviance(fit_ele) / deviance(fit_int))
                                
                                # proportion deviance explained by the mesh:
                                (dev_explained_spatial <- 1 - deviance(fit_sp) / deviance(fit_int))

                                # proportion deviance explained by the covariate:
                                (dev_explained_var <- 1 - deviance(fit_fixed) / deviance(fit_int))
                                
                                
                                library(spdep)
                                
                                dt_mod_sf <- dt_mod %>% 
                                  st_as_sf(.,
                                           coords = c("x_mollweide", "y_mollweide"), 
                                           crs = "ESRI:54009") %>% 
                                  mutate(resids_full = residuals(fit_full), 
                                         resids_fixed = residuals(fit_fixed)) %>% 
                                  filter(!is.infinite(resids_full), !is.infinite(resids_fixed))
                                
                                coords <- st_coordinates(dt_mod_sf)
                              # knn <- knearneigh(coords, k = 250)
                              # nb_knn <- knn2nb(knn)
                                
                                nb_knn <- dnearneigh(coords, 0, 10000)

                                lw <- nb2listw(nb_knn, style = "W")
                                
                                mi_fixed <- moran.test(dt_mod_sf$resids_fixed, lw)
                                mi_fixed
                                
                                mi_full <- moran.test(dt_mod_sf$resids_full, lw)
                                mi_full
    
                                
                                san <- sdmTMB::sanity(fit_full)
                                
                                model_id = paste0(resp, "_best_mesh_1000m_local_density_smoothed")
                                
                                tmp_tidy <- broom::tidy(fit_full, conf.int = TRUE) %>%
                                  #dplyr::filter(!grepl("(Intercept)", term)) %>%
                                  dplyr::mutate(sig = case_when(
                                    .default = "non-significant",
                                    conf.low > 0 ~ "positive",
                                    conf.high < 0 ~ "negative"
                                  )) %>%
                                  dplyr::mutate(
                                    response = resp, 
                                    dev_explained_var = dev_explained_var, 
                                    dev_explained_full = dev_explained_full,
                                    dev_explained_ele = dev_explained_ele, 
                                    dev_explained_spatial = dev_explained_spatial,
                                    morans_i_fixed = as.numeric(mi_fixed$estimate[1]), 
                                    morans_i_full = as.numeric(mi_full$estimate[1]), 
                                    morans_i_p_val_fixed = as.numeric(mi_fixed$p.value), 
                                    morans_i_p_val_full = as.numeric(mi_full$p.value),
                                    cutoff = co,
                                    max_inner_edge = i_e,
                                    mesh_id = mesh_id,
                                    n_vertices = nrow(mesh$mesh$loc),
                                    aic = AIC(fit_full),
                                    n = nrow(dt_mod), 
                                    model_id = model_id, 
                                    model_path = paste0("builds/models/", model_id, ".Rds"),
                                    hessian_ok = san$hessian_ok, 
                                    eigen_values_ok = san$eigen_values_ok,
                                    nlminb_ok = san$nlminb_ok,
                                    range_ok = san$range_ok,
                                    gradients_ok = san$gradients_ok,
                                    se_magnitude_ok = san$se_magnitude_ok,
                                    se_na_ok = san$se_na_ok,
                                    sigmas_ok = san$sigmas_ok,
                                    all_ok = san$all_ok

                                  )
                                
                                saveRDS(fit_full, file = unique(tmp_tidy$model_path))
                                
                                print(paste0(i, " done"))
                                
                                rm(fit_int, fit_sp, fit_ele, fit_full, fit_fixed)
                                gc()
                                
                                return(tmp_tidy)
                                
                              })
plan(sequential)

print(paste0("Started loop at: ", start_time, " and finished at: ", Sys.time()))

## bind results 
dt_res <- rbindlist(best_mesh_res_list) %>% 
  mutate(clean_response = case_when(
    .default = response,
    response == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
    response == "canopy_height_900m_coef" ~  "Canopy Height Trend"
  ), 
  clean_term = case_when(
    .default = term,
    term == "local_density_km2_scaled" ~ "Local Elephant Density",
    term == "mean_density_km2_scaled" ~ "Mean Elephant Density",
    term == "density_trend_estimate_scaled" ~ "Elephant Density Trend",
    term == "mat_change_scaled" ~ "MAT Trend",
    term == "prec_change_scaled" ~ "Precipitation Trend",
    term == "n_deposition_scaled" ~ "Nitrogen deposition",
    term == "fire_frequency_scaled" ~ "Fire frequency",
    term == "months_extreme_drought_scaled" ~ "N Drought Months", 
    term == "s(local_density_km2_scaled, k = 3)" ~ "Local Elephant Density Smoothed",
    term == "s(mean_density_km2_scaled, k = 3)" ~ "Mean Elephant Density Smoothed",
    term == "s(density_trend_estimate_scaled, k = 3)" ~ "Elephant Density Trend Smoothed",
    term == "s(mat_change_scaled, k = 3)" ~ "MAT Trend Smoothed",
    term == "s(prec_change_scaled, k = 3)" ~ "Precipitation Trend Smoothed",
    term == "s(n_deposition_scaled, k = 3)" ~ "Nitrogen deposition Smoothed",
    term == "s(fire_frequency_scaled, k = 3)" ~ "Fire frequency Smoothed",
    term == "s(months_extreme_drought_scaled, k = 3)" ~ "N Drought Months Smoothed"))
unique(dt_res$clean_term)
summary(dt_res)
fwrite(dt_res, "builds/model_outputs/sdmtmb_results_1000m_local_density_smoothed.csv")
