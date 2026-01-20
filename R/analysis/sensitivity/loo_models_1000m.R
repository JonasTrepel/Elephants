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

#### Leave one site out CV


#1 HOUSEKEEPING -------------------------------------

#load data 
#sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100)

setDT(dt)


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
  mutate(park_id = factor(park_id)) #%>% 



dt_mesh_res_fin <- fread("builds/model_outputs/cv_mesh_selection_sdmtmb_results_1000m_local_density_smoothed.csv")

dt_best_mesh <- dt_mesh_res_fin %>% 
  group_by(response) %>% 
  slice_max(sum_loglik) %>% 
  ungroup()


  
responses = c("canopy_height_900m_coef", "tree_cover_1000m_coef")  
parks <- c("No Park", as.character(unique(dt_mod$park_id)))

model_guide <- CJ(response = responses, 
                  park = parks) %>% 
  left_join(dt_best_mesh)


### - Best Mesh ------------------

plan(multisession, workers = 35)
#options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
start_time <- Sys.time()

best_mesh_res_list <- future_map(1:nrow(model_guide),
                                 .progress = TRUE,
                                 .options = furrr_options(seed = TRUE),
                                 function(i) {
                                   
                                   park = model_guide[i,]$park
                                   
                                   dt_sub <- dt_mod %>% 
                                     filter(!park_id %in% c(park)) %>% 
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
                                   
                                   
                                   n_park = as.numeric(nrow(dt_mod) - nrow(dt_sub))
                                   
                                   resp <- model_guide[i, ]$response
                                   co <- as.numeric(model_guide[i, ]$cutoff)
                                   i_e <- as.numeric(model_guide[i, ]$max_inner_edge)

                                   inla_mesh <- fmesher::fm_mesh_2d_inla(
                                     loc = cbind(dt_sub$x_moll_km, dt_sub$y_moll_km),
                                     cutoff = co,
                                     max.edge = c(i_e, 10000)
                                   )
                                   
                                   mesh <- make_mesh(
                                     data = dt_sub,
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
                                                     data = dt_sub,
                                                     mesh = mesh, 
                                                     reml = T)
                                   
                                   
                                   #intercept and spatial  
                                   fit_sp <- sdmTMB(int_formula, 
                                                    spatial = "on", 
                                                    data = dt_sub,
                                                    mesh = mesh, 
                                                    reml = T)
                                   
                                   fit_ele <- sdmTMB(ele_formula, 
                                                     spatial = "off", 
                                                     data = dt_sub,
                                                     mesh = mesh, 
                                                     reml = T)
                                   
                                   #full model 
                                   fit_full <- sdmTMB(full_formula, 
                                                      spatial = "on", 
                                                      data = dt_sub,
                                                      mesh = mesh, 
                                                      reml = T)
                                   
                                   #fixed effects only 
                                   fit_fixed <- sdmTMB(fixed_formula, 
                                                       spatial = "off", 
                                                       data = dt_sub,
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
                                   
                                   san <- sdmTMB::sanity(fit_full)
                                   
                                   park_ <- gsub(" ", "_", tolower(park))
                                   
                                   model_id = paste0("loo_",resp, "_no_",park_,"_1000m_local_density_smoothed")
                                   
                                   tmp_tidy <- broom::tidy(fit_full, conf.int = TRUE) %>%
                                     #dplyr::filter(!grepl("(Intercept)", term)) %>%
                                     dplyr::mutate(sig = case_when(
                                       .default = "non-significant",
                                       conf.low > 0 ~ "positive",
                                       conf.high < 0 ~ "negative"
                                     )) %>%
                                     dplyr::mutate(
                                       response = resp, 
                                       excluded_park = park,
                                       n_park = n_park, 
                                       dev_explained_var = dev_explained_var, 
                                       dev_explained_full = dev_explained_full,
                                       #  dev_explained_re_spatial = dev_explained_re_spatial, 
                                       dev_explained_spatial = dev_explained_spatial,
                                       # dev_explained_re = dev_explained_re,
                                       cutoff = co,
                                       max_inner_edge = i_e,
                                       n_vertices = nrow(mesh$mesh$loc),
                                       aic = AIC(fit_full),
                                       n = nrow(dt_sub), 
                                       model_id = model_id, 
                                       model_path = paste0("builds/loo_models/", model_id, ".Rds"),
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
                                   
                                   rm(fit0, fit1, fit2, fit3, fit4, fit_full)
                                   gc()
                                   
                                   return(tmp_tidy)
                                   
                                 })
plan(sequential)

print(paste0("Started loop at: ", start_time, " and finished at: ", Sys.time()))

## bind results 
unique(responses)
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
fwrite(dt_res, "builds/model_outputs/loo_results_1000m_local_density_smoothed.csv")
