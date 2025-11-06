library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(broom)
library("sdmTMB")
library(sdmTMBextra)
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
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100, 
         evi_900m_coef = evi_900m_coef/100
  ) %>% 
  filter(park_id != "Thornybush Nature Reserve")

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
    mean_habitat_diversity_1000m, mean_evi_sd_900m, mean_canopy_height_sd_900m, 
    
    #starting conditions
    tree_cover_1000m_2015_2016, evi_900m_2013_2014, canopy_height_900m_2000,
    habitat_diversity_1000m_2015_2016, evi_sd_900m_2013_2014, canopy_height_sd_900m_2000,
    
    # environmental predictors
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, months_severe_drought, months_extreme_drought, mat_change, prec_change,
    mat_coef, prec_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2,# density_trend_estimate, density_trend_estimate,
    
    #Trends - Responses 
    tree_cover_1000m_coef, evi_900m_coef, canopy_height_900m_coef, 
    habitat_diversity_1000m_coef, tree_cover_sd_1000m_coef, evi_sd_900m_coef, canopy_height_sd_900m_coef, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, country_code_iso3,
    
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
  mutate(fenced = ifelse(country_code_iso3 == "ZAF", "fenced", "unfenced"), 
         tree_cover_q = case_when(
           tree_cover_1000m_2015_2016 <= quantile(tree_cover_1000m_2015_2016, .33) ~  "lq", 
           tree_cover_1000m_2015_2016 > quantile(tree_cover_1000m_2015_2016, .33) & 
             tree_cover_1000m_2015_2016 < quantile(tree_cover_1000m_2015_2016, .66) ~  "mq", 
           tree_cover_1000m_2015_2016 >= quantile(tree_cover_1000m_2015_2016, .66) ~  "uq"),
         evi_q = case_when(
           evi_900m_2013_2014 <= quantile(evi_900m_2013_2014, .33) ~  "lq", 
           evi_900m_2013_2014 > quantile(evi_900m_2013_2014, .33) & 
             evi_900m_2013_2014 < quantile(evi_900m_2013_2014, .66) ~  "mq", 
           evi_900m_2013_2014 >= quantile(evi_900m_2013_2014, .66) ~  "uq"), 
         canopy_height_q = case_when(
           canopy_height_900m_2000 <= quantile(canopy_height_900m_2000, .33) ~  "lq", 
           canopy_height_900m_2000 > quantile(canopy_height_900m_2000, .33) & 
             canopy_height_900m_2000 < quantile(canopy_height_900m_2000, .66) ~  "mq", 
           canopy_height_900m_2000 >= quantile(canopy_height_900m_2000, .66) ~  "uq"))

table(dt_mod$fenced)
table(dt_mod$tree_cover_q)
table(dt_mod$evi_q)
table(dt_mod$canopy_height_q)

### 2 - Choose Mesh ------------------
#https://www.biorxiv.org/content/10.1101/2022.03.24.485545v4.full.pdf

#https://becarioprecario.bitbucket.io/spde-gitbook/ch-intro.html on how to construct meshs , chapter 2.6 and 2.7

#the following may take a good couple of hours to finish 
mesh_grid <- expand.grid(max_inner_edge = seq(50, 150, by = 50), cutoff = c(2, 4, 8, 16, 32), loc_cpo = NA) %>% 
  mutate(mesh_id = paste0("mesh_", 1:nrow(.)))


subsets <- c("lq", "mq", "uq", #lower, medium and upper quantile
        "fenced", "unfenced") 

responses <- c("tree_cover_1000m_coef", "evi_900m_coef", "canopy_height_900m_coef")

mesh_guide <- CJ(subset = subsets, 
                 response = responses) %>% 
  mutate(tier = paste0(response, "_", subset), 
         filter_call = case_when(
           tier == "canopy_height_900m_coef_lq" ~ "canopy_height_q == 'lq'",
           tier == "canopy_height_900m_coef_mq" ~ "canopy_height_q == 'mq'",
           tier == "canopy_height_900m_coef_uq" ~ "canopy_height_q == 'uq'",
           
           tier == "evi_900m_coef_lq" ~ "evi_q == 'lq'",
           tier == "evi_900m_coef_mq" ~ "evi_q == 'mq'",
           tier == "evi_900m_coef_uq" ~ "evi_q == 'uq'",
           
           tier == "tree_cover_1000m_coef_lq" ~ "tree_cover_q == 'lq'",
           tier == "tree_cover_1000m_coef_mq" ~ "tree_cover_q == 'mq'",
           tier == "tree_cover_1000m_coef_uq" ~ "tree_cover_q == 'uq'",
           
           tier == "canopy_height_900m_coef_fenced" ~ "fenced == 'fenced'",
           tier == "canopy_height_900m_coef_unfenced" ~ "fenced == 'unfenced'",
           
           tier == "evi_900m_coef_fenced" ~ "fenced == 'fenced'",
           tier == "evi_900m_coef_unfenced" ~ "fenced == 'unfenced'",
           
           tier == "tree_cover_1000m_coef_fenced" ~ "fenced == 'fenced'",
           tier == "tree_cover_1000m_coef_unfenced" ~ "fenced == 'unfenced'"))



plan(multisession, workers = 35)
#options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
start_time <- Sys.time()


mesh_res_list <- list()
dt_mesh_res <- data.frame()

for (j in 1:nrow(mesh_guide)) {
  
  (tier = mesh_guide[j, ]$tier)
  (resp = mesh_guide[j, ]$response)
  (filter_call = mesh_guide[j, ]$filter_call)

  print(paste0("Starting with response: ", tier, " at ", Sys.time()))

  
  dt_sub <- dt_mod %>% 
    dplyr::filter(eval(parse(text = filter_call))) %>% 
    mutate(
      local_density_km2_scaled = as.numeric(scale(local_density_km2)),
      mean_density_km2_scaled = as.numeric(scale(mean_density_km2)),
      months_extreme_drought_scaled = as.numeric(scale(months_extreme_drought)),
      fire_frequency_scaled = as.numeric(scale(fire_frequency)),
      mat_change_scaled = as.numeric(scale(mat_change)),
      prec_change_scaled = as.numeric(scale(prec_change)),
      mat_coef_scaled = as.numeric(scale(mat_coef)),
      prec_coef_scaled = as.numeric(scale(prec_coef)),
      n_deposition_scaled = as.numeric(scale(n_deposition)), 
      mat_scaled = as.numeric(scale(mat)), 
      map_scaled = as.numeric(scale(map))
    )
  
  
  
  list_mesh_res_sub <- future_map(
    1:nrow(mesh_grid),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE),
    function(i) {
      
      co <- as.numeric(mesh_grid[i, ]$cutoff)
      i_e <- as.numeric(mesh_grid[i, ]$max_inner_edge)
      mesh_id <- mesh_grid[i, ]$mesh_id
      
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
      
      formula <- as.formula(paste0(resp, " ~ s(local_density_km2_scaled, k = 3) +
                     s(months_extreme_drought_scaled, k = 3) +
                     s(fire_frequency_scaled, k = 3) +
                     s(mat_coef_scaled, k = 3) + 
                     s(n_deposition_scaled, k = 3)"))
      
      fit_cv <- tryCatch({
        sdmTMB::sdmTMB_cv(
          formula,
          data = dt_sub,
          mesh = mesh,
          k_folds = 5,
          #  family = sdmTMB::student(),
          spatial = "on",
          #  fold_ids = "fold_id", 
          parallel = FALSE,
          reml = T
        )
      }, error = function(e) {
        message("Skipping CV model due to error: ", e$message)
        return(NULL)
      })
      
      if(is.null(fit_cv)) return(NULL)
      
      
      cv_model_id <- paste0("cv_", tier, "_", mesh_id, "_1000m_local_density_subsets")
      
      tmp_tidy <- data.frame(
        tier = tier, 
        filter_call = filter_call,
        cutoff = co,
        max_inner_edge = i_e,
        mesh_id = mesh_id,
        n_vertices = nrow(mesh$mesh$loc),
        all_converged = fit_cv$converged,
        p_d_hessian = sum(fit_cv$pdHess),
        response = resp,
        sum_loglik = fit_cv$sum_loglik,
        n = nrow(dt_mod),
        log_cpo_approx = fit_cv$sum_loglik / nrow(dt_sub),
        model_id = cv_model_id,
        model_path = paste0("builds/cv_subset_models/", cv_model_id, ".Rds")
      )
      
      saveRDS(fit_cv, file = tmp_tidy$model_path)
      
      rm(fit_cv)
      gc()
      
      tmp_tidy
    }
  )
  
  dt_mesh_res_sub <- rbindlist(list_mesh_res_sub)
  
  dt_mesh_res <- rbind(dt_mesh_res_sub, dt_mesh_res)
  
  print(paste0("Finished with tier: ", tier, " at ", Sys.time()))
  
}
plan(sequential)

print(paste0("Started loop at: ", start_time, " and finished at: ", Sys.time()))
print(paste0("Estimate time for CV: ", round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2), " mins"))

## bind results 
unique(responses)
dt_mesh_res_fin <- dt_mesh_res  %>% 
  mutate(clean_response = case_when(
    .default = response,
    response == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
    response == "evi_900m_coef" ~  "EVI Trend",
    response == "canopy_height_900m_coef" ~  "Canopy Height Trend",
    response == "tree_cover_sd_1000m_coef" ~ "Tree Cover SD Trend",
    response == "evi_sd_900m_coef" ~ "EVI SD Trend", 
    response == "canopy_height_sd_900m_coef" ~ "Canopy Height SD Trend"))
summary(dt_mesh_res_fin)

fwrite(dt_mesh_res_fin, "builds/model_outputs/cv_mesh_selection_sdmtmb_1000m_subsets.csv")
n_distinct(dt_mesh_res_fin$model_id)

##### run models --------------------------------------------

dt_mesh_res_fin <- fread("builds/model_outputs/cv_mesh_selection_sdmtmb_1000m_subsets.csv")

dt_best_mesh <- dt_mesh_res_fin %>% 
  group_by(tier) %>% 
  slice_max(sum_loglik) %>% 
  ungroup()



### - Use best  Mesh ------------------

plan(multisession, workers = 15)
#options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
start_time <- Sys.time()

best_mesh_res_list <- future_map(1:nrow(dt_best_mesh),
                                 .progress = TRUE,
                                 .options = furrr_options(seed = TRUE),
                                 function(i) {
                                   
                                   tier = dt_best_mesh[i,]$tier
                                   resp = dt_best_mesh[i,]$response 
                                   filter_call = dt_best_mesh[i,]$filter_call
                                   
                                   dt_sub <- dt_mod %>% 
                                     dplyr::filter(eval(parse(text = filter_call))) %>% 
                                     mutate(
                                       local_density_km2_scaled = as.numeric(scale(local_density_km2)),
                                       mean_density_km2_scaled = as.numeric(scale(mean_density_km2)),
                                       months_extreme_drought_scaled = as.numeric(scale(months_extreme_drought)),
                                       fire_frequency_scaled = as.numeric(scale(fire_frequency)),
                                       mat_change_scaled = as.numeric(scale(mat_change)),
                                       prec_change_scaled = as.numeric(scale(prec_change)),
                                       mat_coef_scaled = as.numeric(scale(mat_coef)),
                                       prec_coef_scaled = as.numeric(scale(prec_coef)),
                                       n_deposition_scaled = as.numeric(scale(n_deposition)), 
                                       mat_scaled = as.numeric(scale(mat)), 
                                       map_scaled = as.numeric(scale(map))
                                     )
                                   
                                   
                                   co <- as.numeric(dt_best_mesh[i, ]$cutoff)
                                   i_e <- as.numeric(dt_best_mesh[i, ]$max_inner_edge)
                                   
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
                                   

                                   int_formula <- as.formula(paste0(resp, "~ 1"))
                                   
                                   re_formula <- as.formula(paste0(resp, "~ 1 + (1 | park_id)"))
                                   
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
                                   fit0 <- sdmTMB(int_formula,
                                                  spatial = "off",
                                                  data = dt_sub,
                                                  mesh = mesh, 
                                                  reml = T)
                                   
                                   #intercept and random effect  
                                   #                     fit1 <- update(fit0, 
                                   #                                     spatial = "off", 
                                   #                                    formula. = re_formula, 
                                   #                                   reml = T)
                                   
                                   #intercept and spatial  
                                   fit2 <- update(fit0, 
                                                  spatial = "on", 
                                                  reml = T)
                                   
                                   #intercept,  random effect and spatial  
                                   #   fit3 <- update(fit0, 
                                   #                   spatial = "on", 
                                   #                   formula. = re_formula, 
                                   #                   reml = T)
                                   
                                   #full model 
                                   fit_full <- update(fit0, 
                                                      spatial = "on", 
                                                      formula. = full_formula, 
                                                      reml = T)
                                   
                                   #fixed effects only 
                                   fit4 <- update(fit0, 
                                                  spatial = "off", 
                                                  formula. = fixed_formula, 
                                                  reml = T)
                                   
                                   # total proportion deviance explained by our full model:
                                   (dev_explained_full <- 1 - deviance(fit_full) / deviance(fit0))
                                   
                                   # proportion deviance explained by the random effect:
                                   #   (dev_explained_re <- 1 - deviance(fit1) / deviance(fit0))
                                   
                                   # proportion deviance explained by the mesh:
                                   (dev_explained_spatial <- 1 - deviance(fit2) / deviance(fit0))
                                   
                                   # proportion deviance explained by the mesh and RE:
                                   #  (dev_explained_re_spatial <- 1 - deviance(fit3) / deviance(fit0))
                                   
                                   # proportion deviance explained by the covariate:
                                   (dev_explained_var <- 1 - deviance(fit4) / deviance(fit0))
                                   
                                   # proportion covariate deviance explained compared to just the spatial field:
                                   # i.e., how much additional deviance is explained by the covariate beyond what
                                   # the spatial structure explains
                                   (dev_explained_just_var <- 1 - deviance(fit_full) / deviance(fit2))
                                   
                                   san <- sdmTMB::sanity(fit_full)
                                   
                                   model_id = paste0("subset_",tier, "_1000m_local_density")
                                   
                                   tmp_tidy <- broom::tidy(fit_full, conf.int = TRUE) %>%
                                     #dplyr::filter(!grepl("(Intercept)", term)) %>%
                                     dplyr::mutate(sig = case_when(
                                       .default = "non-significant",
                                       conf.low > 0 ~ "positive",
                                       conf.high < 0 ~ "negative"
                                     )) %>%
                                     dplyr::mutate(
                                       response = resp, 
                                       tier = tier,
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
                                       model_path = paste0("builds/subset_models/", model_id, ".Rds"),
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
    response == "evi_900m_coef" ~  "EVI Trend",
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
fwrite(dt_res, "builds/model_outputs/subset_results_1000m_local_density.csv")
