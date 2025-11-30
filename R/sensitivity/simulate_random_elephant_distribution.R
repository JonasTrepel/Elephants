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
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100)



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

dt_mesh_res_fin <- fread("builds/model_outputs/cv_mesh_selection_sdmtmb_results_1000m_local_density_smoothed.csv")

dt_mesh_res <- dt_mesh_res_fin %>% 
  group_by(response) %>% 
  slice_max(sum_loglik) %>% 
  ungroup()

iter = c(1:101)
resp = c(unique(dt_best_mesh$response))

model_guide = CJ(iter = iter, 
                 response = resp) 



plan(multisession, workers = 10)
#options(future.globals.maxSize = 15 * 1024^3)  # 15 GiB
start_time <- Sys.time()

best_mesh_res_list <- future_map(1:nrow(model_guide),
                                 .progress = TRUE,
                                 .options = furrr_options(seed = TRUE),
                                 function(i) {
                                   
                                   

                                   resp <- model_guide[i, ]$response
                                   
                                   
                                   iter = model_guide[i, ]$iter
                                   
                                   set.seed(iter)
                                   
                                   if(iter == 1){
                                     
                                     dt_sub = dt_mod
                                   }else{
                                     
                                     dt_sub = dt_mod %>% 
                                       mutate(sim_ld = sample(local_density_km2, size = nrow(.), replace = TRUE), 
                                              local_density_km2_scaled = as.numeric(scale(sim_ld)))
                                     
                                   }
                                   
                                
                                   
                                   int_formula <- as.formula(paste0(resp, "~ 1"))
                                   
                                   ele_formula <- as.formula(paste0(resp, " ~ 
                                s(local_density_km2_scaled, k = 3)"))
                                   
                                   #https://github.com/pbs-assess/sdmTMB/issues/466#issuecomment-3119589818
                                   
                                   #intercept only 
                                   fit_int <- tryCatch(
                                     {
                                       sdmTMB(
                                         int_formula,
                                         spatial = "off",
                                         data = dt_sub,
                                         reml = TRUE
                                       )
                                     },
                                     error = function(e) NULL
                                   )
                                   
                                   if(is.null(fit_int)){return(NULL)}
                                   
                                   fit_ele <- tryCatch(
                                     {
                                       sdmTMB(
                                         ele_formula,
                                         spatial = "off",
                                         data = dt_sub,
                                         reml = TRUE
                                       )
                                     },
                                     error = function(e) NULL
                                   )
                                   
                                   if(is.null(fit_ele)){return(NULL)}
                                   

                                   # proportion deviance explained by elephants:
                                   (dev_explained_ele <- 1 - deviance(fit_ele) / deviance(fit_int))
                                   
                                   delta_aic =  AIC(fit_int) - AIC(fit_ele) #when positive, intercept is larger
                                   
                                   san <- sdmTMB::sanity(fit_ele)
                                   
                                   tmp_tidy <- broom::tidy(fit_ele, conf.int = TRUE) %>%
                                     #dplyr::filter(!grepl("(Intercept)", term)) %>%
                                     dplyr::mutate(sig = case_when(
                                       .default = "non-significant",
                                       conf.low > 0 ~ "positive",
                                       conf.high < 0 ~ "negative"
                                     )) %>%
                                     dplyr::mutate(
                                       response = resp, 
                                       dev_explained_ele = dev_explained_ele, 
                                       delta_aic = delta_aic,
                                       iter = iter, 
                                       true_or_sim =ifelse(iter == 1, "true", "sim"),
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
                                   
                                   print(paste0(i, " done"))
                                   
                                   rm(fit_int, fit_ele)
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


p_ld = dt_res %>% 
  filter(term != ("(Intercept)"), true_or_sim == "sim") %>% 
  ggplot() +
  geom_vline(data = dt_res %>% 
               filter(term != ("(Intercept)"), true_or_sim == "true"), 
             aes(xintercept = dev_explained_ele*100), 
             linewidth = 1, color = "red", linetype = "dashed") +
  labs(x = "Deviance Explained (%)", y = "Frequency", title = "Simulated Local Density", 
       subtitle = "Red dashed line indicated actual value") +
  geom_histogram(aes(x = dev_explained_ele*100), bins = 100) +
  facet_wrap(~clean_response, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = .5), 
        plot.subtitle = element_text(hjust = .5), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_ld
ggsave(plot = p_ld, "builds/plots/supplement/deviance_explained_simulated_local_density.png", 
       dpi = 900, width = 8, height = 4)
#fwrite(dt_res, "builds/model_outputs/sdmtmb_results_1000m_local_density_smoothed.csv")
