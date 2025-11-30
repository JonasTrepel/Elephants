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
cents <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") %>% 
  st_centroid(.) %>% 
  mutate(x_moll_km = (st_coordinates(.)[,1])/1000, 
         y_moll_km = (st_coordinates(.)[,2])/1000) %>% 
  as.data.table() %>% 
  mutate(geom = NULL) %>% 
  select(park_id = NAME, area_km2, x_moll_km, y_moll_km)

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100) 



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
  ungroup() 

glimpse(dt_mod)

dt_mod$sd_local_density_km2
dt_mod$sd_local_density_km2


dt_mod %>% ggplot() + 
  geom_point(aes(x = tree_cover_1000m_coef, y = cv_local_density_km2))


dt_cor <- dt_mod %>% 
  
  select(
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


### 3 - Choose Mesh ------------------
#https://www.biorxiv.org/content/10.1101/2022.03.24.485545v4.full.pdf

#https://becarioprecario.bitbucket.io/spde-gitbook/ch-intro.html on how to construct meshs , chapter 2.6 and 2.7

#the following may take a good couple of hours to finish 
responses <- c("tree_cover_1000m_coef", "canopy_height_900m_coef")

mesh_grid <- expand.grid(max_inner_edge = seq(300, 900, by = 300),
                         cutoff = c(200, 400, 800, 1600), 
                         response = responses) %>% 
  mutate(mesh_id = paste0("mesh_", 1:nrow(.)))

plan(multisession, workers = 10)
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
          k_folds = 3,
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

fwrite(dt_mesh_res_fin, "builds/model_outputs/cv_mesh_selection_sdmtmb_results_park_average_density.csv")

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
ggsave(plot = p_loglik, "builds/plots/supplement/sum_loglik_different_meshs_park_average_density.png", dpi = 600, height = 4, width = 8)


