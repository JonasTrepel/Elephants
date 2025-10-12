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
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

#load data 
sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg") 

dt <- fread("data/processed_data/clean_data/final_point_data.csv") %>% 
  mutate(grass_cover_coef = grass_cover_coef*100, 
         gr_n_cr_cover_coef = gr_n_cr_cover_coef*100, 
         tree_cover_coef = tree_cover_coef*100, 
         shrub_cover_coef = shrub_cover_coef*100, 
         bare_cover_coef = bare_cover_coef*100, 
         mean_evi_coef = mean_evi_coef/100
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
    mean_density_km2_scaled = as.numeric(scale(log(mean_density_km2))),
    density_km2_estimate_scaled = as.numeric(scale(density_km2_estimate)),
    mat_coef_scaled = as.numeric(scale(mat_coef)),
    prec_coef_scaled = as.numeric(scale(prec_coef)),
    n_deposition_scaled = as.numeric(scale(n_deposition)),
    fire_frequency_scaled = as.numeric(scale(fire_frequency)),
    burned_area_coef_scaled = as.numeric(scale(burned_area_coef)), 
    x_mollweide_scaled = as.numeric(scale(x_mollweide)), 
    y_mollweide_scaled = as.numeric(scale(y_mollweide)), 
    x_moll_km = x_mollweide/1000, 
    y_moll_km = y_mollweide/1000
  ) %>%
  group_by(park_id) %>% 
  filter(n() >= 10) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  fold(., #make sure to stratify folds in a way that each park is present in each fold
       k = 8,
       # method = "n_dist", 
       cat_col = "park_id") %>% 
  rename(fold_id = `.folds`) %>% 
  as.data.table() %>% 
  mutate(park_id = factor(park_id))


park_counts <- dt_mod[, .N, by = park_id] %>% arrange(N)
print(park_counts)


hist(dt_mod$local_density_km2_scaled)
range(dt_mod$mean_density_km2)


#check model data 
table(dt_mod$park_id)
n_distinct(dt_mod$park_id)
glimpse(dt_mod)

### plot final dataset -----
# World 
sf_world <- ne_countries(scale = "medium", returnclass = "sf")

# Africa 
sf_africa <- sf_world %>% filter(region_un == "Africa") %>% 
  filter(!name == "Madagascar") %>% 
  st_transform(., crs = 4326)


p_loc_fin <- dt_mod %>% ggplot() +
  ylim(-35, -7.5) +
  xlim(9, 40) +
  geom_sf(data = sf_africa, fill = "grey99") +
  geom_point(aes(x = lon, y = lat), size = 0.1, alpha = 0.5) + 
  geom_sf(data = sf_parks %>% filter(NAME %in% unique(dt_mod$park_id)) %>% 
            st_transform(crs = 4326), fill = "orange", alpha = 0.5) +
  theme_void() +
  theme(legend.position = "none")
p_loc_fin 

ggsave(plot = p_loc_fin, "builds/plots/pas_included_in_analysis.png", dpi = 600)

dt_mod %>% ggplot() +
  geom_point(aes(x = x_moll_km, y = y_moll_km, color = park_id), size = 0.2) + 
  theme_void() +
  theme(legend.position = "none")

#Check correlations 
dt_corr <- dt_mod %>% 
  select(-c(x_mollweide, y_mollweide, lon, lat, park_id, fold_id), -contains("scaled"))

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
sf_mod <- st_as_sf(dt_mod, coords = c("x_mollweide", "y_mollweide"), crs = "ESRI:54009")

### 2 - Choose Mesh ------------------
#https://www.biorxiv.org/content/10.1101/2022.03.24.485545v4.full.pdf

#https://becarioprecario.bitbucket.io/spde-gitbook/ch-intro.html on how to construct meshs , chapter 2.6 and 2.7

plot(dt_mod$x_moll_km, dt_mod$y_moll_km)
range(dt_mod$x_moll_km)
range(dt_mod$y_moll_km)

#how variable are our different responses?
sd(dt_mod$mean_evi_coef)/mean(dt_mod$mean_evi_coef)
sd(dt_mod$tree_cover_coef)/mean(dt_mod$tree_cover_coef)
sd(dt_mod$habitat_diversity_100m_coef)/abs(mean(dt_mod$habitat_diversity_100m_coef))
sd(dt_mod$habitat_diversity_1000m_coef)/abs(mean(dt_mod$habitat_diversity_1000m_coef))

#the following may take a good 45-60mins to finish 
mesh_grid <- expand.grid(max_inner_edge = seq(50, 150, by = 50), cutoff = seq(2, 20, by = 2), loc_cpo = NA)
responses <- c("tree_cover_coef", "mean_evi_coef", "habitat_diversity_100m_coef", "habitat_diversity_1000m_coef")

plan(multisession, workers = 4)
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
                                
                                inla_mesh <- fmesher::fm_mesh_2d_inla(
                                  loc = cbind(dt_mod$x_moll_km, dt_mod$y_moll_km),
                                  cutoff = co, max.edge = c(i_e, 10000)
                                )
                                
                                mesh <- make_mesh(
                                  data = dt_mod,
                                  xy_cols = c("x_moll_km", "y_moll_km"),
                                  mesh = inla_mesh
                                )
                                
                                nrow(mesh$mesh$loc)
                                #plot(mesh)
                                
                                formula <- as.formula(paste0(resp, " ~ mean_density_km2_scaled +
      density_km2_estimate_scaled +
      mat_coef_scaled +
      prec_coef_scaled+ 
      n_deposition_scaled +
      fire_frequency_scaled +
      burned_area_coef_scaled +
      (1 | park_id)"))
                                
                                
                                fit_cv <- tryCatch({
                                  sdmTMB::sdmTMB_cv(
                                    formula,
                                    data = dt_mod,
                                    mesh = mesh,
                                    spatial = "on",
                                    fold_ids = "fold_id"  # folds stratified so each park is present in each fold
                                  )
                                }, error = function(e) {
                                  message("sdmTMB_cv failed: ", e$message)
                                  return(NULL)
                                })
                                
                                if (is.null(fit_cv)) {next}
                                
                                
                                fit <- tryCatch({
                                  sdmTMB::sdmTMB(
                                    formula,
                                    data = dt_mod,
                                    mesh = mesh,
                                    spatial = "on"
                                  )
                                }, error = function(e) {
                                  message("sdmTMB failed: ", e$message)
                                  return(NULL)
                                })
                                
                                if (is.null(fit)) {next}
                                
                                # sanity(fit)
                                # summary(fit)
                                
                                san <- sdmTMB::sanity(fit)
                                
                                # AIC(fit) #-232056
                                
                                
                                
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
                                    n_vertices = nrow(mesh$mesh$loc),
                                    aic = AIC(fit),
                                    sanity_checks = all(san == TRUE),
                                    response = resp, 
                                    log_cpo_approx = fit_cv$sum_loglik / nrow(dt_mod)
                                  )
                                
                                
                                dt_mesh_res_sub <- rbind(tmp_tidy, dt_mesh_res_sub)
                                
                                print(paste0(i, " done"))
                              }
                              
                              
                              return(dt_mesh_res_sub)
                            }
)
plan(sequential)

print(paste0("Started loop at: ", start_time, " and finished at: ", Sys.time()))

## bind results 

dt_mesh_res <- rbindlist(mesh_res_list) %>% 
  mutate(clean_response = case_when(
    .default = response,
    response == "tree_cover_coef" ~ "Woody Cover Trend",
    response == "mean_evi_coef" ~  "EVI Trend",
    response == "habitat_diversity_100m_coef" ~ "Habitat Diversity Trend (100m)",
    response == "habitat_diversity_1000m_coef" ~ "Habitat Diversity Trend (1000m)"), 
    clean_term = case_when(
      .default = term,
      term == "local_density_km2_scaled" ~ "Local Elephant Density",
      term == "mean_density_km2_scaled" ~ "Mean Elephant Density",
      term == "density_km2_estimate_scaled" ~ "Elephant Density Trend",
      term == "mat_coef_scaled" ~ "MAT Trend",
      term == "prec_coef_scaled" ~ "Precipitation Trend",
      term == "n_deposition_scaled" ~ "Nitrogen deposition",
      term == "fire_frequency_scaled" ~ "Fire frequency",
      term == "burned_area_coef_scaled" ~ "Burned Area Trend"))

fwrite(dt_mesh_res, "builds/model_outputs/sdmtmb_results_mean_density.csv")
dt_mesh_res <- fread("builds/model_outputs/sdmtmb_results_mean_density.csv")

p_covs <- dt_mesh_res %>% 
  filter(!grepl("Intercept", term)) %>% 
  ggplot() +
  geom_pointrange(aes(x = cutoff, y = estimate, ymin = conf.low, ymax = conf.high, color = sig)) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("non-significant" = "grey", 
                                positive = "forestgreen", 
                                negative = "darkred")) +
  facet_grid(rows = vars(clean_response), cols = vars(clean_term), scales = "free") +
  theme(legend.position = "none")
p_covs
ggsave(plot = p_covs, "builds/plots/supplement/cov_estimates_different_meshs_mean_density.png", dpi = 600, height = 8, width = 12)

p_est <- dt_mesh_res %>% 
  filter(!grepl("Intercept", term)) %>% 
  #filter(sanity_checks == TRUE) %>% 
  group_by(response, term) %>% 
  slice_max(log_cpo_approx) %>% 
  ungroup() %>% 
  ggplot() +
  geom_vline(xintercept = 0,linetype = "dashed") +
  geom_pointrange(aes(y = clean_term, x = estimate, xmin = conf.low, xmax = conf.high, color = sig), 
                  linewidth = 1.1, size = 1.1) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("non-significant" = "grey", 
                                positive = "forestgreen", 
                                negative = "darkred")) +
  facet_wrap(~clean_response, scales = "free_x", ncol = 4) +
  labs(y = "") +
  theme(legend.position = "none")
p_est
ggsave(plot = p_est, "builds/plots/cov_estimates_best_mesh_mean_density.png", dpi = 600, height = 3, width = 9)

p_cpo <- dt_mesh_res %>% 
  ggplot() +
  geom_point(aes(x = cutoff, y = log_cpo_approx, color = max_inner_edge)) +
  facet_wrap(~clean_response, scales = "free") 
p_cpo
ggsave(plot = p_cpo, "builds/plots/supplement/log_cpo_approx_different_meshs_mean_density.png", dpi = 600, height = 8, width = 8)



dt_mesh_res %>% 
  # filter(sanity_checks == TRUE) %>% 
  group_by(response, term) %>% 
  slice_max(aic) %>% 
  ungroup() %>% 
  select(cutoff, max_inner_edge, response) %>% unique()


