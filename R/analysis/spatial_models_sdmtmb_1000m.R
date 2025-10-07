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
  select(
    #mean values /habitat characteristics 
    mean_tree_cover_1000m, mean_evi_900m, mean_canopy_height_900m, 
    mean_habitat_diversity_1000m, mean_evi_sd_900m, mean_canopy_height_sd_900m, 
    
    #starting conditions
    tree_cover_1000m_2015_2016, evi_900m_2013_2014, canopy_height_900m_2000,
    habitat_diversity_1000m_2015_2016, evi_sd_900m_2013_2014, canopy_height_sd_900m_2000,
    
    # environmental predictors
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, months_severe_drought, months_extreme_drought, mat_coef, prec_coef, burned_area_coef,
    
    #Elephant predictors 
    mean_density_km2, local_density_km2, percent_population_growth,
    
    #Trends - Responses 
    tree_cover_1000m_coef, evi_900m_coef, canopy_height_900m_coef, 
    habitat_diversity_1000m_coef, evi_sd_900m_coef, canopy_height_sd_900m_coef, 

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
       k = 8,
       # method = "n_dist", 
       cat_col = "park_id") %>% 
  rename(fold_id = `.folds`) %>% 
  as.data.table() %>% 
  mutate(park_id = factor(park_id)) %>% 
  mutate(
    local_density_km2_scaled       = as.numeric(scale(local_density_km2)),
    percent_population_growth_scaled = as.numeric(scale(percent_population_growth)),
    months_severe_drought_scaled   = as.numeric(scale(months_severe_drought)),
    fire_frequency_scaled          = as.numeric(scale(fire_frequency)),
    mat_coef_scaled                = as.numeric(scale(mat_coef)),
    prec_coef_scaled               = as.numeric(scale(prec_coef)),
    n_deposition_scaled            = as.numeric(scale(n_deposition))
  )


park_counts <- dt_mod[, .N, by = park_id] %>% arrange(N)
print(park_counts)


hist(dt_mod$local_density_km2)
range(dt_mod$mean_density_km2)
dt_mod[dt_mod$mean_density_km2 > 6,]


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


#Check correlations 
dt_corr <- dt_mod %>% 
  select(-c(x_mollweide, y_mollweide, lon, lat, park_id, cluster_id, grid_id, fold_id), -contains("scaled"))

corr <- round(cor(dt_corr), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

hist(dt_mod$habitat_diversity_1000m_coef)

dt_corr2 <- dt_mod %>% 
  select(local_density_km2, percent_population_growth, 
         months_severe_drought, months_severe_drought, 
         mat_coef, prec_coef, n_deposition,
         fire_frequency, burned_area_coef) %>% 
  filter(complete.cases(.))
ggcorrplot(round(cor(dt_corr2), 2), hc.order = TRUE, type = "lower",
           lab = TRUE)

hist(dt_mod$months_severe_drought)
hist(dt_mod$months_severe_drought)

library(performance)
hist(dt_mod$evi_900m_coef)
hist(dt_mod$tree_cover_1000m_coef)
hist(dt_mod$canopy_height_900m_coef)

#test if colinearity is an issue
m_test <- glmmTMB::glmmTMB(tree_cover_1000m_coef ~ 
                             local_density_km2_scaled * percent_population_growth_scaled +
                             local_density_km2_scaled * months_severe_drought_scaled +
                             local_density_km2_scaled * fire_frequency_scaled +
                             mat_coef_scaled +#* prec_coef_scaled +
                             n_deposition_scaled +
                           ( 1 | park_id), 
                           data = dt_mod)

summary(m_test)
check_collinearity(m_test)
MuMIn::r.squaredGLMM(m_test)

ggcorrplot(round(cor(dt_corr2), 2), hc.order = TRUE, type = "lower",
           lab = TRUE)

# get spatial data 
#sf_mod <- st_as_sf(dt_mod, coords = c("x_mollweide", "y_mollweide"), crs = "ESRI:54009")

### 2 - Choose Mesh ------------------
#https://www.biorxiv.org/content/10.1101/2022.03.24.485545v4.full.pdf

#https://becarioprecario.bitbucket.io/spde-gitbook/ch-intro.html on how to construct meshs , chapter 2.6 and 2.7

#the following may take a good couple of hours to finish 
mesh_grid <- expand.grid(max_inner_edge = seq(50, 150, by = 50), cutoff = seq(2, 20, by = 2), loc_cpo = NA)
responses <- c("tree_cover_1000m_coef", "evi_900m_coef", "canopy_height_900m_coef",
               "habitat_diversity_1000m_coef", "evi_sd_900m_coef", "canopy_height_sd_900m_coef")

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

      formula <- as.formula(paste0(resp, " ~ local_density_km2_scaled * percent_population_growth_scaled +
                             local_density_km2_scaled * months_severe_drought_scaled +
                             local_density_km2_scaled * fire_frequency_scaled +
                             mat_coef_scaled + 
                             n_deposition_scaled"))
      

      
      fit_cv <- sdmTMB::sdmTMB_cv(formula,
        data = dt_mod,
        mesh = mesh,
        spatial = "on", 
        fold_ids = "fold_id" # created above, folds are stratified to ensure each park is present in each fold 
      )
      
      fit <- sdmTMB::sdmTMB(formula,
                    data = dt_mod,
                    mesh = mesh,
                    spatial = "on"
      )
      

      gen_r2 <- performance::r2(fit) #1-sum((y-y_hat)^2)/sum((y-y_bar)^2)
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
          sum_loglik = fit_cv$sum_loglik, 
          n = nrow(dt_mod), 
          generic_r2 = gen_r2, 
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
unique(responses)
dt_mesh_res <- rbindlist(mesh_res_list) %>% 
  mutate(clean_response = case_when(
    .default = response,
    response == "tree_cover_1000m_coef" ~ "Woody Cover Trend",
    response == "evi_900m_coef" ~  "EVI Trend",
    response == "canopy_height_900m_coef" ~  "Canopy Height Trend",
    response == "habitat_diversity_1000m_coef" ~ "Habitat Diversity Trend",
    response == "evi_sd_900m_coef" ~ "EVI SD Trend", 
    response == "canopy_height_sd_900m_coef" ~ "Canopy Height SD Trend"
    ), 
    clean_term = case_when(
      .default = term,
      term == "local_density_km2_scaled" ~ "Local Elephant Density",
      term == "percent_population_growth_scaled" ~ "Elephant Population Growth",
      term == "mat_coef_scaled" ~ "MAT Trend",
      term == "prec_coef_scaled" ~ "Precipitation Trend",
      term == "n_deposition_scaled" ~ "Nitrogen deposition",
      term == "fire_frequency_scaled" ~ "Fire frequency",
      term == "months_severe_drought_scaled" ~ "N Drought Months", 
      term == "local_density_km2_scaled:percent_population_growth_scaled" ~ "Local Ele. Density:Ele. Pop. Growth",
      term == "local_density_km2_scaled:months_severe_drought_scaled" ~ "Local Ele. Density:Drought Months",
      term == "local_density_km2_scaled:fire_frequency_scaled" ~ "Local Ele. Density:Fire Frequency",
      term == "mat_coef_scaled:prec_coef_scaled" ~ "MAT Trend:Prec Trend"))
unique(dt_mesh_res$clean_term)
summary(dt_mesh_res)
fwrite(dt_mesh_res, "builds/model_outputs/sdmtmb_results_1000m.csv")

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
ggsave(plot = p_covs, "builds/plots/supplement/cov_estimates_different_meshs_1000m.png", dpi = 600, height = 12, width = 12)

p_est <- dt_mesh_res %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
    "Habitat Diversity Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
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
ggsave(plot = p_est, "builds/plots/cov_estimates_best_mesh_1000m.png", dpi = 600, height = 6.5, width = 10)

p_cpo <- dt_mesh_res %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend", 
    "Habitat Diversity Trend", "Canopy Height SD Trend", "EVI SD Trend"))) %>% 
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
ggsave(plot = p_cpo, "builds/plots/supplement/log_cpo_approx_different_meshs_1000m.png", dpi = 600, height = 8, width = 8)


#best AIC and logCPO at 2km cutoff... 
dt_mesh_res %>% 
  filter(sanity_checks == TRUE) %>% 
  group_by(response, term) %>% 
  slice_min(aic) %>% 
  ungroup() %>% 
  select(cutoff, max_inner_edge, response) %>% unique()


