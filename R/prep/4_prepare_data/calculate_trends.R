library(remotePARTS)
library(tidyverse)
library(data.table)
library(future)
library(furrr)
library(tictoc)

#param = "pas"
#param = "grid"
#param = "pa_points"
param = "pa_grid_100m"
#param = "pa_grid_1000m"


if(param == "pa_points"){
  dt <- fread("data/processed_data/data_fragments/pa_points_with_timeseries.csv") %>% 
    as.data.frame() 
} else if(param == "pa_grid_100m"){
  dt <- fread("data/processed_data/data_fragments/pa_grid_100m_with_timeseries.csv") %>% 
    as.data.frame()
} else if(param == "pa_grid_1000m"){
  dt <- fread("data/processed_data/data_fragments/pa_grid_1000m_with_timeseries.csv") %>% 
    as.data.frame()
}



#dt <- dt %>% sample_n(1000)

# Define a helper function to process trends
process_trend <- function(cols_pattern, trend_name, dt) {
  
  cols <- grep(cols_pattern, names(dt), value = TRUE)
  
  dt_subset <- dt %>% dplyr::select(all_of(cols), lon, lat, unique_id) %>% 
    filter(complete.cases(.)) %>% as.data.frame()
  
  Y <- as.matrix(dt_subset[, cols])
  coords <- as.matrix(dt_subset[, c("lon", "lat")])
  
  ar_results <- fitAR_map(Y = Y, coords = coords)
  
  dt_subset[[paste0(trend_name, "_coef")]] <- coefficients(ar_results)[, "t"] 
  dt_subset[[paste0(trend_name, "_p_value")]] <- ar_results$pvals[, 2]
  
  
  dt_subset <- dt_subset %>% 
    dplyr::select(paste0(trend_name, "_coef"),
                  paste0(trend_name, "_p_value"),
                  unique_id)
  
  return(dt_subset)
  rm(dt_subset)
  gc()
}

# List of trends
trend_configs <- data.frame(
  pattern = c("grass_cover_100m", "gr_n_cr_cover_100m",
              "tree_cover_100m", "tree_cover_sd_100m", "shrub_cover_100m", "bare_cover_100m",
              "evi_90m", "canopy_height_90m", 
              "habitat_diversity_100m_", "canopy_height_sd_90m_",
              "evi_sd_90m_", 
              "grass_cover_1000m", "gr_n_cr_cover_1000m",
              "tree_cover_1000m", "tree_cover_sd_1000m", "shrub_cover_1000m", "bare_cover_1000m",
              "evi_900m", "canopy_height_900m", 
              "habitat_diversity_1000m_", "canopy_height_sd_900m_",
              "evi_sd_900m_", 
              "mat_", "prec_", 
              "burned_area_"),
  name = c("grass_cover_100m", "gr_n_cr_cover_100m",
           "tree_cover_100m", "tree_cover_sd_100m", "shrub_cover_100m", "bare_cover_100m",
           "evi_90m", "canopy_height_90m", 
           "habitat_diversity_100m", "canopy_height_sd_90m",
           "evi_sd_90m", 
           "grass_cover_1000m", "gr_n_cr_cover_1000m",
           "tree_cover_1000m", "tree_cover_sd_1000m", "shrub_cover_1000m", "bare_cover_1000m",
           "evi_900m", "canopy_height_900m", 
           "habitat_diversity_1000m", "canopy_height_sd_900m",
           "evi_sd_900m", 
           "mat", "prec", 
           "burned_area"),
  stringsAsFactors = FALSE
)



################################## LOOOOOOOOOOOOP ############################            
options(future.globals.maxSize = 10 * 1024^3)  # 10 GB
plan(multisession, workers = 13)
tic()

# Add chunk_id column
chunk_size <- 250000
dt$chunk_id <- ceiling(seq_len(nrow(dt)) / chunk_size)
table(dt$chunk_id)

all_trends_list <- list()
dt_trend <- data.table()

tic()
for (chunk in unique(dt$chunk_id)) {
  
  
  print(paste0("Starting with chunk ", chunk, " of ", max(dt$chunk_id)))
  
  dt_chunk <- dt[dt$chunk_id == chunk, ]
  
  
  dt_trend_chunk_list <- future_map(1:nrow(trend_configs),
                             .progress = TRUE,
                             .options = furrr_options(seed = TRUE),
                             function(i) {
                               
                               #for(i in 1:nrow(trend_configs)){
                               config <- trend_configs[i, ]
                               
                               dt_sub <- process_trend(config$pattern, config$name, dt_chunk)
                               
                               return(dt_sub)
                               
                               print(paste0(config$name, " done! ", Sys.time()))
                               
                             }
  )
  
  dt_trend_chunk <- dt_trend_chunk_list %>%
    reduce(~ left_join(.x, .y, by = "unique_id"))
  
  dt_trend <- rbind(dt_trend, dt_trend_chunk)
  all_trends_list[[as.character(chunk)]] <- dt_trend_chunk
  
  
  print(paste0(chunk, " done! ", Sys.time()))
  
  rm(dt_trend_chunk)
  gc()

}

toc()
plan(sequential)
Sys.time()
dt_trend_from_list <- rbindlist(all_trends_list)

#gotta keep also the starting conditions
ctk <- dt %>% dplyr::select(unique_id,
                            mean_grass_cover_100m, grass_cover_100m_2015_2016,
                            mean_gr_n_cr_cover_100m, gr_n_cr_cover_100m_2015_2016,
                            mean_shrub_cover_100m, shrub_cover_100m_2015_2016,
                            mean_tree_cover_100m, tree_cover_100m_2015_2016,
                            mean_tree_cover_sd_100m, tree_cover_sd_100m_2015_2016,
                            mean_bare_cover_100m, bare_cover_100m_2015_2016,
                            mean_evi_90m, evi_90m_2013_2014,
                            mean_canopy_height_90m,  canopy_height_90m_2000,
                            mean_habitat_diversity_100m, habitat_diversity_100m_2015_2016,
                            mean_evi_sd_90m, evi_sd_90m_2013_2014,
                            mean_canopy_height_sd_90m, canopy_height_sd_90m_2000,
                            mean_grass_cover_1000m, grass_cover_1000m_2015_2016,
                            mean_gr_n_cr_cover_1000m, gr_n_cr_cover_1000m_2015_2016,
                            mean_shrub_cover_1000m, shrub_cover_1000m_2015_2016,
                            mean_tree_cover_1000m, tree_cover_1000m_2015_2016,
                            mean_tree_cover_sd_1000m, tree_cover_sd_1000m_2015_2016,
                            mean_bare_cover_1000m, bare_cover_1000m_2015_2016,
                            mean_evi_900m, evi_900m_2013_2014,
                            mean_canopy_height_900m, canopy_height_900m_2000,
                            mean_habitat_diversity_1000m, habitat_diversity_1000m_2015_2016,
                            mean_evi_sd_900m, evi_sd_900m_2013_2014,
                            mean_canopy_height_sd_900m, canopy_height_sd_900m_2000,
                            mean_mat, mean_prec, 
                            mean_burned_area)

dt_res <- dt %>% 
  as.data.table() %>% 
  left_join(dt_trend) %>%
  dplyr::select(-all_of(grep("grass_cover_", names(dt), value = T)),
                -all_of(grep("gr_n_cr_cover_", names(dt), value = T)),
                -all_of(grep("shrub_cover_", names(dt), value = T)),
                -all_of(grep("tree_cover_", names(dt), value = T)),
                -all_of(grep("bare_cover_", names(dt), value = T)),
                -all_of(grep("habitat_diversity_", names(dt), value = T)),
                -all_of(grep("evi_", names(dt), value = T)),
                -all_of(grep("canopy_height_", names(dt), value = T)),
                -all_of(grep("mat_", names(dt), value = T)),
                -all_of(grep("prec_", names(dt), value = T)),
                -all_of(grep("burned_area_", names(dt), value = T))) %>% 
  left_join(ctk)


summary(dt_res)

if(param == "pa_points"){
  fwrite(dt_res, "data/processed_data/data_fragments/pa_points_with_trends.csv")
} else if(param == "pa_grid_100m"){
  fwrite(dt_res, "data/processed_data/data_fragments/pa_grid_100m_with_trends.csv")
} else if(param == "pa_grid_1000m"){
  fwrite(dt_res, "data/processed_data/data_fragments/pa_grid_1000m_with_trends.csv")
} 
