
library(remotePARTS)
library(tidyverse)
library(data.table)
library(future)
library(furrr)
library(tictoc)

#param = "pas"
#param = "grid"
#param = "pa_grid"
param = "pa_points"

if(param == "pa_points"){
  dt <- fread("data/processed_data/data_fragments/pa_points_with_timeseries.csv") %>% 
    as.data.frame() 
} else if(param == "pa_grid"){
  dt <- fread("data/processed_data/data_fragments/pa_grid_with_timeseries.csv") %>% 
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
}

# List of trends
trend_configs <- data.frame(
  pattern = c("grass_cover_", "gr_n_cr_cover_",
              "tree_cover_", "shrub_cover_", "bare_cover_",
              "habitat_diversity_100m_", "habitat_diversity_1000m_", 
              "mean_evi_", 
              "mat_", "prec_", 
              "burned_area_"),
  name = c("grass_cover", "gr_n_cr_cover",
           "tree_cover", "shrub_cover", "bare_cover",
            "habitat_diversity_100m", "habitat_diversity_1000m", 
           "mean_evi", 
           "mat", "prec", 
           "burned_area"),
  stringsAsFactors = FALSE
)






################################## LOOOOOOOOOOOOP ############################            
options(future.globals.maxSize = 10 * 1024^3)  # 10 GB
plan(multisession, workers = 11)
tic()

# Add chunk_id column
chunk_size <- 500000
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

}

toc()
plan(sequential)
Sys.time()
dt_trend_from_list <- rbindlist(all_trends_list)


ctk <- dt %>% dplyr::select(unique_id,
                            mean_grass_cover, mean_gr_n_cr_cover, 
                            mean_shrub_cover, mean_tree_cover, mean_bare_cover,
                            mean_habitat_diversity_100m, mean_habitat_diversity_1000m,
                            mean_evi, 
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
                -all_of(grep("mean_evi_", names(dt), value = T)),
                -all_of(grep("mat_", names(dt), value = T)),
                -all_of(grep("prec_", names(dt), value = T)),
                -all_of(grep("burned_area_", names(dt), value = T))) %>% 
  left_join(ctk)


summary(dt_res)

if(param == "pa_points"){
  fwrite(dt_res, "data/processed_data/data_fragments/pa_points_with_trends.csv")
} else if(param == "pa_grid"){
  fwrite(dt_res, "data/processed_data/data_fragments/pa_grid_with_trends.csv")
} 
