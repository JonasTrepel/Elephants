#library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(tidylog)
library(sf)
library(tictoc)
library(furrr)
library(terra)
library(exactextractr)
### define if we want to run it for control or PA 

# param <- "grid"
# param = "pa_grid"
 param = "pa_points"

if(param == "grid"){
  vect <- read_sf("data/spatial_data/grid/empty_grid.gpkg") %>% 
    mutate(unique_id = grid_id)
  
} else if(param == "pa_grid"){
  vect <- read_sf("data/spatial_data/grid/empty_grid_pas.gpkg") %>% 
    mutate(unique_id = grid_id)
  
} else  if(param == "pa_points"){
  vect <- read_sf("data/spatial_data/grid/empty_points_pas.gpkg") %>% 
    st_buffer(dist = 50)
  
  #dt_covs <- fread("data/processed_data/data_fragments/pa_points_with_covariates.csv")
}



## get file paths sorted 

#Grass cover
grass_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                              pattern = "grass_cover", 
                                              full.names = TRUE), 
                        filename = list.files("data/spatial_data/time_series/",
                                              pattern = "grass_cover", 
                                              full.names = FALSE)
) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename)


#Grass and Crop cover
gr_n_cr_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                      pattern = "gr_n_cr_cover", 
                                                      full.names = TRUE), 
                                filename = list.files("data/spatial_data/time_series/",
                                                      pattern = "gr_n_cr_cover", 
                                                      full.names = FALSE)
) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename)

#Shrub cover
shrub_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                      pattern = "shrub_cover", 
                                                      full.names = TRUE), 
                                filename = list.files("data/spatial_data/time_series/",
                                                      pattern = "shrub_cover", 
                                                      full.names = FALSE)
) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename)


#Tree cover
tree_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                      pattern = "tree_cover", 
                                                      full.names = TRUE), 
                                filename = list.files("data/spatial_data/time_series/",
                                                      pattern = "tree_cover", 
                                                      full.names = FALSE)
) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename)


#Bare Ground
bare_cover_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                     pattern = "bare_cover", 
                                                     full.names = TRUE), 
                               filename = list.files("data/spatial_data/time_series/",
                                                     pattern = "bare_cover", 
                                                     full.names = FALSE)
) %>% 
  mutate(filename = gsub("_100m.tif", "", filename), 
         colname = filename)


#Shannon 100m
shannon_100m_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                     pattern = "shannon_diversity",
                                                     full.names = TRUE),
                               filename = list.files("data/spatial_data/time_series/",
                                                     pattern = "shannon_diversity",
                                                     full.names = FALSE)
) %>%
  filter(!grepl("1000m", filename)) %>%
  mutate(filename = gsub(".tif", "", filename),
         colname = gsub("shannon_diversity_habitat_vegetation_types_", "habitat_diversity_", filename))

#Shannon 1000m
shannon_1000m_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                       pattern = "shannon_diversity",
                                                       full.names = TRUE),
                                 filename = list.files("data/spatial_data/time_series/",
                                                       pattern = "shannon_diversity",
                                                       full.names = FALSE)
) %>%
  filter(!grepl("100m", filename)) %>%
  mutate(filename = gsub(".tif", "", filename),
         colname = gsub("shannon_diversity_habitat_vegetation_types_", "habitat_diversity_", filename))

# EVI 90m
evi_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                   pattern = "hsl_clamped_evi_", 
                                                   full.names = TRUE), 
                             filename = list.files("data/spatial_data/time_series/",
                                                   pattern = "hsl_clamped_evi_", 
                                                   full.names = FALSE)) %>% 
  filter(grepl("90m", filename)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("hsl_clamped_evi_", "evi_", filename))

### MAT

mat_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                   pattern = "mat_", 
                                                   full.names = TRUE), 
                             filename = list.files("data/spatial_data/time_series/",
                                                   pattern = "mat_", 
                                                   full.names = FALSE)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("mat_", "mat_", filename))

### Prec sum 

prec_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                              pattern = "precipitation_sum", 
                                              full.names = TRUE), 
                        filename = list.files("data/spatial_data/time_series/",
                                              pattern = "precipitation_sum", 
                                              full.names = FALSE)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("precipitation_sum_", "prec_", filename))

### Burned area 

burned_area_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                              pattern = "burned_area_", 
                                              full.names = TRUE), 
                        filename = list.files("data/spatial_data/time_series/",
                                              pattern = "burned_area_", 
                                              full.names = FALSE)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("burned_area_5000m_", "burned_area_", filename))


### EVI SD 90m

evi_sd_90_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                      pattern = "hsl_sd_clamped", 
                                                      full.names = TRUE), 
                                filename = list.files("data/spatial_data/time_series/",
                                                      pattern = "hsl_sd_clamped", 
                                                      full.names = FALSE)) %>% 
  filter(grepl("90m", filename)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("_90m", "", filename), 
         colname =  gsub("hsl_sd_clamped_evi_", "evi_sd_90m_", colname))

### EVI SD 900m

evi_sd_900_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                    pattern = "hsl_sd_clamped", 
                                                    full.names = TRUE), 
                              filename = list.files("data/spatial_data/time_series/",
                                                    pattern = "hsl_sd_clamped", 
                                                    full.names = FALSE)) %>% 
  filter(grepl("900m", filename)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("_900m", "", filename), 
         colname =  gsub("hsl_sd_clamped_evi_", "evi_sd_900m_", colname))


### Canopy Height #30m

canopy_height_files <- data.table(filepath = list.files("data/spatial_data/time_series/",
                                                     pattern = "canopy_height", 
                                                     full.names = TRUE), 
                               filename = list.files("data/spatial_data/time_series/",
                                                     pattern = "canopy_height", 
                                                     full.names = FALSE)) %>% 
  filter(grepl("_30m", filename)) %>% 
  mutate(filename = gsub(".tif", "", filename),
         colname =  gsub("_30m", "", filename), 
         colname =  gsub("canopy_height", "canopy_height_30m", colname))



covs <- rbind(grass_cover_files, 
              gr_n_cr_cover_files, 
              tree_cover_files,
              shrub_cover_files,
              bare_cover_files,
              shannon_100m_files, 
              shannon_1000m_files, 
              mean_evi_files, 
              mat_files,
              prec_files,
              burned_area_files
              )



################################## LOOOOOOOOOOOOP ############################            
options(future.globals.maxSize = 10 * 1024^3)  # 10 GB
plan(multisession, workers = 15)
tic()

# Add chunk_id column
chunk_size <- 500000
vect$chunk_id <- ceiling(seq_len(nrow(vect)) / chunk_size)
table(vect$chunk_id)

all_dt_covs_list <- list()

tic()
for (chunk in unique(vect$chunk_id)) {
  
  
  print(paste0("Starting with chunk ", chunk, " of ", max(vect$chunk_id)))
  
  vect_chunk <- vect[vect$chunk_id == chunk, ]
  
  
  dt_covs_list <- future_map(1:nrow(covs),
                             .progress = TRUE,
                             .options = furrr_options(seed = TRUE),
                             function(i) {
                               
                               #for(i in 1:nrow(covs)){
                              
                               cov_r <- rast(covs[i, ]$filepath)
                               
                               vect_trans <- st_transform(vect_chunk, crs = st_crs(cov_r))
                               
                               extr <- exactextractr::exact_extract(cov_r, 
                                                                    append_cols = c("unique_id"),
                                                                    vect_trans, 
                                                                    fun = "mean")
                               
                               setnames(extr, "mean", covs[i, ]$colname)
                               
                               dt_extr_fin <- extr %>% 
                                 as.data.table() %>%
                                 mutate(geom = NULL) %>% 
                                 unique()
                              
                               print(paste0(covs[i, ]$col_name,"; i = ", i))
                               
                               return(dt_extr_fin)
                               
                             }
  )
  
  chunk_dt <- dt_covs_list %>%
    reduce(~ left_join(.x, .y, by = "unique_id"))
  
  all_dt_covs_list[[as.character(chunk)]] <- chunk_dt
}
toc()
plan(sequential)
Sys.time()
dt_covs <- rbindlist(all_dt_covs_list)



#combine
vect_covs <- vect %>% 
  as.data.table() %>% 
  mutate(x = NULL, geom = NULL, geometry = NULL) %>% 
  left_join(dt_covs) %>% 
  as.data.table() %>% 
  mutate(x = NULL, 
         geom = NULL,
         geometry = NULL) %>% 
  mutate(mean_grass_cover = rowMeans(select(., contains("grass_cover")), na.rm = TRUE), 
         mean_gr_n_cr_cover = rowMeans(select(., contains("gr_n_cr_cover")), na.rm = TRUE), 
         mean_tree_cover = rowMeans(select(., contains("tree_cover")), na.rm = TRUE), 
         mean_shrub_cover = rowMeans(select(., contains("shrub_cover")), na.rm = TRUE), 
         mean_bare_cover = rowMeans(select(., contains("bare_cover")), na.rm = TRUE), 
         mean_habitat_diversity_100m = rowMeans(select(., matches("habitat_diversity.*_100m")), na.rm = TRUE), 
         mean_habitat_diversity_1000m = rowMeans(select(., matches("habitat_diversity.*_1000m")), na.rm = TRUE), 
         mean_evi = rowMeans(select(., contains("mean_evi")), na.rm = TRUE), 
         mean_mat = rowMeans(select(., contains("mat")), na.rm = TRUE),
         mean_prec = rowMeans(select(., contains("prec")), na.rm = TRUE),
         mean_burned_area = rowMeans(select(., contains("burned_area")), na.rm = TRUE)) 


if(param == "grid"){
  
  dt_habitat_vars <- fread("")
  
  
  fwrite(vect_covs %>% 
           left_join(dt_habitat_vars) %>% 
           mutate(unique_id = NULL), "data/processed_data/data_fragments/grid_with_timeseries.csv")
  

} else if(param == "pa_grid"){
  

  dt_habitat_vars <- fread("data/processed_data/data_fragments/pa_grids_with_covariates.csv") %>% 
    mutate(wdpa_pid = as.character(wdpa_pid))
  
  
  fwrite(vect_covs %>% 
           left_join(dt_habitat_vars[, -c("x_mollweide", "y_mollweide", "lon", "lat")]),
         "data/processed_data/data_fragments/pa_grid_with_timeseries.csv")
  
} else if(param == "pa_points"){
  
  dt_habitat_vars <- fread("data/processed_data/data_fragments/pa_points_with_covariates.csv") %>% 
    mutate(wdpa_pid = as.character(wdpa_pid))
  
  
  fwrite(vect_covs %>% 
           left_join(dt_habitat_vars[, -c("x_mollweide", "y_mollweide", "lon", "lat")]),
         "data/processed_data/data_fragments/pa_points_with_timeseries.csv")
  
}
