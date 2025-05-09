library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(tidylog)
library(sf)


### define if we want to run it for control or PA 

param <- "grid"
#param <- "points"

if(param == "grid"){
  vect <- read_sf("data/spatial_data/grid/empty_grid.gpkg") %>% 
    mutate(unique_id = grid_id)
} else if(param == "points"){
  vect <- read_sf("data/spatial_data/...")
}  

# get legends -----------

lc_leg <- data.frame(
  land_cover_num = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
  land_cover = c(
    "tree_cover",
    "shrubland",
    "grassland",
    "cropland",
    "built_up",
    "bare_sparse_vegetation",
    "snow_ice",
    "water",
    "herbaceous_wetland",
    "mangroves",
    "moss_lichen"
  ))


wwf_biome <- read_sf("data/spatial_data/covariates/vector/WWF_BIOMES.gpkg")

biome_leg <- wwf_biome %>%
  as.data.table() %>%
  mutate(geom = NULL) %>%
  unique() %>% 
  rename(olson_biome_num = BIOME, 
         olson_biome = BIOME_Name)

fun_biome_leg <- data.table(
  functional_biome_num = c(1:24), 
  functional_biome = c("SLC","SMC","SHC","TLC","TMC","THC",
                       "SLD","SMD","SHD","TLD","TMD","THD",
                       "SLB","SMB","SHB","TLB","TMB","THB","SLN","SMN","SHN",
                       "TLN","TMN","THN"))

#### EXTRACT CONTINUOUS COVS #### ----------------------------------
col_names <- c(
  
  #### Continuous ####
  "elevation", ## Elevation
  "map", ## MAP
  "mat", ## MAT
  
  "slope", #Slope
  "enerscape", #Energy landscape 
  
  "distance_to_water_km", # Distance to water
  "distance_to_settlement_km", # Distance to settlement 
  
  "water_fraction", #Fraction of water
  "settlement_fraction", #Fraction of settlement 

  "n_deposition", ## Nitrogen depo
  "human_modification", #Human modification index
  
  "evi_mean", #Average EVI
  "evi_dry_season", #Average EVI in the dry season
  "evi_wet_season", #Average EVI in the wet season
  
  #### categorical ####
  "functional_biome_num", # functional biome 
  "olson_biome_num", # olson biome 
  "land_cover_num" #esa landcover
)

cov_paths <- c(
 
   #### Continuous ####
  "data/spatial_data/covariates/raster/nasa_dem_90m.tif", ## Elevation
  "data/spatial_data/covariates/raster/CHELSA_bio12_1981-2010_V.2.1.tif", ## MAP
  "data/spatial_data/covariates/raster/CHELSA_bio1_1981-2010_V.2.1.tif", ## MAT
  
  "data/spatial_data/covariates/raster/slope_degree.tif", #Slope
  "data/spatial_data/covariates/raster/energyscape_kcal.tif", #Energy landscape 
  
  "data/spatial_data/covariates/raster/distance_to_water_km.tif", # Distance to water
  "data/spatial_data/covariates/raster/distance_to_settlement_km.tif", # Distance to settlement 
  
  "data/spatial_data/covariates/raster/esa_wc_water_2021_10m.tif", #Fraction of water
  "data/spatial_data/covariates/raster/world_settlement_footprint_2015_10m.tif", #Fraction of settlement 
  
  "data/spatial_data/covariates/raster/total_N_dep.tif", ## Nitrogen depo
  "data/spatial_data/covariates/raster/human_land_modification_lulc_kennedy.tif", #Human modification index
  
  "data/spatial_data/covariates/raster/mean_evi_2001_2024.tif", #Average EVI
  "data/spatial_data/covariates/raster/dry_season_mean_evi_2001_2024.tif", #Average EVI in the dry season
  "data/spatial_data/covariates/raster/wet_season_mean_evi_2001_2024.tif", #Average EVI in the wet season
  
  #### categorical ####
  "data/spatial_data/covariates/raster/higgins_functional_biomes.tif", # functional biome 
  "data/spatial_data/covariates/raster/wwf_olson_biome.tif", # olson biome 
  "data/spatial_data/covariates/raster/esa_world_cover_2021_10m.tif" #esa landcover
)


funcs <- c(rep("mean", 14), rep("mode", 3))

covs <- data.table(
  col_name = col_names, 
  cov_path = cov_paths, 
  func = funcs
) %>% filter(!is.na(cov_paths))

#### write a little loop to plot all rasters and check if they're fine 

 for(i in 1:nrow(covs)) {
   cov_r <- rast(covs[i, ]$cov_path)
   plot(cov_r, main = paste0(covs[i, ]$col_name))
   Sys.sleep(2)
   
 }


vect_covs_raw <- vect %>% as.data.table() %>% mutate(geom = NULL, x = NULL, geometry = NULL)

############### create cluster ####################
library(doSNOW)
library(foreach)
library(tictoc)

# Create and register a cluster
clust <- makeCluster(15)
registerDoSNOW(clust)

## progress bar 
iterations <- nrow(covs)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

##############################################################################            
################################## LOOOOOOOOOOOOP ############################            
##############################################################################    

tic()

dt_covs <- foreach(i = 1:nrow(covs),
                  .packages = c('tidyverse', 'exactextractr', 'data.table', 'terra', 'sf'),
                  .options.snow = opts,
                  .inorder = FALSE,
                  .verbose = TRUE, 
                  .combine = left_join) %dopar% {
                    
                    #for(i in 1:nrow(covs)){
                    
                    cov_r <- rast(covs[i, ]$cov_path)
                    
                    world_grid_t <- st_transform(vect, crs = st_crs(cov_r))
                    
                    all_polys <- vect %>% dplyr::select(unique_id)
                    
                    func <- covs[i, ]$func
                    
                    
                    extr <- exactextractr::exact_extract(cov_r, 
                                                         world_grid_t, 
                                                         append_cols = c("unique_id"),
                                                         fun = func)
                    
                    setnames(extr, func, covs[i, ]$col_name)
                    
                    dt_extr <- all_polys %>%
                      left_join(extr %>% unique()) %>%
                      as.data.table() %>%
                      mutate(geom = NULL, x = NULL, geometry = NULL) %>% 
                      unique()
                    
                    return(dt_extr)
                    
                  }

stopCluster(clust)
toc()


dt_vect_covs <- vect_covs_raw %>% 
  left_join(dt_covs) %>% 
  left_join(biome_leg) %>%
  left_join(fun_biome_leg) %>% 
  left_join(lc_leg) %>% 
  as.data.table() %>% 
  mutate(x = NULL, 
         geom = NULL) %>% 
  dplyr::select(-land_cover_num, -functional_biome_num, -olson_biome_num)


summary(dt_vect_covs)
          



library(ggcorrplot)
corr <- round(cor(dt_vect_covs  %>% 
                    dplyr::select(-unique_id, -functional_biome, -olson_biome, -land_cover) %>% 
                    mutate(grid_id = NULL) %>% 
                    filter(complete.cases(.))), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)


vect_covs <- dt_vect_covs %>% left_join(vect) %>% st_as_sf

mapview::mapview(vect_covs, zcol = "distance_to_water_km")

if(param == "grid"){
  
  fwrite(dt_vect_covs %>% 
           mutate(unique_id = NULL), "data/processed_data/data_fragments/grid_habitat_covariates.csv")
  
} else if(param == "points"){
  
  fwrite(dt_vect_covs, "data/processed_data/data_fragments/points_habitat_covariates.csv")
  
} 
