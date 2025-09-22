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

# param = "pa_grid_100m"
# param = "pa_grid_1000m"
# param = "pa_points"
# param <- "steps_1hr"
# param <- "steps_3hrs"
# param <- "steps_12hrs"
# param <- "steps_24hrs"
#param <- "knp_elephants

params <- c("pa_grid_100m")
for(param in unique(params)){ 
  
if(param == "knp_elephants"){
    vect <- read_sf("data/spatial_data/elephants/elephant_count_points.gpkg") %>% 
      mutate(unique_id = paste0("point_", 1:nrow(.))) %>% 
      st_transform(crs = "ESRI:54009") %>% 
      st_buffer(dist = 100)
      
} else if(param == "pa_grid_100m"){
    vect <- read_sf("data/spatial_data/grid/empty_grid_pas_100m.gpkg") %>% 
      mutate(
        unique_id = grid_id)
    
} else if(param == "pa_grid_1000m"){
  vect <- read_sf("data/spatial_data/grid/empty_grid_pas_1000m.gpkg") %>% 
    mutate(
      unique_id = grid_id)
  
} else if(param == "pa_points"){
    vect <- read_sf("data/spatial_data/grid/empty_points_pas.gpkg") %>% 
      st_buffer(dist = 56.42)
    
} else if(param == "steps_1hr"){
  vect <- fread("data/processed_data/data_fragments/steps_1hr_incl_random.csv") %>% 
    mutate(x = x2_, 
           y = y2_) %>% 
    filter(!is.na(x)) %>% 
    st_as_sf(coords = c("x", "y"), 
             crs = "ESRI:54009") %>% 
    st_buffer(dist = 10)
  
} else if(param == "steps_3hrs"){
  vect <- fread("data/processed_data/data_fragments/steps_3hrs_incl_random.csv") %>% 
    mutate(x = x2_, 
           y = y2_) %>% 
    filter(!is.na(x)) %>% 
    st_as_sf(coords = c("x", "y"), 
             crs = "ESRI:54009") %>% 
    st_buffer(dist = 10)
  
} else if(param == "steps_12hrs"){
  vect <- fread("data/processed_data/data_fragments/steps_12hrs_incl_random.csv") %>% 
    mutate(x = x2_, 
           y = y2_) %>% 
    filter(!is.na(x)) %>% 
    st_as_sf(coords = c("x", "y"), 
             crs = "ESRI:54009") %>% 
    st_buffer(dist = 10)
}  else if(param == "steps_24hrs"){
  vect <- fread("data/processed_data/data_fragments/steps_24hrs_incl_random.csv") %>% 
    mutate(x = x2_, 
           y = y2_) %>% 
    filter(!is.na(x)) %>% 
    st_as_sf(coords = c("x", "y"), 
             crs = "ESRI:54009") %>% 
    st_buffer(dist = 10)
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


# eco regions legegend 

dt_eco <- st_read("data/spatial_data/covariates/vector/ecoregions/Ecoregions2017.shp")  %>% 
  as.data.frame() %>% 
  mutate(geometry = NULL)


eco_leg <- dt_eco %>% 
  dplyr::select(ecoregion_num = ECO_ID, ecoregion = ECO_NAME) %>% 
  arrange(ecoregion_num) %>%
  unique()

res_biome_leg = dt_eco %>% 
  dplyr::select(resolve_biome_num = BIOME_NUM, resolve_biome = BIOME_NAME) %>% 
  arrange(resolve_biome_num) %>%
  unique()

#### EXTRACT CONTINUOUS COVS #### ----------------------------------
col_names <- c(
  
  #### Continuous ####
  "elevation", ## Elevation
  "map", ## MAP
  "mat", ## MAT
  "mean_spei", #average standardized precipitation evapotranspiration index
  "months_severe_drought", #n months with SPEI <-1.6 between 2000 and 2023
  "months_extreme_drought", #n months with SPEI <-2 between 2000 and 2023
  
  "slope", #Slope
  "enerscape", #Energy landscape 
  
  "distance_to_water_km", # Distance to water
  "distance_to_settlement_km", # Distance to settlement 
  
#  "water_fraction", #Fraction of water
#  "settlement_fraction", #Fraction of settlement 

  "n_deposition", ## Nitrogen depo
  "human_modification", #Human modification index
  
  "evi_mean", #Average EVI
 # "evi_dry_season", #Average EVI in the dry season
 # "evi_wet_season", #Average EVI in the wet season

  "fire_frequency", #average number of fires per year

  "dw_min_min_mode_fraction", #minimum fractzio of the mode (dynamic world) 
  "dw_min_median_mode_fraction", #minimum fractzio of the mode (dynamic world) 
  "dw_median_median_mode_fraction", #minimum fractzio of the mode (dynamic world) 


  #### categorical ####
  "functional_biome_num", # functional biome 
  "olson_biome_num", # olson biome 
  "land_cover_num", #esa landcover
  "ecoregion_num",  #resolve eco region
  "resolve_biome_num"

)

cov_paths <- c(
 
   #### Continuous ####
  "data/spatial_data/covariates/raster/nasa_dem_90m.tif", ## Elevation
  "data/spatial_data/covariates/raster/CHELSA_bio12_1981-2010_V.2.1.tif", ## MAP
  "data/spatial_data/covariates/raster/CHELSA_bio1_1981-2010_V.2.1.tif", ## MAT
  
  "data/spatial_data/covariates/raster/mean_spei_2000_2023.tif", #average standardized precipitation evapotranspiration index
  "data/spatial_data/covariates/raster/spei_months_severe_drought_2000_2023.tif", #n months with SPEI <-1.6 between 2000 and 2023
  "data/spatial_data/covariates/raster/spei_months_extreme_drought_2000_2023.tif", #n months with SPEI <-2 between 2000 and 2023
  
  "data/spatial_data/covariates/raster/slope_degree.tif", #Slope
  "data/spatial_data/covariates/raster/energyscape_kcal.tif", #Energy landscape 
  
  "data/spatial_data/covariates/raster/distance_to_water_km.tif", # Distance to water
  "data/spatial_data/covariates/raster/distance_to_settlement_km.tif", # Distance to settlement 
  
#  "data/spatial_data/covariates/raster/esa_wc_water_2021_10m.tif", #Fraction of water
#  "data/spatial_data/covariates/raster/world_settlement_footprint_2015_10m.tif", #Fraction of settlement 
  
  "data/spatial_data/covariates/raster/total_N_dep.tif", ## Nitrogen depo
  "data/spatial_data/covariates/raster/human_land_modification_lulc_kennedy.tif", #Human modification index
  
  "data/spatial_data/covariates/raster/hls_clamped_mean_evi_2013_2025_90m.tif", #Average EVI
 # "data/spatial_data/covariates/raster/dry_season_mean_evi_2001_2024.tif", #Average EVI in the dry season
 # "data/spatial_data/covariates/raster/wet_season_mean_evi_2001_2024.tif", #Average EVI in the wet season
  
  "data/spatial_data/covariates/raster/fire_frequency_500m_2001_2024.tif", #average number of fires per year

  "data/spatial_data/covariates/raster/dw_min_mode_fraction_100m.tif", #min mode fraciton 
  "data/spatial_data/covariates/raster/dw_min_median_mode_fraction_100m.tif", #min median fraction
  "data/spatial_data/covariates/raster/dw_median_median_mode_fraction_100m.tif", #median median fraction


  #### categorical ####
  "data/spatial_data/covariates/raster/higgins_functional_biomes.tif", # functional biome 
  "data/spatial_data/covariates/raster/wwf_olson_biome.tif", # olson biome 
  "data/spatial_data/covariates/raster/esa_world_cover_2021_10m.tif", #esa landcover
  "data/spatial_data/covariates/raster/resolve_eco_regions_1km.tif", #resolve ecoregions
  "data/spatial_data/covariates/raster/resolve_biomes_1km.tif"# resolve biomes 
)


funcs <- c(rep("mean", 17), rep("mode", 5))

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
vect_backup <- vect
vect <- vect_backup
#vect <- vect %>% sample_n(1000000)



################################## LOOOOOOOOOOOOP ############################            
options(future.globals.maxSize = 10 * 1024^3)  # 10 GB
plan(multisession, workers = 22)
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
                    
                    cov_r <- rast(covs[i, ]$cov_path)
                    
                    vect_t <- st_transform(vect_chunk, crs = st_crs(cov_r))
                    
                    all_polys <- vect_chunk %>% dplyr::select(unique_id)
                    
                    func <- covs[i, ]$func
                    
                    
                    extr <- exactextractr::exact_extract(cov_r, 
                                                         vect_t, 
                                                         append_cols = c("unique_id"),
                                                         fun = func)
                    
                    setnames(extr, func, covs[i, ]$col_name)
                    
                    dt_extr <- all_polys %>%
                      left_join(extr %>% unique()) %>%
                      as.data.table() %>%
                      mutate(geom = NULL, x = NULL, geometry = NULL) %>% 
                      unique()
                    
                    print(paste0(covs[i, ]$col_name,"; i = ", i))
                    
                    return(dt_extr)
                    gc()
                    
  }
)

  chunk_dt <- dt_covs_list %>%
    reduce(~ left_join(.x, .y, by = "unique_id"))
  
  all_dt_covs_list[[as.character(chunk)]] <- chunk_dt
  
  gc()
}
toc()

dt_covs <- rbindlist(all_dt_covs_list)


## Combine -------------------

dt_vect_covs <- vect_covs_raw %>% 
  left_join(dt_covs) %>% 
  left_join(biome_leg) %>%
  left_join(fun_biome_leg) %>% 
  left_join(lc_leg) %>% 
  left_join(eco_leg) %>% 
  left_join(res_biome_leg) %>% 
  as.data.table() %>% 
  mutate(x = NULL, 
         geom = NULL) %>% 
  dplyr::select(-functional_biome_num, -olson_biome_num, -land_cover_num, -ecoregion_num, - resolve_biome_num)


summary(dt_vect_covs)
          



# library(ggcorrplot)
# corr <- round(cor(dt_vect_covs  %>% 
#                     dplyr::select(-unique_id, -functional_biome, -olson_biome) %>% 
#                     mutate(grid_id = NULL) %>% 
#                     filter(complete.cases(.))), 1)
# ggcorrplot(corr, hc.order = TRUE, type = "lower",
#            lab = TRUE)


vect_covs <- dt_vect_covs %>% left_join(vect) %>% st_as_sf

#mapview::mapview(vect_covs %>% sample_n(10000), zcol = "distance_to_water_km")

if(param == "knp_elephants"){
  
  fwrite(dt_vect_covs %>% 
           mutate(unique_id = NULL), "data/processed_data/data_fragments/knp_elephant_counts_habitat_covariates.csv")

} else if(param == "pa_grid_100m"){
  
  fwrite(dt_vect_covs %>% 
           mutate(unique_id = NULL), "data/processed_data/data_fragments/pa_grids_100m_with_covariates.csv")
  
} else if(param == "pa_grid_1000m"){
  
  fwrite(dt_vect_covs %>% 
           mutate(unique_id = NULL), "data/processed_data/data_fragments/pa_grids_1000m_with_covariates.csv")
  
} else if(param == "pa_points"){
  
  fwrite(dt_vect_covs, "data/processed_data/data_fragments/pa_points_with_covariates.csv")
  
} else if(param == "steps_1hr"){
  
  fwrite(dt_vect_covs, "data/processed_data/data_fragments/steps_1hr_habitat_covariates.csv")
  
} else if(param == "steps_3hrs"){
  
  fwrite(dt_vect_covs, "data/processed_data/data_fragments/steps_3hrs_habitat_covariates.csv")
  
} else if(param == "steps_12hrs"){
  
  fwrite(dt_vect_covs, "data/processed_data/data_fragments/steps_12hrs_habitat_covariates.csv")
  
} else if(param == "steps_24hrs"){
  
  fwrite(dt_vect_covs, "data/processed_data/data_fragments/steps_24hrs_habitat_covariates.csv")
  
} 


}
