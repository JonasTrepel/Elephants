library(terra)
library(remotePARTS)
library(data.table)
library(tidyverse)


mat_files <- list.files("data/spatial_data/time_series", pattern = "mat_", full.names = T)


for(i in 1:length(mat_files)){
  
  r <- rast(mat_files[i])
  
  dt_tmp <- as.data.frame(r, xy = T)
  dt_tmp <- setnames(dt_tmp, old = names(dt_tmp), new = c("x", "y", paste0("mat", i)))
  
  if(i == 1){
    dt_mat <- dt_tmp
  } else {
    dt_mat <- left_join(dt_mat, dt_tmp)
  }
  print(i)
}


dt_mat_sub <- dt_mat %>% 
  filter(complete.cases(.)) %>% as.data.frame()

Y_mat <- as.matrix(dt_mat_sub %>% dplyr::select(contains("mat")))
coords_mat <- as.matrix(dt_mat_sub[, c("x", "y")])

ar_results_mat <- fitAR_map(Y = Y_mat, coords = coords_mat)

dt_mat_sub$mat_coef <- coefficients(ar_results_mat)[, "t"] 
dt_mat_sub$mat_p_val <- ar_results_mat$pvals[, 2]


dt_mat_res <- dt_mat %>% 
  left_join(dt_mat_sub) %>% 
  dplyr::select(x, y, mat_coef)

r_mat_coef <- rast(dt_mat_res, type = "xyz")
crs(r_mat_coef) <- crs(rast(mat_files[1]))
plot(r_mat_coef)
writeRaster(r_mat_coef, "data/spatial_data/covariates/raster/era5_mat_coef.tif", overwrite = T)

library(scico)
p_mat <- dt_mat_res %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = mat_coef)) +
  scale_fill_scico(palette = "vikO", midpoint = 0, direction = 1) +
  labs(fill = "MAT trend") +
  theme_void()
p_mat
ggsave(plot = p_mat, "builds/plots/supplement/mat_trend_for_rob.png", dpi = 600)
