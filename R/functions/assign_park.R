
assign_park <- function(polygon = NA, pas = NA, ind = NA){

sf_use_s2(FALSE)
  
overlap_list <- st_intersects(polygon, pas, sparse = TRUE)
v <- overlap_list[[1]]

pas_sub <- pas[v, ] %>% 
  st_make_valid()

if(nrow(pas_sub) > 0){

  is <- st_intersection(pas_sub, polygon)

is_trans <- st_transform(is, crs = "ESRI:54009")

is_trans$is_area_km2 <- as.numeric(st_area(st_make_valid(is_trans))/1000000)

res <- is_trans %>% 
  slice_max(is_area_km2) %>% 
  dplyr::select(park_id = NAME, wdpa_pid = WDPA_PID, is_area_km2) %>% 
  as.data.table() %>% 
  mutate(geom = NULL, 
         hr_area_km2 = as.numeric(st_area(st_make_valid(polygon))/1000000), 
         overlap_percent = round((is_area_km2/hr_area_km2)*100, 2), 
         individual_id = ind) 

sf_use_s2(TRUE)

return(res)
} else {
  return(data.frame(park_id = NA, 
                    wdpa_pid = NA,
                  hr_area_km2 = as.numeric(st_area(st_make_valid(polygon))/1000000), 
                  overlap_percent = 0, 
                  is_area_km2 = 0,
                  individual_id = ind))
}



}

