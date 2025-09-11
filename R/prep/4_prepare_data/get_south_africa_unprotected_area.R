#get elephant nogo areas 
#(e.g., outside of PAs in South Africa)

library(tidyverse)
library(data.table)
library(sf)
library(rnaturalearth)
library(mapview)

sf_pa <- st_read("data/spatial_data/protected_areas/sa_pas/SAPAD_OR_2025_Q1.shp")
mapview(sf_pa)

sf_sa <- ne_countries(country = "South Africa", scale = "large", returnclass = "sf") %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  dplyr::slice_max(order_by = st_area(geometry), n = 1)

st_crs(sf_sa) == st_crs(sf_pa)


sf_use_s2(FALSE)
sf_pa_union <- st_union(sf_pa)
sa_unprotected <- st_difference(sf_sa, sf_pa_union)
mapview(sa_unprotected)


st_write(sa_unprotected, "data/spatial_data/protected_areas/south_africa_unprotected.gpkg", append = F)
