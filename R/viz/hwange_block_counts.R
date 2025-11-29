library(data.table)
library(tidyverse)
library(sf)
library(mapview)

dt <- fread("data/processed_data/clean_data/analysis_ready_grid_1000m.csv") %>% 
  mutate(tree_cover_1000m_coef = tree_cover_1000m_coef*100, 
  ) %>% 
  filter(park_id == "Hwange")


#https://www.nature.com/articles/s41598-025-19902-x/figures/1
sf_b <- st_read("data/spatial_data/protected_areas/hwange_blocks/BLOCKS_UTM.shp") %>% 
  mutate(num_id = 1:nrow(.), 
         block_name = case_when(
           num_id == 1 ~ "Robins", 
           num_id == 2 ~ "Sinamatella", 
           num_id == 3 ~ "Dandari", 
           num_id == 4 ~ "Mtoa", 
           num_id == 5 ~ "Shapi", 
           num_id == 6 ~ "Shakwanki", 
           num_id == 7 ~ "Dzivanini", 
           num_id == 8 ~ "Central", 
           num_id == 9 ~ "Ngamo", 
           num_id == 10 ~ "Main Camp"), 
         ele_count_2022 = case_when(
           block_name == "Robins" ~ 4762, 
           block_name == "Sinamatella" ~ 5340, 
           block_name == "Dandari" ~ 4318, 
           block_name == "Mtoa" ~ 2055, 
           block_name == "Shapi" ~ 6281, 
           block_name == "Shakwanki" ~ 1080, 
           block_name == "Dzivanini" ~ 7126, 
           block_name == "Central" ~ 1991, 
           block_name == "Ngamo" ~ 5527, 
           block_name == "Main Camp" ~ 3740
         ))


mapview(sf_b, zcol = "ele_count_2022")


sf_hw = dt %>% 
  st_as_sf(coords = c("x_mollweide", "y_mollweide"), 
           crs = "ESRI:54009") %>% 
  st_transform(., crs = st_crs(sf_b)) %>% 
  st_join(sf_b)


block_densities = sf_hw %>% 
  as.data.table() %>% 
  mutate(geometry = NULL) %>% 
  group_by(block_name ) %>% 
  summarise(predicted_block_count = sum(local_density_km2)) %>% 
  filter(!is.na(block_name))


sf_b2 = sf_b %>% 
  left_join(block_densities)
  
library(scico)
p_map = sf_b2 %>% 
  pivot_longer(cols = c("ele_count_2022", "predicted_block_count"), 
               names_to = "type", values_to = "numbers") %>% 
  mutate(type = case_when(
    type == "ele_count_2022" ~ "Count (2022 Dry Season)", 
    type == "predicted_block_count" ~ "Predicted"
  )) %>% 
  ggplot() +
  geom_sf(aes(fill = numbers)) + 
  scale_fill_scico(palette = "batlow", end = .8, begin = 0.2) +
  labs(fill = "N Elephants", color = "N Elephants") +
  theme_void() +
  facet_wrap(~type) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_map

p_bar = sf_b2 %>% 
  pivot_longer(cols = c("ele_count_2022", "predicted_block_count"), 
               names_to = "type", values_to = "numbers") %>% 
  mutate(type = case_when(
    type == "ele_count_2022" ~ "Count", 
    type == "predicted_block_count" ~ "Predicted"
  )) %>% 
  ggplot() +
  geom_col(aes(x = block_name, y = numbers, fill = type, group = type),
           position = "dodge") +
  scale_fill_scico_d(palette = "batlow", end = .7, begin = 0.3) +
  labs(x = "Block", y = "Elephant Numbers") +
  coord_flip() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
      #  axis.text.x = element_text(angle = 45), 
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_bar

p_cor = sf_b2 %>% 
  rename(Count = ele_count_2022, 
         Predicted = predicted_block_count) %>%
  ggplot() +
  geom_point(aes(x = Count, y = Predicted), size = 3) +
  scale_fill_scico_d(palette = "batlow", end = .7, begin = 0.3) +
  geom_abline(linetype = "dashed") +
  coord_equal() +
  labs(x = "Predicted", y = "Count (2022 Dry Season)") +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        #  axis.text.x = element_text(angle = 45), 
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_cor


p_hw = (p_map / (p_bar | p_cor)) + plot_annotation(tag_level = "A") +
  plot_layout(heights = c(1, 1.5))
p_hw
ggsave(plot = p_hw, "builds/plots/supplement/hwange_elephant_count_vs_predicted.png", 
       dpi = 900, height = 7, width = 8)
