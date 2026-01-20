library(data.table)
library(tidyverse)
library(ggrepel)
library(scico)

dt_ele <- fread("data/processed_data/clean_data/elephant_id_meta_data.csv")


dt_est <- fread("builds/model_outputs/issf_estimates_24hr_steps.csv") %>% 
  filter(season == "whole_year") %>% 
  as.data.table()


#check median differences -----------------
unique(dt_est$term)
dt_m <- dt_est %>% 
  filter(sex == "M")

dt_f <- dt_est %>% 
  filter(sex == "F") %>% 
  as.data.table()

median(dt_m[term == "evi_mean",]$estimate)
median(dt_f[term == "evi_mean",]$estimate)

median(dt_m[term == "slope",]$estimate)
median(dt_f[term == "slope",]$estimate)

median(dt_m[term == "distance_to_water_km",]$estimate)
median(dt_f[term == "distance_to_water_km",]$estimate)

median(dt_m[term == "distance_to_settlement_km",]$estimate)
median(dt_f[term == "distance_to_settlement_km",]$estimate)

median(dt_m[term == "human_modification",]$estimate)
median(dt_f[term == "human_modification",]$estimate)

#check environmental differences -----------

# Variable mean in homerange ------

summary(m_vm_evi <- lm(estimate ~ evi_mean_mean, dt_est[term == "evi_mean"]))
summary(m_vm_sl <- lm(estimate ~ slope_mean, dt_est[term == "slope"]))
summary(m_vm_dw <- lm(estimate ~ distance_to_water_km_mean, dt_est[term == "distance_to_water_km"]))
summary(m_vm_ds <- lm(estimate ~ distance_to_settlement_km_mean, dt_est[term == "distance_to_settlement_km"]))
summary(m_vm_hm <- lm(estimate ~ human_modification_mean, dt_est[term == "human_modification"]))

# Variable range in homerange
summary(m_vr_evi <- lm(estimate ~ evi_mean_range, dt_est[term == "evi_mean"]))
summary(m_vr_sl <- lm(estimate ~ slope_range, dt_est[term == "slope"]))
summary(m_vr_dw <- lm(estimate ~ distance_to_water_km_range, dt_est[term == "distance_to_water_km"]))
summary(m_vr_ds <- lm(estimate ~ distance_to_settlement_km_range, dt_est[term == "distance_to_settlement_km"]))
summary(m_vr_hm <- lm(estimate ~ human_modification_range, dt_est[term == "human_modification"]))



#### Estimates vs context ----------------------
p_est_tm <- dt_est %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ), 
  term_mean = case_when(
    term == "evi_mean" ~ evi_mean_mean,
    term == "distance_to_water_km" ~ distance_to_water_km_mean,
    term == "distance_to_settlement_km" ~ distance_to_settlement_km_mean,
    term == "human_modification" ~ human_modification_mean,
    #   term == "enerscape" ~ enerscape_mean,
    term == "slope" ~ slope_mean
  )) %>% 
  ggplot() + 
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = term_mean, y = estimate), size = 0.75, alpha = 0.25, color = "ivory4") +
  geom_smooth(aes(x = term_mean, y = estimate), method = "lm", color = "black", fill = "black") +
  facet_wrap(~clean_term, scales = "free", ncol = 5) +
  labs(x = "Variable Mean", y = "Estimate", subtitle = "Estimate ~ Variable Mean") +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_est_tm

p_est_tr <- dt_est %>% 
  mutate(clean_term = case_when(
    .default = term,
    term == "evi_mean" ~ "EVI",
    term == "distance_to_water_km" ~ "Distance to Water",
    term == "distance_to_settlement_km" ~ "Distance to Settlement",
    term == "human_modification" ~ "Human Modification Index",
    term == "enerscape" ~ "Energy Landscape",
    term == "slope" ~ "Slope",
  ), 
  term_range = case_when(
    term == "evi_mean" ~ evi_mean_range,
    term == "distance_to_water_km" ~ distance_to_water_km_range,
    term == "distance_to_settlement_km" ~ distance_to_settlement_km_range,
    term == "human_modification" ~ human_modification_range,
    #   term == "enerscape" ~ enerscape_range,
    term == "slope" ~ slope_range
  )) %>% 
  ggplot() + 
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = term_range, y = estimate), size = 0.75, alpha = 0.25, color = "ivory4") +
  geom_smooth(aes(x = term_range, y = estimate), method = "lm", color = "black", fill = "black") +
  facet_wrap(~clean_term, scales = "free", ncol = 5) +
  labs(x = "Variable Range", y = "Estimate", subtitle = "Estimate ~ Variable Range") +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))


p_est_tr

library(patchwork)
p_evc <- p_est_tm / p_est_tr
p_evc
ggsave(plot = p_evc, "builds/plots/supplement/estimates_vs_var_range_and_mean.png", 
       dpi = 600, height = 5, width = 10)
unique(dt_est$cluster_id)

########## Cluster PCAs ------------

dt_pca_mean <- dt_est %>%
  ungroup() %>% 
  left_join(dt_ele) %>% 
  select(individual_id, cluster_id, evi_mean_mean, distance_to_water_km_mean, distance_to_settlement_km_mean, human_modification_mean, slope_mean) %>%
  filter(complete.cases(.)) %>% 
  unique() %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>% 
  rename(EVI = evi_mean_mean, 
         `Dist. to Water` = distance_to_water_km_mean, 
         `Dist. to Settlement` = distance_to_settlement_km_mean, 
         `Human Modification` = human_modification_mean, 
         `Slope` = slope_mean) %>%
  mutate(cluster_id = case_when(
    .default = cluster_id, 
    !cluster_id %in% c("chobe", "kzn", "limpopo", "luangwa") ~ "Not assigned", 
    cluster_id == "limpopo" ~ "GL & GM", 
    cluster_id == "kzn" ~ "Lebombo", 
    cluster_id == "luangwa" ~ "MAZA", 
    cluster_id == "chobe" ~ "KAZA"
  ))
pr_mean <- princomp(dt_pca_mean %>% select(-individual_id, -cluster_id))

dt_pca_mean$pc1 <- pr_mean$scores[,1]
dt_pca_mean$pc2 <- pr_mean$scores[,2]

#viz 
loadings_mean <- as_tibble(pr_mean$loadings[, 1:2], rownames = "Variable")
scores_mean <- as_tibble(pr_mean$scores[, 1:2]) %>%
  mutate(individual_id = dt_pca_mean$individual_id, 
         cluster_id = dt_pca_mean$cluster_id) 

#importance 
#https://stackoverflow.com/questions/62422277/how-to-obtain-principal-component-variance-explained-in-r-prcomp-and-prepro
pr_mean_sum <- summary(pr_mean)
pr_mean_sum
#Alternative (same results)
eigenvalues_mean <- pr_mean$sdev^2
variance_explained_mean <- eigenvalues_mean / sum(eigenvalues_mean)
(percent_var_mean <- round(variance_explained_mean[1:2] * 100, 1))


p_pca_mean <- ggplot() +
  geom_point(data = scores_mean, aes(x = Comp.1, y = Comp.2, color = cluster_id),  size = 1, alpha = 0.75) +
  stat_ellipse(data = scores_mean %>% 
                 filter(cluster_id != "Not assigned"),
               aes(x = Comp.1, y = Comp.2, color = cluster_id),
               type = "t", level = 0.9, linewidth = 0.8, alpha = 0.8) +
  geom_segment(data = loadings_mean, aes(x = 0, y = 0, xend = Comp.1*5, yend = Comp.2*5),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
   geom_text_repel(data = loadings_mean, aes(x = Comp.1*5, y = Comp.2*5, label = Variable),
                   size = 4, color = "black") +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(color = "Cluster",
       title = "Variable Mean",
       x = paste0("PC1 (", percent_var_mean[1], "% variance)"),
       y = paste0("PC2 (", percent_var_mean[2], "% variance)")) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_pca_mean

dt_pca_range <- dt_est %>%
  ungroup() %>% 
  left_join(dt_ele) %>% 
  select(individual_id, cluster_id, evi_mean_range, distance_to_water_km_range, distance_to_settlement_km_range,
         human_modification_range, slope_range) %>%
  filter(complete.cases(.)) %>% 
  unique() %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>% 
  rename(EVI = evi_mean_range, 
         `Dist. to Water` = distance_to_water_km_range, 
         `Dist. to Settlement` = distance_to_settlement_km_range, 
         `Human Modification` = human_modification_range, 
         `Slope` = slope_range) %>%
  mutate(cluster_id = case_when(
    .default = cluster_id, 
    !cluster_id %in% c("chobe", "kzn", "limpopo", "luangwa") ~ "Not assigned",
    cluster_id == "limpopo" ~ "GL & GM", 
    cluster_id == "kzn" ~ "Lebombo", 
    cluster_id == "luangwa" ~ "MAZA", 
    cluster_id == "chobe" ~ "KAZA"
  ))
pr_range <- princomp(dt_pca_range %>% select(-individual_id, -cluster_id))

dt_pca_range$pc1 <- pr_range$scores[,1]
dt_pca_range$pc2 <- pr_range$scores[,2]

#viz 
loadings_range <- as_tibble(pr_range$loadings[, 1:2], rownames = "Variable")
scores_range <- as_tibble(pr_range$scores[, 1:2]) %>%
  mutate(individual_id = dt_pca_range$individual_id, 
         cluster_id = dt_pca_range$cluster_id) 

#importance 
#https://stackoverflow.com/questions/62422277/how-to-obtain-principal-component-variance-explained-in-r-prcomp-and-prepro
pr_range_sum <- summary(pr_range)
pr_range_sum
#Alternative (same results)
eigenvalues_range <- pr_range$sdev^2
variance_explained_range <- eigenvalues_range / sum(eigenvalues_range)
(percent_var_range <- round(variance_explained_range[1:2] * 100, 1))


p_pca_range <- ggplot() +
  geom_point(data = scores_range, aes(x = Comp.1, y = Comp.2, color = cluster_id),  size = 1, alpha = 0.75) +
  stat_ellipse(data = scores_range %>% 
                 filter(cluster_id != "Not assigned"),
               aes(x = Comp.1, y = Comp.2, color = cluster_id),
               type = "t", level = 0.9, linewidth = 0.8, alpha = 0.8) +
  geom_segment(data = loadings_range, aes(x = 0, y = 0, xend = Comp.1*5, yend = Comp.2*5),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text_repel(data = loadings_range, aes(x = Comp.1*5, y = Comp.2*5, label = Variable),
                  size = 4, color = "black") +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(color = "Cluster",
       title = "Variable Range",
       x = paste0("PC1 (", percent_var_range[1], "% variance)"),
       y = paste0("PC2 (", percent_var_range[2], "% variance)")) +
  theme_minimal() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_pca_range

library(patchwork)
p_pca <- (p_pca_mean | p_pca_range)

ggsave(plot = p_pca, "builds/plots/supplement/cluster_pca.png", dpi = 600, 
       height = 4, width = 8)


#### Show distribution 

dt_mean <- dt_est %>%
  ungroup() %>% 
  left_join(dt_ele) %>% 
  select(individual_id, cluster_id, evi_mean_mean, distance_to_water_km_mean, distance_to_settlement_km_mean, human_modification_mean, slope_mean) %>%
  filter(complete.cases(.)) %>% 
  unique() %>% 
  rename(EVI = evi_mean_mean, 
         `Dist. to Water` = distance_to_water_km_mean, 
         `Dist. to Settlement` = distance_to_settlement_km_mean, 
         `Human Modification` = human_modification_mean, 
         `Slope` = slope_mean) %>%
  mutate(cluster_id = case_when(
    .default = cluster_id, 
    !cluster_id %in% c("chobe", "kzn", "limpopo", "luangwa") ~ "Not assigned", 
    cluster_id == "limpopo" ~ "GL & GM", 
    cluster_id == "kzn" ~ "Lebombo", 
    cluster_id == "luangwa" ~ "MAZA", 
    cluster_id == "chobe" ~ "KAZA"
  ))


library(ggridges)
p_mean = dt_mean %>% 
  #filter(cluster_id != "Not assigned") %>% 
  pivot_longer(cols = c("EVI", "Dist. to Water", "Dist. to Settlement", 
                        "Human Modification", "Slope"), 
               names_to = "var_name", values_to = "var_value") %>% 
  ggplot(aes(x = var_value, y = cluster_id)) +
  facet_wrap(~var_name, scales = "free_x", ncol = 5) +
  geom_density_ridges(aes(fill = cluster_id, color = cluster_id)) +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  theme_bw() +
  labs(x = "", y = "")+
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow", color = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))
p_mean

ggsave(plot = p_mean, "builds/plots/supplement/var_distibution_clusters.png", 
       dpi = 900, height = 3, width = 12) 

library(patchwork)
p_pca <- (p_pca_mean | p_pca_range)

p_comb = (p_pca / p_mean) + plot_layout(heights = c(1.8, 1)) + plot_annotation(tag_levels = "A")
p_comb
ggsave(plot = p_comb, "builds/plots/supplement/cluster_pca_and_ridges.png", dpi = 600, 
       height = 6.5, width = 9)

  