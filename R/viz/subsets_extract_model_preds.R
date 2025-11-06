library(data.table)
library(patchwork)
library(tidyverse)
library(sdmTMB)
library(patchwork)
library(groupdata2)
library(future)
library(furrr)
library(ggeffects)



#### subsets ----------

dt_bm_subset <- fread("builds/model_outputs/subset_results_1000m_local_density.csv") %>% 
  mutate(clean_response = factor(clean_response, levels = c(
    "Woody Cover Trend", "Canopy Height Trend", "EVI Trend"))) %>% 
  dplyr::select(tier, model_id, model_path, response, dev_explained_full, dev_explained_var) %>% 
  unique()



#### Extract variable specific predictions -----

vars <- c("local_density_km2_scaled",
          "months_extreme_drought_scaled",
          "fire_frequency_scaled", 
          "mat_coef_scaled", 
          "n_deposition_scaled")

tiers <- unique(dt_bm_subset$tier)

extr_guide <- CJ(var = vars, 
                 tier = tiers) %>% 
  left_join(dt_bm_subset)

plan(multisession, workers = 15)

for_results_pred <- future_map(
  1:nrow(extr_guide),
  .progress = TRUE,
  .options = furrr_options(seed = TRUE),
  function(i) {
    
    response <- extr_guide[i, ]$response
    tier <- unique(extr_guide[i, ]$tier)
    var <- unique(extr_guide[i, ]$var)
    
    
    m <- readRDS(extr_guide[i, ]$model_path)
    dat <- m$data
    
    
    var_us <- gsub("_scaled", "", var)
    mean_x <- mean(dat[[var_us]], na.rm = TRUE)
    sd_x   <- sd(dat[[var_us]], na.rm = TRUE)
    
    term_call <- paste0(var, " [all]")
    
    m_plot <- ggeffects::ggpredict(m, terms = term_call)
    
    plot_data <- as.data.table(m_plot) %>%
      mutate(
        x_unscaled = round(x * sd_x + mean_x, 3),
        var_name = var,
        response_name = response,
        tier = tier,
        dev_explained_full = extr_guide[i,]$dev_explained_full,
        dev_explained_var = extr_guide[i,]$dev_explained_var,
        n = nrow(dat)
      )
    
    # Ensure confidence interval columns exist
    if (!any(grepl("conf", names(plot_data)))) {
      plot_data <- plot_data %>%
        mutate(conf.low = NA,
               conf.high = NA,
               std.error = NA)
    }
    
    
    #print(paste0(response, " done at: ", Sys.time()))
    
    # Clean up that mess
    rm(m)
    gc()
    
    return(plot_data)
  }
)

plan(sequential)
print(paste0("subsetp done ", Sys.time()))

dt_pred_comp <- rbindlist(for_results_pred) %>% 
  mutate(scale = "km2", 
         response_clean = case_when(
           response_name == "canopy_height_900m_coef" ~ "Canopy Height Trend",
           response_name == "tree_cover_1000m_coef" ~ "Tree Cover Trend",
           response_name == "evi_900m_coef" ~ "EVI Trend"
         ),
         var_clean = case_when(
           var_name == "local_density_km2_scaled" ~ "Local Elephant Density",
           var_name == "months_extreme_drought_scaled" ~ "N Drought Months",
           var_name == "fire_frequency_scaled" ~ "Fire Frequency",
           var_name == "mat_coef_scaled" ~ "Temperature Change",
           var_name == "n_deposition_scaled" ~ "N Deposition",
         ))
unique(dt_pred_comp$response_name)
unique(dt_pred_comp$var_name)

fwrite(dt_pred_comp, "builds/model_outputs/subset_predictions_1000m.csv")

###### Plot - 1 facte_grid for each response, comparing different tiers? 


p_smooth <- dt_pred_comp %>% 
  filter(!excluded_park %in% c("No Park")) %>% 
  ggplot() +
  # geom_point(data = dt_long, aes(x = var_value, y = response_value), alpha = 0.1, size = 0.1, color = "grey25") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high, fill = excluded_park ), alpha = 0.1) +
  geom_line(aes(x = x_unscaled, y = predicted, color = excluded_park), linewidth = 1) +
  geom_ribbon(data = dt_pred_comp %>% 
                filter(excluded_park %in% c("No Park")), 
              aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.1, fill = "black") +
  geom_line(data = dt_pred_comp %>% 
              filter(excluded_park %in% c("No Park")),
            aes(x = x_unscaled, y = predicted, color = excluded_park), color = "black", linewidth = 1) +
  facet_grid(rows = vars(response_clean), cols = vars(var_clean), scales = "free") +
  labs(y = "Response Value", title = "", x = "Predictor Value",
       color = "Excluded Park", fill = "Excluded Park") +
  scico::scale_color_scico_d(begin = .1, end = .9, palette = "batlow") +
  scico::scale_fill_scico_d(begin = .1, end = .9, palette = "batlow") +
  theme_bw() +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_smooth

ggsave(plot = p_smooth, "builds/plots/supplement_subset_1000m_model_predictions.png", dpi = 900, height = 6, width = 9)
