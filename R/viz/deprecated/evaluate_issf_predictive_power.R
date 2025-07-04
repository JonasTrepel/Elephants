#evaluate model fit 

library(data.table)
library(tidyverse)
library(patchwork)


dt <- fread("builds/model_outputs/habitat_selection_cross_validation.csv") %>% 
  pivot_longer(cols = c("r_sq_adj", "lm_estimate", "lm_p", "cor"), 
               names_to = "var_name", values_to = "var_value") %>% 
  mutate(var_value = round(var_value, 4))


means_df <- dt %>% 
  group_by(var_name) %>% 
  summarize(mean_val = mean(var_value, na.rm = TRUE), .groups = "drop")

medians_df <- dt %>% 
  group_by(var_name) %>% 
  summarize(median_val = median(var_value, na.rm = TRUE), .groups = "drop")

p1 <- dt %>% 
  ggplot(aes(x = var_value)) +
  geom_histogram() +
  geom_vline(data = means_df, aes(xintercept = mean_val), color = "red") +
  geom_vline(data = medians_df, aes(xintercept = median_val), color = "red", linetype = "dashed") +
  facet_wrap(~var_name, scales = "free")

p1

p2 <- dt %>% 
  ggplot(aes(x = var_value)) +
  geom_histogram(aes(color = sex, fill = sex), alpha = 0.1) +
  facet_wrap(~var_name, scales = "free")

p2

p <- (p1) | (p2)
ggsave(plot = p, "builds/plots/exploratory/habitat_quality_selection.png", 
       dpi = 600, width = 10, height = 5)
