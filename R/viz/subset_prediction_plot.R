library(tidyverse)
library(data.table)
library(tidylog)
library(ggcorrplot)
library(patchwork)
library("sdmTMB")
library(rnaturalearth)
library(future)
library(ggspatial)
library(groupdata2)
library(GGally)
library(scico)
library(sf)
#first sfuture#first stab at sdmTMB


#1 HOUSEKEEPING -------------------------------------

dt_pred <- fread("builds/model_outputs/subset_predictions_1000m.csv")  %>% 
  filter(tier_clean %in% c("Lower Third", "Middle Third", "Upper Third"))


dt_dev <- dt_pred %>% 
  dplyr::select(dev_explained_full, dev_explained_var, tier_clean, response_clean) %>% 
  unique()

####### Get rectangles ready: 
dt_rect_ch_lt <- dt_pred %>%
  filter(tier_clean %in% c("Lower Third") & response_clean %in% c("Canopy Height Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_ch_mt <- dt_pred %>%
  filter(tier_clean %in% c("Middle Third") & response_clean %in% c("Canopy Height Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_ch_ut <- dt_pred %>%
  filter(tier_clean %in% c("Upper Third") & response_clean %in% c("Canopy Height Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()


dt_rect_tc_lt <- dt_pred %>%
  filter(tier_clean %in% c("Lower Third") & response_clean %in% c("Tree Cover Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_tc_mt <- dt_pred %>%
  filter(tier_clean %in% c("Middle Third") & response_clean %in% c("Tree Cover Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

dt_rect_tc_ut <- dt_pred %>%
  filter(tier_clean %in% c("Upper Third") & response_clean %in% c("Tree Cover Trend")) %>% 
  dplyr::select(q95_unscaled, q05_unscaled, response_clean, var_clean) %>% 
  unique() %>% 
  group_by(var_clean) %>%
  summarize(
    ymin = -Inf, ymax = Inf, xmin1 = -Inf,
    xmax1 = first(q05_unscaled), xmin2 = first(q95_unscaled), xmax2 = Inf
  ) %>%
  ungroup()

##### Plot different tiers -------------

p_ch_lt <- dt_pred %>% 
  filter(tier_clean %in% c("Lower Third") & response_clean %in% c("Canopy Height Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#40631F") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#40631F") +
  geom_rect(data = dt_rect_ch_lt,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_ch_lt,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Canopy Height Trend", x = "Variable Value", title = "Cells With Low Initial Canopy Height") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_ch_lt

p_ch_mt <- dt_pred %>% 
  filter(tier_clean %in% c("Middle Third") & response_clean %in% c("Canopy Height Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#40631F") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#40631F") +
  geom_rect(data = dt_rect_ch_mt,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_ch_mt,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Canopy Height Trend", x = "Variable Value", title = "Cells With Intermediate Initial Canopy Height") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_ch_mt

p_ch_ut <- dt_pred %>% 
  filter(tier_clean %in% c("Upper Third") & response_clean %in% c("Canopy Height Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#40631F") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#40631F") +
  geom_rect(data = dt_rect_ch_ut,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_ch_ut,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Canopy Height Trend", x = "Variable Value", title = "Cells With High Initial Canopy Height") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_ch_ut


p_tc_lt <- dt_pred %>% 
  filter(tier_clean %in% c("Lower Third") & response_clean %in% c("Tree Cover Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#215F61") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#215F61") +
  geom_rect(data = dt_rect_tc_lt,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_tc_lt,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Tree Cover Trend", x = "Variable Value", title = "Cells With Low Initial Tree Cover") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_tc_lt

p_tc_mt <- dt_pred %>% 
  filter(tier_clean %in% c("Middle Third") & response_clean %in% c("Tree Cover Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#215F61") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#215F61") +
  geom_rect(data = dt_rect_tc_mt,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_tc_mt,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Tree Cover Trend", x = "Variable Value", title = "Cells With Intermediate Initial Tree Cover") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_tc_mt

p_tc_ut <- dt_pred %>% 
  filter(tier_clean %in% c("Upper Third") & response_clean %in% c("Tree Cover Trend")) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  geom_ribbon(aes(x = x_unscaled, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#215F61") +
  geom_line(aes(x = x_unscaled, y = predicted), linewidth = 1.1, color = "#215F61") +
  geom_rect(data = dt_rect_tc_ut,
            aes(xmin = xmin1, xmax = xmax1, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  geom_rect(data = dt_rect_tc_ut,
            aes(xmin = xmin2, xmax = xmax2, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.8, inherit.aes = FALSE) +
  labs(y = "Tree Cover Trend", x = "Variable Value", title = "Cells With High Initial Tree Cover") +
  theme_bw() +
  facet_wrap(~var_clean, scales = "free_x", ncol = 45) +
  theme(legend.position = "right", 
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "snow"), 
        strip.background = element_rect(fill = "linen", color = "linen"))

p_tc_ut

p <- p_ch_lt / p_ch_mt / p_ch_ut / p_tc_lt / p_tc_mt / p_tc_ut
p
ggsave(plot = p, "builds/plots/supplement/subset_predictions_1000m.png", 
       dpi = 900, height = 12, width = 9)
