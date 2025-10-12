library(tidyverse)
pop <- c(200, 300, 450, 650)

years <- c(2004, 2005, 2006, 2007)

park_size <- 120

mean_dens <- pop/park_size

norm_year <- years - min(years-1)

dt_lm <- data.frame(pop, mean_dens, norm_year)

rel_habitat_quality <- c(0.1, 0.1, 0.5, 0.25, 0.05)
cell_id <- c("a", "b", "c", "d", "e")

cell_size <- 24

dt_hq <- data.frame(rel_habitat_quality, cell_size, park_size = 120, cell_id)

dt_mod <- data.frame()

for(i in 1:nrow(dt_lm)){
  
  i_pop <- dt_lm[i,]$pop
  i_year <- dt_lm[i,]$norm_year
  
  dt_tmp <- dt_hq %>% mutate(rel_dens = (i_pop*rel_habitat_quality)/cell_size, 
                        mean_dens = i_pop/park_size) %>% 
    mutate(norm_year = i_year)
  
  dt_mod = rbind(dt_tmp, dt_mod)

}

dt_ave <- dt_mod %>% dplyr::select(norm_year, mean_dens) %>% unique()

summary(m1 <- lm(mean_dens~norm_year, data = dt_ave))

estimates <- c()
for(cell in unique(dt_mod$cell_id)){
  
dt_cell <- dt_mod %>%
  filter(cell_id == cell) %>% 
  dplyr::select(norm_year, mean_dens) %>% unique()

m2 <- lm(mean_dens~norm_year, data = dt_cell)
estimates <- c(m2$coefficients[2], estimates)

}
estimates
