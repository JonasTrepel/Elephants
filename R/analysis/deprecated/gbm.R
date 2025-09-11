
library(data.table)
library(ggplot2)
library(gbm)
library(caret)
library(MetBrewer)
library(tictoc)
library(gridExtra)

#load data 
sf_parks <- st_read("data/spatial_data/protected_areas/park_boundaries.gpkg")

dt <- fread("data/processed_data/clean_data/final_point_data.csv") %>% 
  mutate(grass_cover_coef = grass_cover_coef*100, 
         gr_n_cr_cover_coef = gr_n_cr_cover_coef*100, 
         tree_cover_coef = tree_cover_coef*100, 
         shrub_cover_coef = shrub_cover_coef*100, 
         bare_cover_coef = bare_cover_coef*100, 
         mean_evi_coef = mean_evi_coef/100,
         park_id = as.factor(park_id)
  ) %>% 
  filter(!park_id %in% c("Zambezi"))

# get dataframe with comlete and clean data fro mdoeling 
dt_mod <- dt %>% 
  filter(dw_min_mode_fraction >= 50) %>% 
  select(
    #mean values /habitat characteristics 
    mean_grass_cover, mean_gr_n_cr_cover, mean_tree_cover, mean_shrub_cover, mean_evi, 
    mean_habitat_diversity_100m, mean_habitat_diversity_1000m, 
    
    elevation, mat, map, slope, distance_to_water_km, n_deposition, human_modification, 
    fire_frequency, 
    
    #Trends - Responses 
    grass_cover_coef, gr_n_cr_cover_coef, tree_cover_coef, shrub_cover_coef, bare_cover_coef, 
    habitat_diversity_100m_coef, habitat_diversity_1000m_coef, mean_evi_coef, 
    
    #Trends  - Predictors 
    mat_coef, prec_coef, burned_area_coef,
    
    #Elephant variables 
    mean_density_km2, local_density_km2, density_km2_estimate, population_trend_estimate, 
    
    #Coords 
    x_mollweide, y_mollweide, lon, lat, 
    
    #identifieres 
    park_id
  ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    local_density_km2_scaled = as.numeric(scale(local_density_km2)),
    density_km2_estimate_scaled = as.numeric(scale(density_km2_estimate)),
    mat_coef_scaled = as.numeric(scale(mat_coef)),
    prec_coef_scaled = as.numeric(scale(prec_coef)),
    n_deposition_scaled = as.numeric(scale(n_deposition)),
    fire_frequency_scaled = as.numeric(scale(fire_frequency)),
    burned_area_coef_scaled = as.numeric(scale(burned_area_coef))
  )

dt_gbm <- dt_mod %>% 
  select(mean_evi_coef, 
         n_deposition, 
         mat_coef, 
         prec_coef, 
         local_density_km2, 
         density_km2_estimate, 
         burned_area_coef, 
         fire_frequency)


fit_control <- trainControl(## 10 fold cross validation
  method = "repeatedcv",
  number = 10, ##repeat 10 times
  repeats = 10, #
  savePredictions = "final", #keep final model predictions, would otherwise be dumped
  returnResamp = "final")


# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c( .001, .005, .01),
  interaction.depth = c(3),#3, 6, 9
  n.minobsinnode = c(12), ## 6, 9, 12
  # bag.fraction = c(.65, .8, 1), 
  n.trees = seq(200, 8200, 1000)#c(150, 300, 450, 600, 750, 900)#, #seq(150, 4650, 600) #seq(150, 6500, 600)
  # optimal_trees = 0,               # a place to dump results
  # min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
set.seed(1312)
tic()
gbm_fit2 <- train(mean_evi_coef ~ .,
                  data = dt_gbm, 
                  method = "gbm",
                  trControl = fit_control,
                  verbose = TRUE,
                  tuneGrid = hyper_grid)
toc() # min
gbm_fit2

#palette <- met.brewer("VanGogh3", n = 20)

head(gbm_fit2$results[order(gbm_fit2$results$RMSE),]) #sort that the best model (lowest root mean squared error) is on top
bf2 <- row.names(gbm_fit2$bestTune)
gbm_fit2$results[grep(bf2, row.names(gbm_fit2$results)),]

psi <- ggplot(gbm_fit2) +
  theme_bw() + ## sweet SI plot 
  scale_color_manual(values = c("#c2d6a4", "#669d62", "#1e3d14")) +
  labs(x = "Number of Boosting Iterations", color = "Interaction depth", shape = "Interaction depth") +
  scale_x_continuous(sec.axis = dup_axis(name = "Minimum number of points per node")) +
  scale_y_continuous(sec.axis = dup_axis(name = "Learning rate (shrinkage)")) +
  theme(axis.line.x.top=element_line(color="white"),
        axis.text.x.top = element_text(color="white"),
        axis.ticks.x.top = element_line(color="white"),
        axis.line.y.right =element_line(color="white"),
        axis.text.y.right = element_text(color="white"),
        axis.ticks.y.right = element_line(color="white"),
        axis.title.y = element_text(color="black")) +
  guides(shape = FALSE)
psi

dt_rel <- as.data.table(summary(gbm_fit2)) #takes summary from the best model 


dt_rel$var_clean <- gsub("`", "", dt_rel$var)

p1 <- ggplot() +
  geom_col(data = dt_rel[var_clean != "Spatial predictor"], aes(y = reorder(var_clean, rel.inf), x = rel.inf, fill = var_clean)) +
 # scale_fill_manual(values = c("#192813", "#c2d6a4","#1f5b25", "#669d62")) +
  labs(x = "Relative variable importance", y = "", fill = "Variable") +
  theme_classic() +
  theme(legend.position = "none")

p1



#### Marginal effect plots ----
for(i in 1:(ncol(dt_gbm)-1)){
  temp <- plot.gbm(gbm_fit2$finalModel, i, return.grid = TRUE)
  if(i==1){
    marg <- temp}else{
      marg <- cbind(marg, temp)}
}
# add meaningful names for marginal effect plots df
names(marg)[seq(2, ncol(marg), 2)] <- paste0(names(marg)[seq(1, (ncol(marg)-1), 2)], "_y")

marg.t = reshape2::melt(marg) # mel hates this melt shit sometimes....
y = grep("_y", marg.t$variable)
marg.plot = data.frame(var = marg.t[-y,]$variable,
                       x = marg.t[-y,]$value,
                       y = marg.t[y,]$value
)

marg.plot$var <- gsub("`", "", marg.plot$var)
table(marg.plot$var)
p1
p2 <- ggplot(marg.plot, aes(x=x, y=y, color = var))+
  geom_line() +
  facet_wrap(~var, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_line(color = "white"))
p2

p <- grid.arrange(p1, p2, ncol = 2)