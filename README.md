# Elephant Impacts on Woody Plant Dynamics Across Southern Africa

This repository contains the code used for the analyses presented in the manuscript *“Elephants have scale dependent impacts on woody vegetation structure across southern African savannas”*, currently under review.

Due to privacy constraints and data-sharing agreements, the raw data underlying this study cannot be made publicly available. However, we provide the processed datasets required to reproduce the main analysis reported in the manuscript.

Because of their size, these data are not hosted on GitHub and can instead be accessed here: https://osf.io/zdyje/files/osfstorage

## Repository structure

The repository is organized into the following directories:

### `R/prep/`
NB! These scripts cannot be reproduced as we are not permitted to share the data :(. 
Scripts for data preparation, including:
- Cleaning raw data
- Acquiring and processing covariates
- Compiling analysis-ready datasets used in downstream models

### `R/functions/`
Customized functions called in scripts in the preparation steps.  
- `monitor_gee_task.R`: function to pause script while task is running on google earth engine until it's complete
- `assign_cluster.R`: function to assign each movement track to one of the clusters 
- `assign_park.R`: function to assign each movement track to one of the parks 


### `R/analysis/`
Scripts used to fit the statistical models reported in the manuscript. Key files include:

NB! Both the 1000m_models stript as well as the step_selection_functions script exceed the processing capacity of personal computers (at least of mine) and may have to be run on a supercomputer, server or cloud  
- `park_level_average_models.R`: Runs models at the regional / reserve-level scale.
- `step_selection_functions.R`: Run step selection analysis for different time steps (incl. 24 hours, which we used in the main analysis)
- `1000m_models.R`: Runs local-scale models using predicted, spatially explicit elephant density estimates.

### `R/analysis/sensitivity/`
Scripts to conduct the sensitivity analysis 
NB! Most sensitivity analysis scripts exceed the processing capacity of personal computers (at least of mine) and may have to be run on a supercomputer, server or cloud

- `loo_models_1000m.R`: Conducts a sensitivity analysis using a leave-one-reserve-out approach.
- `subset_models_1000m.R`: Fits models separately for subsets of the data defined by different vegetation starting conditions.
- `park_models_1000m.R`: Fits models separately for each park
- `simulate_random_elephant_distribution.R`: Checks if our actual elephant data is better than a random guess (turns out it is!)
- `100m_models.R`: Runs local-scale models using predicted, spatially explicit elephant density estimates at 100m instead of 1000m scale.
- `check_concurvity.R`: Checks if we have concurvity issues in the models (we have not).
- `model_habitat_quality_knp_counts.R`: Test predicted elephant densities vs spatially explicit counts 

### `R/viz/`
Scripts used to generate the figures included in the manuscript
- `reserve_level_figure.R`: code for fig. 1
- `plot_step_selection_figure.R`: code for fig. 2 (needs location data, which we sadly cannot share)
- `extract_model_preds.R`: extract model predictions from main models at 1000m scale
- `prediction_figure_local_scale.R`: produces figure 3. 
R/viz/supplement/ contains scripts to produce the supplementary figures`


Please don't hesitate to contact me at jonas.trepel[at]bio.au.dk should you have any questions! 
