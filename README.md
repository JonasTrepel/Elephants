# Elephant Impacts on Woody Plant Dynamics Across Southern Africa

This repository contains the code used for the analyses presented in the manuscript *“Elephants have scale dependent impacts on woody vegetation structure across southern African savannas”*, currently under review.

Due to privacy constraints and data-sharing agreements, the raw data underlying this study cannot be made publicly available. However, we provide the processed datasets required to reproduce the main analysis, results and figures reported in the manuscript.

Because of their size, these data are not hosted on GitHub and can instead be accessed here: https://osf.io/zdyje/files/osfstorage

## Repository structure

The repository is organized into the following directories:

### `R/prep/`
Scripts for data preparation, including:
- Cleaning raw data
- Acquiring and processing covariates
- Compiling analysis-ready datasets used in downstream models

### `R/functions/`
Customized functions called in scripts in the preparation steps.  

### `R/analysis/`
Scripts used to fit the statistical models reported in the manuscript. Key files include:
- `park_level_average_models.R`: Runs models at the regional / reserve-level scale.
- `1000m_models.R`: Runs local-scale models using predicted, spatially explicit elephant density estimates.
- `loo_models_1000m.R`: Conducts a sensitivity analysis using a leave-one-reserve-out approach.
- `subset_models_1000m.R`: Fits models separately for subsets of the data defined by different vegetation starting conditions.

### `R/sensitivity/`
Additional scripts used for sensitivity analyses beyond those included in the main analysis directory.

### `R/viz/`
Scripts used to generate the figures included in the manuscript and associated supplementary materials.
