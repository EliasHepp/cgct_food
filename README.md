# Causal Machine Learning for Cost-Effective Allocation of Food Development Aid

This project is based on the CG-CT framework developed here: https://github.com/mkuzma96/CG-CT

Please find the corresponding article here: https://arxiv.org/abs/2401.16986

# Requirements 

python 3.11.3

R 4.2.2.

# Data

The data was acquired from the following sources:

Food Insecurity as calculated by the Food and Agricultural Organization (https://www.fao.org/faostat/en/#data/FS).

Development Aid by AidDAta's Financing the 2030 Agenda for Sustainable Development, Version 1.0.  (https://www.aiddata.org/data/financing-the-2030-agenda-for-sustainable-development-version-1-0).

Covariate Data 

- On Conflict by the UCDP's Geo-Referenced Dataset
- On Economic Data by the World Bank's World Development Indicator database (https://data.worldbank.org/).
- On Governance by the Wolrd Bank's Worldwide Governance Indicators database (https://www.worldbank.org/en/publication/worldwide-governance-indicators)
- On Disaster Affection by The International Disaster Database (https://www.emdat.be/)
- On Coups by V-Dem's dataset (https://v-dem.net/)
- On Regime Type by the Polity Project (https://www.systemicpeace.org/polityproject.html)
 - On Conflict by UCDP's Georeferenced Event Dataset (https://ucdp.uu.se/downloads/index.html#ged_global)
- On Road Networks by the International Road Federation (https://worldroadstatistics.org/)
- On Precipitation by the Copernicus Climate Change Service (https://climate.copernicus.eu/)

Original datasources are omitted from the repository due to their size.

# Structure and Implementation

Subfolder data contains all original datasets and scripts for data wrangling. Also contains script to visualize all outputs only using the dataset.

Subfolder hyperpars contains lists of optimal hyperparameters for all applied models.
Subfolder opt_studies contains list of all optuna runs for hyperparameter optimization

Subfolder output contains all outputs and script to for visualisations and tables.

Script main.py contains the functions to calcualte the metric of interest for all models. 

- Hyperparamter Tuning:

Script contains the functions to find the optimal hyperparameters for all models. Script cv_params.ipynb implements the search.

- Results:

Scripts res_run_sim.py and res_run.py reproduces the results of the analysis with synthetic data and with real-world data, using the functions from main.py and the hyperparameters in the subfolder hyperpars respectively.

Script optim_allocation.py calculates the hypothetical optimal allocation of food aid.

- Ablation:

Scripts for ablation study of cg-ct framework.

- Robustness:

Scripts for robustness tests.

- Causal Sensitivity:

Script to get treatment-response curves.version


