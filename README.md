# ESGI 187: Vodafone challenge
## Signal Strength Sync: Modelling and Verifying Ofcom 4G Field Measurements Against VodafoneThree Predicted Radio Network Data

This repository contains code for analyses related to the Vodafone challenge at ESGI 187.

### Key scripts

+ `data_processing.jl`: Julia script for data cleaning and processing which produces a csv file with relevant data fields from both Ofcom and Vodafone data for each terrainbin ([@dawbarton](https://github.com/dawbarton))
+ `cecina_landuse.R`: R script taking the cleaned data produced by `data_processing.jl`, adding on urban vs. rural land use classification, and writing out the resulting csv ([@babichmorrowc](https://github.com/babichmorrowc))
+ `ejh_pca.R`: R script performing Principle Components Analysis (PCA) ([@ejhall](https://github.com/ejhall))
+ `ejh_gam.R` : R script for building and fitting the GAM model. ([@ejhall](https://github.com/ejhall))
+ `ejh_xgboost.R` : R script for building and fitting the XGBoost regression models and performing SHAP analysis. ([@ejhall](https://github.com/ejhall))