# Allometric models for quantifying shrub biomass

This repository provides further details on the development of the allometric models used in the R package [BerkeleyShrubAnalytics](https://github.com/kearutherford/BerkeleyShrubAnalytics).

### Description of files in repository

* **prep_data.Rmd:** code for preparing the data for fitting the allometric models 
* **ABOVEg_model_fits.Rmd:** code for running all model fits for aboveground live shrub biomass
* **ABOVEg_model_fits.html:** html file with all model fits for aboveground live shrub biomass (best option for viewing)
* **diag_plots_fn.R:** function for producing diagnostic plots for all model fits (this function is called in the ABOVEg_model_fits.Rmd file)
* **ABOVEg_coefs.Rmd:** code for pulling the coefficients for the final model fits

### Descripton of data in data folder 

* **shrub_data_JB.csv:** input dataset for prep_data.Rmd. Columns described below:
    * `Species`: Follows four-letter species code naming conventions (first two letters of the genus followed by the first two letters of the species). See species code table in BerkeleyShrubAnalytics.
    * `Site`: Site codes for location of sampling.
        * LNF (Lassen NF), YOSE (Yosemite NP), COMPLEX (Stanilaus Complex Fire), STAR (Star Fire, Eldorado and Tahoe NF), MCNALLY (McNally Fire, Sierra NF), PLUMAS (Plumas NF), SAGEHEN (Sagehen Experimental Forest), BROOK (Brookings, OR)
    * `Author`: First author of data.
    * `Hgt.m`: Height in meters.
    * `CW1.m`: Major axis (longest axis) of shrub crown in meters. 
    * `CW2.m`: Minor axis (longest axis perpendicular to major axis) of shrub crown in meters. 
    * `Leaf.g`: Mass of leaves in grams.
    * `Stem.g`: Mass of stem in grams. 
    * `AGL.g`: Mass of aboveground live biomass in grams. Includes wood, bark, and leaves. Excludes flowers, seeds, litter, and dead wood.
    * `CA.m2`: Crown area in meters squared. 

* **shrub_prepped.csv:** dataset created by prep_data.Rmd file. Used for fitting aboveground live shrub biomass models.

* **agl_coefs.csv:** dataset created by ABOVEg_coefs.Rmd file.

### Contact information

For further details or questions contact Kea Rutherford (krutherford@berkeley.edu) or John Battles (jbattles@berkeley.edu).
