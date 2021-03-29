splink: Covariate-linked biomass dynamic models
======================================================================

Installation
------------

The "splink" R package can be installed from GitHub with:

``` r
# Run if you don't already have devtools installed
install.packages("devtools")

# Run once devtools is successfully installed
devtools::install_github("cfree14/splink", force=T)
library(splink)
```


Functions
---------

The package implements functions to:

- Prepare data for fitting biomass dynamics models: `?prep_data`
- Quickly visualizing prepped data: `?plot_raw`
- Fitting biomass dynamics modeling using NLS: `?fit_sp_nls`
- Fitting biomass dynamics modeling using maximum likelihood using TMB: `?fit_sp`
- Extracting parameter estimates from TMB model fits: `?get_results`
- Quickly plotting parameter estimates from TMB model fits: `?plot_results`


Citation
---------

Free CM (2021) splink: Covariate-linked biomass dynamic models.
