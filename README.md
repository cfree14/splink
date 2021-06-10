splink: Covariate-linked population dynamics models
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

- Fit covariate-linked stock recruit models: `?fit_sr`
- Fit covariate-linked surplus production models: `?fit_sr`
- Extract parameter estimates and confidence intervals from model fits: `?get_results`
- Plot parameter estimates and confidence intervals: `?plot_results`
- Plot covariate influence estimates and confidence intervals: `?plot_thetas`
- Plot model fits on top of data: `?plot_fits`

The fitting functions allow covariate estimates to be estimated as either fixed or random effects. The production model can be fit using a user-specified shape parameter and the stock recruit model can be fit using either a Ricker or Beverton-Holt stock recruit relationship.


Citation
---------

Free CM (2021) splink: Covariate-linked population dynamics models. Available at: https://github.com/cfree14/splink/
