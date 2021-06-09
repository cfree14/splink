
# Packages
library(TMB)
library(TMBhelper)
library(tidyverse)

# Test recruitment functions
#################################################################################

# Get data
data <- splink::ram_wc

# Fit models
model_base <- splink::fit_sr(data=data, b_col="b_scaled", r_col="r_scaled", type="ricker")
model_fixed <- splink::fit_sr(data=data, b_col="b_scaled", r_col="r_scaled", type="ricker", cov_col="sst_c_scaled", cov_effect = "fixed")
# model_random <- splink::fit_sr(data=data, b_col="b_scaled", r_col="r_scaled", type="ricker", cov_col="sst_c_scaled", cov_effect = "random")


# Get results
results_base <- splink::get_results(model_base)
results_fixed <- splink::get_results(model_fixed)

# Plot results
splink::plot_results(results_base)
splink::plot_results(results_fixed)

# Plot thetas
splink::plot_thetas(results_fixed)

# Compare models
stats <- splink::compare_models(models=list(model_base, model_fixed), names=c("Base", "Fixed"))



# Test production functions
#################################################################################

# Get RAM data
data <- splink::ram

# Prep RAM data
data_prepped <- splink::prep_data(data)

# Inspect a sample of prepped data
splink::plot_raw(data_prepped, b_col="biomass_scaled", sp_col="sp_scaled", n=20)

# Use simple NLS fit to identify problematic stocks
output <- splink::fit_sp_nls(data = data_prepped,  b_col="biomass_scaled", sp_col="sp_scaled", p=0.2)

# Inspect fits
hist(output$fits$r, breaks=seq(0,10,0.2))
hist(outputfits$k, xlim=c(0,10), breaks=seq(0,10000,0.2))

# Problem stocks
problem_stocks <- output$fits %>%
  filter(is.na(r) | is.na(k) | r>3 | k>10) %>%
  pull(stockid)

# Remove problem stocks
data_final <- data_prepped %>%
  filter(!stockid %in% problem_stocks)

# Fit to data
output <- splink::fit_sp(data = data_final, p=1)

# Extract results
results <- splink::get_results(output)

# Plot results
splink::plot_results(results)

