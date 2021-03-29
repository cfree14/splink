
# Packages
library(TMB)
library(TMBhelper)
library(tidyverse)

# Get RAM data
data <- splink::ram

# Prep RAM data
data_prepped <- splink::prep_data(data)

# Inspect a sample of prepped data
splink::plot_raw(data_prepped, n=20)

# Use simple NLS fit to identify problematic stocks
output <- splink::fit_sp_nls(data = data_prepped, p=0.2)

# Inspect fits
hist(output$r, breaks=seq(0,10,0.2))
hist(output$k, xlim=c(0,10), breaks=seq(0,10000,0.2))

# Problem stocks
problem_stocks <- output %>%
  filter(is.na(r) | is.na(k) | r>3 | k>10) %>%
  pull(stockid)

# Remove problem stocks
data_final <- data_prepped %>%
  filter(!stockid %in% problem_stocks)

# Fit to data
output <- splink::fit_sp(data = data_final, p=0.2)

# Extract results
results <- splink::get_results(output)

# Plot results
splink::plot_results(results)


