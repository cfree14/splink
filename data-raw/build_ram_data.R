
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "approach2/data"

# Read RAM Legacy Database v4.491 (with model fits)
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAM v4.491 Files (1-14-20)/RAM v4.491/DB Files With Model Fit Data/R Data/DBdata[mdl][v4.491].RData")

crazy <- "REYEROCKGA"
perfect_srs <- c("BGROCKPCOAST", "GRSPROCKNCAL", "GRSPROCKSCAL", "LNOSESKAPCOAST", "SPSDOGPCOAST", "YEYEROCKPCOAST")
se_fail <- c("PCOD5AB", "SARDPCOAST", "SABLEFPCOAST", "PCODHS") # no peak?

# Read WC RAM data for example recruitment data
ram_wc <-readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/ramldb/processed/RAM_WC_recruitment_data_prepped.Rds") %>%
  filter(!stockid %in% c(crazy, perfect_srs, se_fail))


# Build data
################################################################################

# Data
ram <- timeseries_values_views %>%
  # Data of interest
  select(stockid, year, TBbest, TCbest) %>%
  rename(biomass=TBbest, catch=TCbest) %>%
  # Add units
  left_join(timeseries_units_views %>% select(stockid, TBbest, TCbest), by="stockid") %>%
  rename(biomass_units=TBbest, catch_units=TCbest) %>%
  # Add source
  left_join(timeseries_sources_views %>% select(stockid, TBbest, TCbest), by="stockid") %>%
  rename(biomass_source=TBbest, catch_source=TCbest) %>%
  # Add source
  left_join(timeseries_ids_views %>% select(stockid, TBbest, TCbest), by="stockid") %>%
  rename(biomass_type=TBbest, catch_type=TCbest) %>%
  # Arrange
  select(stockid, year,
         #biomass_source, biomass_type,
         biomass_units, biomass,
         #catch_source, catch_type,
         catch_units, catch) %>%
  # Reduce to stock with biomass and catch in same units
  filter(biomass_units==catch_units)

# RAM New England data
ram_ne <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/ne_fishing_portfolio/data/ramldb/data/processed/RAM_NE_data_w_sst_trimmed_prepped.Rds") %>%
  select(-c(source, catch_type))


# Export data
################################################################################

# Save data
usethis::use_data(ram, overwrite = T)
usethis::use_data(ram_ne, overwrite = T)
usethis::use_data(ram_wc, overwrite = T)






