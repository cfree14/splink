

data <- splink::ram
prep_data <- function(data){

  # Format data
  data_prepped <- data %>%
    # Calculate surplus production
    group_by(stockid) %>%
    mutate(sp=calc_sp(biomass=biomass, catch=catch)) %>%
    ungroup() %>%
    # Reduce to years with data
    filter(!is.na(biomass) & !is.na(sp)) %>%
    # Scale biomass and production
    group_by(stockid) %>%
    mutate(biomass_scaled=biomass/max(biomass),
           sp_scaled=sp/max(biomass))

  # Return
  return(data_prepped)

}
