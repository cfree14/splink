
#' Calculate surplus production time series
#'
#' This function calculates a time series of surplus production from time series of biomass and catch: SP(t) = TB(t+1) - TB(t) + C(t). Biomass and catch must be in the same units.
#'
#' @param biomass A time series of biomass
#' @param catch A time series of catch (in same units as biomass)
#' @return A time series of surplus production (in same units as biomass/catch)
calc_sp <- function(biomass, catch){
  sp <- c(biomass[2:length(biomass)] - biomass[1:(length(biomass)-1)] + catch[1:(length(catch)-1)], NA)
  return(sp)
}
