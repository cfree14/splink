
# Calculate surplus production
# SP(t) = TB(t+1) - TB(t) + C(t)
calc_sp <- function(biomass, catch){
  sp <- c(biomass[2:length(biomass)] - biomass[1:(length(biomass)-1)] + catch[1:(length(catch)-1)], NA)
  return(sp)
}
