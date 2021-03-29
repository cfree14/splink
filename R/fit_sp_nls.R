
#' Fit surplus production model using NLS estimation
#'
#' This function fits a surplus production model using NLS estimation.
#'
#' @param data A prepped data frame with the following required columns: stockid, year, biomass_scaled, sp_scaled
#' @param p The shape parameter
#' @return A data frame containing the point estimate fits for r and K
fit_sp_nls <- function(data, p=1){

  # Parameters
  stocks <- unique(data$stockid)
  nstocks <- length(stocks)

  # Function to fit surplus production model
  fit_sp_nls <- function(sp, biomass, p, r_start=NA, k_start=NA){
    if(is.na(r_start)){r_start <- log(0.4)}
    if(is.na(k_start)){k_start <- log(max(biomass) * 1.5)}
    spfit <- try(nls(sp ~ exp(r)/p*biomass*(1-(biomass/exp(k))^p),
                     start=list(r=r_start, k=k_start)))
    return(spfit)
  }

  # Data frame to record SP fits
  spfits <- data %>%
    group_by(stockid) %>%
    summarize(biomass_max=max(biomass)) %>%
    ungroup() %>%
    mutate(p=p, r=NA, k=NA)

  # Loop and fit
  for(i in 1:nrow(spfits)){

    # Subset data
    stock <- spfits$stockid[i]
    sdata <- data %>%
      filter(stockid==stock)
    print(paste(i, stock))

    # Fit SP model
    spfit <- fit_sp_nls(sp=sdata$sp_scaled, biomass=sdata$biomass_scaled, p=p)
    if(!(inherits(spfit, "try-error"))){
      r <- exp(coef(spfit)["r"])
      k <- exp(coef(spfit)["k"])
      spfits$r[spfits$stockid==stock] <- r
      spfits$k[spfits$stockid==stock] <- k
      # plot(sp_scaled ~ biomass_scaled, sdata)
      # curve(r/p*x*(1-(x/k)^p), from=0, to=1, add=T, lty=1, lwd=0.9, col="red")
    }

  }

  # Return
  return(spfits)

}
