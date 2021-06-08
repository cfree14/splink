
#' Fit surplus production model using NLS estimation
#'
#' This function fits a surplus production model using NLS estimation.
#'
#' @param data A prepped data frame with the following required columns: stockid, year, biomass_scaled, sp_scaled
#' @param p The shape parameter
#' @return A data frame containing the point estimate fits for r and K
#' @examples
#' data <- splink::ram_ne
#' output <- fit_sp_nls(data=data, b_col="tb_scaled", sp_col="sp_scaled", p=0.4)
#' output$fits
#' splink::plot_fits_nls(output)
#' @export
fit_sp_nls <- function(data, b_col, sp_col, p=1){

  # For testing
  # data=ram_ne; b_col="tb_scaled"; sp_col="sp_scaled"; p=0.4

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

  # Format data
  data_use <- data[,c("stockid", "year", b_col, sp_col)] %>%
    setNames(c("stockid", "year", "biomass", "sp"))

  # Data frame to record SP fits
  spfits <- data_use %>%
    group_by(stockid) %>%
    summarize(biomass_max=max(biomass)) %>%
    ungroup() %>%
    mutate(p=p, r=NA, k=NA)

  # Loop and fit
  for(i in 1:nrow(spfits)){

    # Subset data
    stock <- spfits$stockid[i]
    sdata <- data_use %>%
      filter(stockid==stock)
    print(paste(i, stock))

    # Fit SP model
    spfit <- fit_sp_nls(sp=sdata$sp, biomass=sdata$biomass, p=p)
    if(!(inherits(spfit, "try-error"))){
      r <- exp(coef(spfit)["r"])
      k <- exp(coef(spfit)["k"])
      spfits$r[spfits$stockid==stock] <- r
      spfits$k[spfits$stockid==stock] <- k
      # plot(sp ~ biomass, sdata)
      # curve(r/p*x*(1-(x/k)^p), from=0, to=max(sdata$biomass), add=T, lty=1, lwd=0.9, col="red")
    }

  }

  # Package
  output <- list(data=data, fits=spfits, b_col=b_col, sp_col=sp_col)

  # Return
  return(output)

}
