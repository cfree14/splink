
#' Build surplus production curves
#'
#' This function builds surplus production curves.
#'
#' @param output SP model output
#' @param cov_vals Covariate models to build curves for
#' @return A dataframe with SP curves.
#' @export
build_sp_curves <- function(output, cov_vals){

  # Build lines
  #############################

  # Extract results
  results_orig <- splink::get_results(output)
  if(length(results_orig)==2){
    results <- results_orig$stock
  }else{
    results <- results_orig
  }

  # Params
  spfits <- results %>%
    select(stockid, param, est) %>%
    spread(key="param", value="est")

  # Biomass values to evaluate
  b <- seq(0, 1, 0.01)

  # Create lines
  sp_lines <- purrr::map_df(1:nrow(spfits), function(x){

    # Parameters
    stockid <- spfits$stockid[x]
    r <- spfits$r[x]
    k <- spfits$B0[x]
    theta <- spfits$theta[x]
    p <- output$p

    # Loop through covariate values
    sp_lines1 <- purrr::map_df(cov_vals, function(x){

      # Simulate data
      sp <- r/p * b * (1-(b/k)^p) * exp(theta*x)

      # Record production
      z <- data.frame(stockid=stockid,
                      cov_scaled=x,
                      b_scaled=b,
                      sp_scaled=sp)

    })

  })

  # Return
  return(sp_lines)


}
