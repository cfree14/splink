
#' Build stock recruit curves
#'
#' This function builds stock recruit curves.
#'
#' @param output SR model output
#' @param cov_vals Covariate models to build curves for
#' @return A dataframe with SR curves.
#' @export
build_sr_curves <- function(output, cov_vals){

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
  srfits <- results %>%
    select(stockid, param, est) %>%
    spread(key="param", value="est")

  # Biomass values to evaluate
  b <- seq(0, 1, 0.01)

  # Create lines
  sr_lines <- purrr::map_df(1:nrow(srfits), function(x){

    # Parameters
    stockid <- srfits$stockid[x]
    alpha <- srfits$alpha[x]
    beta <- srfits$beta[x]
    theta <- srfits$theta[x]

    # Loop through covariate values
    sr_lines1 <- purrr::map_df(cov_vals, function(x){

      # Simulate data
      recruits <- alpha * b * exp(-beta*b) * exp(theta*x)

      # Record production
      z <- data.frame(stockid=stockid,
                      cov_scaled=x,
                      b_scaled=b,
                      r_scaled=recruits)

    })

  })

  # Return
  return(sr_lines)


}
