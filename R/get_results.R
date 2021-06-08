
#' Extracts the parameter estimates and confidence intervals from a TMB model fit
#'
#' This function extracts the parameter estimates and confidence intervals from a TMB model fit.
#'
#' @param output Output from function fitting biomass dynamics model
#' @return A data frame with the point estimates, standard error, and 95% confidence interval from model fit
#' @export
get_results <- function(output){

  # Stocks
  stocks <- unique(output$data$stockid)

  # Extract estimates and standard errors
  results_mat <- TMB::summary.sdreport(output$sd)

  # Format parameter estimates
  results_df <- data.frame(stockid=stocks,
                           param=rownames(results_mat),
                           est=results_mat[,1],
                           est_se=results_mat[,2], row.names = NULL,
                           stringsAsFactors = F) %>%
    # Calculate confidence intervals
    mutate(est_lo=est - est_se*1.96,
           est_hi=est + est_se*1.96)

  # Format log-transformed results
  results_logged <- results_df %>%
    filter(grepl("ln_", param)) %>%
    mutate(param=gsub("ln_", "", param)) %>%
    mutate_at(.vars=paste0("est", c("", "_se", "_lo", "_hi")), .funs=exp) %>%
    ungroup()

  # Format theta results
  results_theta <- results_df %>%
    filter(param=="theta")

  # Merge
  results <- bind_rows(results_logged, results_theta)

  # Return
  return(results)

}
