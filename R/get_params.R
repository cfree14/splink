
# Extract parameters
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

  # Fortmat log-transformed results
  results <- results_df %>%
    filter(grepl("ln_", param)) %>%
    mutate(param=gsub("ln_", "", param)) %>%
    mutate_at(.vars=paste0("est", c("", "_se", "_lo", "_hi")), .funs=exp) %>%
    ungroup()

  # Return
  return(results)

}
