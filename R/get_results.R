
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
  nstocks <- length(stocks)

  # Fixed or random effects output?
  cov_effect <- output$cov_effect

  # Extract fixed effects results
  ########################################

  # Fixed
  if(cov_effect %in% c("none", "fixed")){

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

  }

  # Extract random effects results
  ########################################

  # Fixed
  if(cov_effect=="random"){

    # Format parameter estimates
    results.mat <- summary.sdreport(output$sd)
    results.df <- data.frame(param=rownames(results.mat),
                             est=results.mat[,1],
                             est_se=results.mat[,2]) %>%
      # Calculate confidence intervals
      mutate(est_lo=est - est_se*1.96,
             est_hi=est + est_se*1.96)

    # Build parameter key
    param_key <- results.df %>%
      group_by(param) %>%
      summarize(n=n()) %>%
      ungroup() %>%
      mutate(type=ifelse(n==nstocks, "stock", "global"))
    stock_params <- param_key$param[param_key$type=="stock"]

    # Stock-level parameters
    ###################################

    # Extract
    results_stock_orig <- results.df %>%
      filter(param %in% stock_params) %>%
      mutate(stockid=rep(stocks, length(stock_params))) %>%
      select(stockid, everything())

    # Format log-transformed results
    results_stock_logged <- results_stock_orig %>%
      filter(grepl("ln_", param)) %>%
      mutate(param=gsub("ln_", "", param)) %>%
      mutate_at(.vars=paste0("est", c("", "_se", "_lo", "_hi")), .funs=exp) %>%
      ungroup()

    # Format theta results
    results_stock_theta <- results_stock_orig %>%
      filter(param=="theta")

    # Merge
    results_stock <- bind_rows(results_stock_logged, results_stock_theta)

    # Global parameters
    ###################################

    # Format global results
    results_global <- results.df %>%
      filter(!param %in% stock_params)

    # Merge
    ###################################

    results <- list(stock=results_stock, global=results_global)

  }


  # Return
  ########################################

  # Return
  return(results)

}
