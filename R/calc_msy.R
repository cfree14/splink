
#' Calculate covariate-linked MSY over time
#'
#' This function calculates MSY.
#' @param output Output of model fit
#' @param cov_df Data frame containing: stockid, year, cov_col
#' @export
calc_msy <- function(output, cov_df){

  # For testsing
  # output <- splink::splink_model; cov_df <- output$data

  # Add cov_col for covariate dataframe. Add grouping?

  # Model type
  model_type <- output$cov_effect

  # Parameters
  ntrajs <- 1000

  # If fixed....
  if(model_type=="fixed"){

    # Extract results
    results <- splink::get_results(output)
    stocks <- results %>%
      select(stockid, param, est) %>%
      spread(key="param", value="est")
    stockids <- stocks$stockid

    # Extract sd
    sd <- output$sd

    # Extract covariance matrix
    covmat <- sd$cov.fixed

    # Isolate the theta portion of covariance matrix
    covmat_theta <- covmat[rownames(covmat)=="theta", colnames(covmat)=="theta"]

    # Draw random values
    # [each simulation, each param]
    theta_sim <- mvtnorm::rmvnorm(n=ntrajs, mean=stocks$theta, sigma=covmat_theta) %>% as.matrix()
    colnames(theta_sim) <- stockids
    rownames(theta_sim) <- 1:ntrajs

  }

  # If random....
  if(model_type=="random"){

    # Step 1. Simulate theta using covariance matrix
    #########################################################

    # Extract results
    results_full <- splink::get_results(output)
    results <- results_full$stock
    stocks <- results %>%
      select(stockid, param, est) %>%
      spread(key="param", value="est")
    stockids <- stocks$stockid

    # Extract sd
    sd <- output$sd

    # Extract joint precision matrix
    joint_inv_covmat <- sd$jointPrecision

    # Invert the joint precision matrix
    joint_covmat <- solve(joint_inv_covmat)

    # Isolate the theta portion of covariance matrix
    joint_covmat_theta <- joint_covmat[rownames(joint_covmat)=="theta", colnames(joint_covmat)=="theta"]

    # Draw random values
    # [each simulation, each param]
    theta_sim <- mvtnorm::rmvnorm(n=ntrajs, mean=stocks$theta, sigma=joint_covmat_theta) %>% as.matrix()
    colnames(theta_sim) <- stockids
    rownames(theta_sim) <- 1:ntrajs

    # Check that simulated data has the same mean
    mean_sim <- apply(theta_sim, 2, mean)
    plot(mean_sim ~ stocks$theta)

  }

  # Step 2. Simulate population trajectories using each simulated theta
  #########################################################

  # Loop through stocks
  x <- stockids[1]
  data_all <- purrr::map_df(stockids, function(x){

    # Stock
    stock <- x

    # Extract data
    sdata <- output$data %>%
      filter(stockid==stock)

    # Prep covariate values
    covdata <- cov_df %>%
      filter(stockid==stock)
    cov_vals <- covdata$sst_c_scaled

    #  Extract parameters
    p <- output$p
    div <- (p+1)^((p+1)/p)
    r <- stocks$r[stocks$stockid==stock]
    K_scaled <- stocks$B0[stocks$stockid==stock]
    B_max <- max(sdata$b)
    K <- K_scaled * B_max

    # Extrac thetas
    thetas <- theta_sim[,stock]

    # Calculate MSY for each theta
    msys <- sapply(thetas, function(x) r * K / div * exp(cov_vals * x))

    # Format MSYS
    msy_df <- msys %>%
      as.data.frame() %>%
      mutate(stockid=stock, year=covdata$year) %>%
      select(stockid, year, everything()) %>%
      gather(key="id", value="msy", 3:ncol(.))

  })

  # Calculate MSY stock-level
  msy_stock <- data_all %>%
    group_by(stockid, year) %>%
    summarize(msy_md=median(msy),
              msy_lo=quantile(msy, probs=(0.025)),
              msy_hi=quantile(msy, probs=(0.975))) %>%
    ungroup()

  # Calculate MSY global
  msy_global <- data_all %>%
    # Calculate sums
    group_by(year, id) %>%
    summarise(msy=sum(msy)) %>%
    ungroup() %>%
    # Calculate median and confidence intervals
    group_by(year) %>%
    summarize(msy_md=median(msy),
              msy_lo=quantile(msy, probs=(0.025)),
              msy_hi=quantile(msy, probs=(0.975))) %>%
    ungroup()

  # Plot MSY stock
  if(F){
    g_stock <- ggplot(msy_stock, aes(x=year, y=msy_md)) +
      facet_wrap(~stockid, ncol=5, scales="free_y") +
      geom_ribbon(mapping=aes(x=year, ymin=msy_lo, ymax=msy_hi), fill="grey80") +
      geom_line() +
      # Labels
      labs(x="Year", y="MSY") +
      # Theme
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
    print(g_stock)
  }

  # Plot global
  g <- ggplot(msy_global, aes(x=year, y=msy_md/1e6)) +
    geom_ribbon(mapping=aes(x=year, ymin=msy_lo/1e6, ymax=msy_hi/1e6), fill="grey80") +
    geom_line() +
    # Labels
    labs(x="Year", y="MSY (millions)") +
    # Theme
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  print(g)

  # Return
  out <- list(msy_stock=msy_stock, msy_global=msy_global)
  return(out)


}
