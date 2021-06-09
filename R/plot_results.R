
#' Plot parameter estimates and confidence intervals
#'
#' This function plots point estimates and confidence intervals for parameters in a covariate-linked biomass dynamic model fit. The plot shows histograms of the point estimates and spline plots showing the point estimates and their 95% confidence intervals.
#'
#' @param results Results data frame
#' @return Plot of parameter estimates and confidence intervals
#' @export
plot_results <- function(results){

  # Format data
  ####################################

  # Type
  type <- ifelse(length(results)==2, "random", "fixed")

  #  Format results
  if(type=="fixed"){
    results1 <- results %>%
      group_by(param) %>%
      arrange(param, desc(est)) %>%
      mutate(order=1:n()) %>%
      # Cap hi estimates
      mutate(est_cap=recode(param, "B0"=10, "r"=3, "sigmaP"=1,
                            "alpha"=15, "beta"=15, "sigmaR"=2, theta=10) %>% as.numeric(),
             est_hi_cap=pmin(est_cap, est_hi))
  }else{
    results1 <- results$stock %>%
      group_by(param) %>%
      arrange(param, desc(est)) %>%
      mutate(order=1:n()) %>%
      # Cap hi estimates
      mutate(est_cap=recode(param, "B0"=10, "r"=3, "sigmaP"=1,
                            "alpha"=15, "beta"=15, "sigmaR"=2, theta=10) %>% as.numeric(),
             est_hi_cap=pmin(est_cap, est_hi))
  }

  # Number of parameters
  nparams <- n_distinct(results1$param)

  # Plot data
  ####################################

  # Base theme
  base_theme <- theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"))

  # Histograms
  g1 <- ggplot(results1, aes(x=est)) +
    facet_wrap(~param, scales="free", ncol=nparams) +
    geom_histogram() +
    # Labels
    labs(x="Estimate", y="Number of stocks") +
    # Theme
    theme_bw() + base_theme
  g1

  # Spline plots
  g2 <- ggplot(results1, aes(y=order, x=est)) +
    facet_wrap(~param, scales="free", ncol=nparams) +
    geom_errorbar(data=results1, mapping=aes(y=order, xmin=est_lo, xmax=est_hi_cap), inherit.aes = F, color="grey70") +
    geom_point() +
    # Labels
    labs(x="Estimate", y="Stock") +
    # Theme
    theme_bw() + base_theme
  g2

  # Merge
  g <- gridExtra::grid.arrange(g1, g2, nrow=2, heights=c(0.3, 0.7))
  g

  # Return
  return(g)

}
