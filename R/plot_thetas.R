
#' Plot theta estimates and confidence intervals
#'
#' This function plots point estimates and confidence intervals for the covriate influence parameter (theta) in a covariate-linked model fit. The spline plot shows the point estimates and their 95% confidence intervals.
#'
#' @param results Results data frame
#' @return Plot of theta estimates and confidence intervals
#' @export
plot_thetas <- function(results){

  # Format data
  data <- results %>%
    # Reduce
    filter(param=="theta") %>%
    # Reduce
    select(stockid, est, est_lo, est_hi) %>%
    # Order
    arrange(desc(est)) %>%
    mutate(stockid=factor(stockid, levels=stockid)) %>%
    # Add sig
    mutate(est_inf=ifelse(est_hi<0, "negative",
                          ifelse(est_lo>0, "positive", "none")))

  # Spline bars
  g <- ggplot() +
    # Vertical
    geom_vline(xintercept = 0, color="grey30") +
    # Lines
    geom_errorbar(data=data, mapping=aes(y=stockid, xmin=est_lo, xmax=est_hi, color=est_inf, width=0, alpha=0.5)) +
    geom_point(data=data, mapping=aes(y=stockid, x=est, color=est_inf)) +
    # Labels
    labs(x="Covariate effect", y="") +
    # Legend
    scale_color_manual(name="Covariate effect", values=c("red", "black", "blue")) +
    guides(alpha=F) +
    # Theme
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  g

}

