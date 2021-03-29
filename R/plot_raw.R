
#' Plot sample of prepped surplus production data
#'
#' This function plots a sample of prepped surplus production data.
#'
#' @param data Prepped data
#' @param n Number of stocks to plot data for
#' @return Plot of SP curves for N stocks
#' @export
plot_raw <- function(data, n=NULL){

  # Number of stocks to plot
  stockids <- sort(unique(data$stockid))
  nstocks <-length(stockids)
  n_plot <- ifelse(!is.null(n), n, nstocks)

  # Subset data
  stocks_do <- stockids[1:n_plot]
  sdata <- data %>%
    filter(stockid %in% stocks_do)

  # Plot data
  g <- ggplot(sdata, aes(x=biomass_scaled, y=sp_scaled)) +
    facet_wrap(~stockid, ncol=4, scales="free") +
    geom_point() +
    # Labels
    labs(x="Biomass\n(scaled to maximum biomass)", y="Surplus production\n(scaled to maximum biomass)") +
    # Lines
    geom_hline(yintercept=0) +
    # Limits
    lims(x=c(0,1)) +
    # Theme
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  g

}
