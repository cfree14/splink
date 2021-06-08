
#' Plot surplus production data
#'
#' This function prepares a quick plot of the provided surplus production data. The dataframe must contain a column called "stockid" and a column called "year". There must also be a column containing biomass time series and a column containing surplus production time series. If a covariate is provided (optional), points will be colored based on the covariate.
#'
#' @param data Data
#' @param b_col Name of column containing biomass time series
#' @param sp_col Name of column containing surplus production time series
#' @param cov_col Name of column containing covariate time series (optional)
#' @param n Number of stocks to plot (defaults to 12 or fewer)
#' @return A plot showing the biomass-production relationship for each stock
#' @examples
#' data <- splink::ram_ne
#' plot_raw(data, "tb_scaled", "sp_scaled")
#' plot_raw(data, "tb_scaled", "sp_scaled", "sst_c_scaled")
#' plot_raw(data, "tb_scaled", "sp_scaled", "sst_c_scaled", n=20)
#' @export
plot_raw <- function(data, b_col, sp_col, cov_col=NULL, n=NULL){

  # For testing
  # data <- splink::ram_ne; b_col <- "tb_scaled"; sp_col <- "sp_scaled"; cov_col <- "sst_c_scaled"; n=NULL

  # Stocks to plot
  n_default <- 12
  stockids <- sort(unique(data$stockid))
  nstocks <-length(stockids)
  n_plot <- ifelse(!is.null(n), n, pmin(nstocks,  n_default))
  stocks_do <- stockids[1:n_plot]

  # Covariate
  cov_yn <- !is.null(cov_col)

  # If covariate
  if(cov_yn){

    # Format data
    data_use <- data[, c("stockid", "year", b_col, sp_col, cov_col)] %>%
      # Rename
      setNames(c("stockid", "year", "biomass", "production", "covariate")) %>%
      # Filter
      filter(stockid %in% stocks_do)

    # Plot data
    g <- ggplot(data_use, aes(x=biomass, y=production, fill=covariate)) +
      facet_wrap(~stockid, ncol=4, scales="free") +
      geom_point(pch=21) +
      # Labels
      labs(x="Biomass", y="Surplus production") +
      # Lines
      geom_hline(yintercept=0) +
      # Limits
      lims(x=c(0,1)) +
      # Legend
      scale_fill_gradient2(name="Covariate", midpoint=0, high="darkred", low="navy", mid="white") +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      # Theme
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
    g

  # No covariate
  }else{

    # Format data
    data_use <- data[, c("stockid", "year", b_col, sp_col)] %>%
      # Rename
      setNames(c("stockid", "year", "biomass", "production")) %>%
      # Filter
      filter(stockid %in% stocks_do)

    # Plot data
    g <- ggplot(data_use, aes(x=biomass, y=production, fill=year)) +
      facet_wrap(~stockid, ncol=4, scales="free") +
      geom_point(pch=21) +
      # Labels
      labs(x="Biomass", y="Surplus production") +
      # Lines
      geom_hline(yintercept=0) +
      # Limits
      lims(x=c(0,1)) +
      # Legend
      scale_fill_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      # Theme
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
    g

  }

}
