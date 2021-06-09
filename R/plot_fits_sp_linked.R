
#' Plot covariate-linked stock recruit fits
#'
#' This function plots stock recruit model fits and exports to a PDF.
#'
#' @param output SP model output
#' @param plotdir Path for plot export
#' @param plotname Plot name (must end with ".pdf)
#' @return PDF with production model fits
#' @export
plot_fits_sp_linked <- function(output, b_col, sp_col, cov_col,
                                plotdir=getwd(), plotname="TMB_fits.pdf"){

  # Stocks do
  stocks_do <- sort(unique(output$data$stockid))

  # Data to plot
  data_plot <- output$data %>%
    # Rename columns
    rename("b_scaled"=b_col, "sp_scaled"=sp_col, "cov_scaled"=cov_col)

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

  # Covariate values to evaluate
  cov_vals <- seq(-1, 1, 0.5)

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

  # Plot data
  #############################

  # Base theme
  my_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=8),
                     strip.text=element_text(size=6),
                     plot.title=element_blank(),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))

  # Plot data
  g <- ggplot(data_plot, aes(x=b_scaled, y=sp_scaled, fill=cov_scaled)) +
    geom_point(pch=21, size=2, color="grey30") +
    # Line
    geom_line(data=sp_lines, mapping=aes(x=b_scaled, y=sp_scaled, color=cov_scaled, group=cov_scaled), size=0.7, inherit.aes = F) +
    # Labels
    labs(x="Abundance (scaled)", y='Surplus production (scaled)') +
    # Horizontal guide
    geom_hline(yintercept=0, linetype="dotted", color="black") +
    # Legend
    scale_fill_gradient2(name="Covariate\nobservations", midpoint=0, mid="white", high="darkred", low="navy") +
    scale_color_gradient2(name="Covariate\ninfluence", midpoint=0, mid="grey80", high="darkred", low="navy") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black"),
           color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme +
    # Paginate
    ggforce::facet_wrap_paginate(~stockid, scales="free_y", ncol = 4, nrow = 7, page=1)

  # Number of pages
  npages <- ggforce::n_pages(g)

  # Loop through pages
  pdf(file.path(plotdir, plotname), paper= "letter", width = 7.5, height=11)
  for(i in 1:npages){
    print(g + ggforce::facet_wrap_paginate(~stockid, scales="free_y", ncol = 4, nrow = 7, page=i))
  }
  dev.off()

}
