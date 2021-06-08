
#' Plot covariate-linked stock recruit fits
#'
#' This function plots stock recruit model fits and exports to a PDF.
#'
#' @param output SP model output
#' @param plotdir Path for plot export
#' @param plotname Plot name (must end with ".pdf)
#' @return PDF with production model fits
#' @export
plot_fits_sr_linked <- function(output, plotdir=getwd(), plotname="TMB_fits.pdf"){

  # Stocks do
  stocks_do <- sort(unique(output$data$stockid))

  # Data to plot
  data_plot <- output$data

  # Build lines
  #############################

  # Params
  results <- splink::get_results(output)
  srfits <- results %>%
    select(stockid, param, est) %>%
    spread(key="param", value="est")

  # SR type
  type <- output$type

  # Biomass values to evaluate
  b <- seq(0, 1, 0.01)

  # SST values to evaluate
  sst_vals <- seq(-1, 1, 0.5)

  # Create lines
  sr_lines <- purrr::map_df(1:nrow(srfits), function(x){

    # Parameters
    stockid <- srfits$stockid[x]
    alpha <- srfits$alpha[x]
    beta <- srfits$beta[x]
    theta <- srfits$theta[x]

    # Loop through SST values
    sr_lines1 <- purrr::map_df(sst_vals, function(sst_c){

      # Simulate data
      if(type=="ricker"){
        recruits <- alpha * b * exp(-beta*b) * exp(theta*sst_c)
      }
      if(type=="bev-holt"){
        recruits <- alpha * b / (beta + b) * exp(theta*sst_c)
      }

      # Record production
      z <- data.frame(stockid=stockid,
                      sst_c_scaled=sst_c,
                      b_scaled=b,
                      r_scaled=recruits)

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
  g <- ggplot(data_plot, aes(x=b_scaled, y=r_scaled, fill=sst_c_scaled)) +
    geom_point(pch=21, size=2, color="grey30") +
    # Line
    geom_line(data=sr_lines, mapping=aes(x=b_scaled, y=r_scaled, color=sst_c_scaled, group=sst_c_scaled), size=0.7, inherit.aes = F) +
    # Labels
    labs(x="Abundance (scaled)", y='Recruitment (scaled)') +
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
