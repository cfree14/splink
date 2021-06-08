
#' Plot stock recruit model fits
#'
#' This function plots stock recruit model fits and exports to a PDF.
#'
#' @param output SR model output
#' @param plotdir Path for plot export
#' @param plotname Plot name (must end with ".pdf)
#' @return PDF with production model fits
#' @export
plot_fits_sr <- function(output, plotdir=getwd(), plotname="TMB_fits.pdf"){

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

  # Create lines
  sr_lines <- purrr::map_df(1:nrow(srfits), function(x){

    # Parameters
    stockid <- srfits$stockid[x]
    alpha <- srfits$alpha[x]
    beta <- srfits$beta[x]

    # Simulate data
    b <- seq(0, 1, 0.01)
    if(type=="ricker"){
      recruits <- alpha * b * exp(-beta*b)
    }
    if(type=="bev-holt"){
      recruits <- alpha * b / (beta + b)
    }

    # Record production
    z <- data.frame(stockid=stockid,
                    b_scaled=b,
                    r_scaled=recruits)

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
  g <- ggplot(data_plot, aes(x=b_scaled, y=r_scaled)) +
    geom_point(pch=21, size=2, color="grey30") +
    # Line
    geom_line(data=sr_lines, mapping=aes(x=b_scaled, y=r_scaled), color="black", size=0.7) +
    # Labels
    labs(x="Spawners (scaled)", y='Recruits (scaled)') +
    # Horizontal guide
    geom_hline(yintercept=0, linetype="dotted", color="black") +
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
