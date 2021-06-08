
#' Plot surplus production model fits
#'
#' This function plots surplus production model fits and exports to a PDF.
#'
#' @param output SP model output
#' @param plotdir Path for plot export
#' @param plotname Plot name (must end with ".pdf)
#' @return PDF with production model fits
#' @export
plot_fits_nls <- function(output, plotdir=getwd(), plotname="NLS_SP_fits.pdf"){

  # Stocks do
  stocks_do <- sort(unique(output$data$stockid))

  # Data to plot
  data_plot <- output$data[, c("stockid", "year", output$b_col, sp_col)] %>%
    setNames(c("stockid", "year", "biomass", "sp"))

  # Build lines
  #############################

  # Params
  spfits <- output$fits %>%
    filter(!is.na(r) & !is.na(k))

  # Create lines
  sp_lines <- purrr::map_df(1:nrow(spfits), function(x){

    # Parameters
    stockid <- spfits$stockid[x]
    r <- spfits$r[x]
    k <- spfits$k[x]
    p <- spfits$p[x]
    max_b <- spfits$biomass_max[x]

    # Simulate data
    b <- seq(0, max_b, length.out = 100)
    sp <- r/p * b * (1-(b/k)^p)

    # Record production
    z <- data.frame(stockid=stockid,
                    biomass=b,
                    sp=sp)

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
  g <- ggplot(data_plot, aes(x=biomass, y=sp)) +
    geom_point(pch=21, size=2, color="grey30") +
    # Line
    geom_line(data=sp_lines, mapping=aes(x=biomass, y=sp), color="black", size=0.7) +
    # Labels
    labs(x="Biomass", y='Surplus production') +
    # Horizontal guide
    geom_hline(yintercept=0, linetype="dotted", color="black") +
    # Theme
    theme_bw() + my_theme +
    # Paginate
    ggforce::facet_wrap_paginate(~stockid, scales="free", ncol = 4, nrow = 7, page=1)

  # Number of pages
  npages <- ggforce::n_pages(g)

  # Loop through pages
  pdf(file.path(plotdir, plotname), paper= "letter", width = 7.5, height=11)
  for(i in 1:npages){
    print(g + ggforce::facet_wrap_paginate(~stockid, scales="free_y", ncol = 4, nrow = 7, page=i))
  }
  dev.off()


}
