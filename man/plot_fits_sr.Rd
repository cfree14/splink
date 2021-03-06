% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_fits_sr.R
\name{plot_fits_sr}
\alias{plot_fits_sr}
\title{Plot stock recruit model fits}
\usage{
plot_fits_sr(
  output,
  b_col,
  r_col,
  plotdir = getwd(),
  plotname = "TMB_fits.pdf"
)
}
\arguments{
\item{output}{SR model output}

\item{plotdir}{Path for plot export}

\item{plotname}{Plot name (must end with ".pdf)}
}
\value{
PDF with production model fits
}
\description{
This function plots stock recruit model fits and exports to a PDF.
}
