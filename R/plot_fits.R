
#' Plot surplus production model fits
#'
#' This function plots surplus production model fits and exports to a PDF.
#'
#' @param output SP model output
#' @param b_col Biomass column
#' @param sp_col Production column
#' @param r_col Recruitment column
#' @param cov_col Covariate column
#' @param plotdir Path for plot export
#' @param plotname Plot name (must end with ".pdf)
#' @return PDF with production model fits
#' @export
plot_fits <- function(output,
                      b_col, sp_col=NULL, r_col=NULL, cov_col=NULL,
                      plotdir=getwd(), plotname="TMB_fits.pdf"){

  # Covariate?
  cov_yn <- !is.null(cov_col)

  # Production or recruitment?
  model_type <- ifelse(!is.null(sp_col), "production", "recruitment")

  # Plot models
  if(model_type=="production"){
    if(cov_yn==F){
      plot_fits_sp(output=output, b_col=b_col, sp_col=sp_col,
                   plotdir=plotdir, plotname=plotname)
    }else{
      plot_fits_sp_linked(output=output, b_col=b_col, sp_col=sp_col, cov_col=cov_col,
                          plotdir=plotdir, plotname=plotname)
    }
  }else{
    if(cov_yn==F){
      plot_fits_sr(output=output, b_col=b_col, r_col=r_col,
                   plotdir=plotdir, plotname=plotname)
    }else{
      plot_fits_sr_linked(output=output, b_col=b_col, r_col=r_col, cov_col=cov_col,
                          plotdir=plotdir, plotname=plotname)
    }
  }




}
