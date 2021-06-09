
#' Fit stock recruit model using maximum likelihood estimation
#'
#' This function fits a Ricker or Beverton-Holt stock recruitment model using maximum likelihood estimation in TMB.
#'
#' @param data A data frame
#' @param b_col Name of the column with biomass
#' @param r_col Name of the column with recruitment
#' @param type Stock recruit type: "ricker" or "bev-holt"
#' @param cov_col Name of the column with the covariate
#' @return A list containing: (1) the data; (2) the model fit; and (3) the standard error report.
#' @examples
#' data <- splink::ram_wc
#' output <- splink::fit_sr(data=data, b_col="b_scaled", r_col="r_scaled", type="ricker")
#' results <- splink::get_results(output)
#' splink::plot_results(results)
#' splink::plot_fits_sr(output, b_col="b_scaled", r_col="r_scaled", plotdir="~/Desktop", plotname="test.pdf")
#' @export
fit_sr <- function(data, b_col, r_col, type, cov_col=NULL, cov_effect=NULL){

  # Perform checks
  if(!type %in% c("ricker", "bev-holt")){stop("The 'type' must be either 'ricker' or 'bev-holt'.")}
  if(!is.null(cov_effect)){
    if(!cov_effect %in% c("fixed", "random")){stop("The 'cov_effect' must be either 'fixed' or 'random'.")}
  }

  # Which model to fit?
  model2fit <- ifelse(is.null(cov_effect), "base",
                      ifelse(cov_effect=="fixed", "fixed", "random"))

  # Fit model
  if(model2fit=="base"){
    output <- splink::fit_sr_base(data=data, b_col="b_scaled", r_col="r_scaled", type=type)
  }
  if(model2fit=="fixed"){
    output <- splink::fit_sr_linked_fixed(data=data, b_col="b_scaled", r_col="r_scaled", type=type, cov_col=cov_col)
  }
  if(model2fit=="random"){
    output <- splink::fit_sr_linked_random(data=data, b_col="b_scaled", r_col="r_scaled", type=type, cov_col=cov_col)
  }

  # Return output
  return(output)

}
