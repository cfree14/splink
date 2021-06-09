
#' Fit surplus production model using maximum likelihood estimation
#'
#' This function fits a surplus production model using maximum likelihood estimation in TMB.
#'
#' @param data Data
#' @param b_col Name of biomass column
#' @param sp_col Name of surplus production column
#' @param p Shape parameter
#' @param cov_col Name of covariate column
#' @param cov_effect Type of covariate effect: "fixed" or "random"
#' @return A list containing: (1) the data; (2) the model fit; and (3) the standard error report.
#' @examples
#' data <- splink::ram_ne
#' output <- splink::fit_sp(data=data, b_col="tb_scaled", sp_col="sp_scaled", p=0.2)
#' results <- splink::get_results(output)
#' splink::plot_results(results)
#' splink::plot_fits(output)
#' @export
fit_sp <- function(data, b_col, sp_col, p=1, cov_col=NULL, cov_effect=NULL){

  # Perform checks
  if(!is.null(cov_effect)){
    if(!cov_effect %in% c("fixed", "random")){stop("The 'cov_effect' must be either 'fixed' or 'random'.")}
  }

  # Which model to fit?
  model2fit <- ifelse(is.null(cov_effect), "base",
                      ifelse(cov_effect=="fixed", "fixed", "random"))

  # Fit model
  if(model2fit=="base"){
    output <- splink::fit_sp_base(data=data, b_col=b_col, sp_col=sp_col, p=p)
  }
  if(model2fit=="fixed"){
    output <- splink::fit_sp_linked_fixed(data=data, b_col=b_col, sp_col=sp_col, p=p, cov_col=cov_col)
  }
  if(model2fit=="random"){
    output <- splink::fit_sp_linked_random(data=data, b_col=b_col, sp_col=sp_col, p=p, cov_col=cov_col)
  }

  # Return output
  return(output)

}
