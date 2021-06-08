
#' Fit stock recruit model using maximum likelihood estimation
#'
#' This function fits a Ricker or Beverton-Holt stock recruitment model using maximum likelihood estimation in TMB.
#'
#' @param data A data frame
#' @param type "ricker" or "bev-holt"
#' @return A list containing: (1) the data; (2) the model fit; and (3) the standard error report.
#' @examples
#' data <- splink::ram_wc
#' output <- splink::fit_sr_linked_fixed(data=data, b_col="b_scaled", r_col="r_scaled", cov_col="sst_c_scaled", type="ricker")
#' results <- splink::get_results(output)
#' splink::plot_results(results)
#' splink::plot_fits_sr(output, plotdir="~/Desktop", plotname="test.pdf")
#' @export
fit_sr_linked_fixed <- function(data, b_col, r_col, cov_col, type){

  # Perform checks
  if(!type %in% c("ricker", "bev-holt")){stop("The 'type' must be either 'ricker' or 'bev-holt'.")}

  # Parameters
  stocks <- unique(data$stockid)
  nstocks <- length(stocks)

  # Starting values
  if(type=="ricker"){
    params <- list(ln_alpha=rep(log(2), nstocks),
                   ln_beta=rep(log(0.3), nstocks),
                   ln_sigmaR=rep(0.1, nstocks),
                   theta=rep(0, nstocks))
  }
  if(type=="bev-holt"){
    params <- list(ln_alpha=rep(log(3), nstocks),
                   ln_beta=rep(log(2), nstocks),
                   ln_sigmaR=rep(0.1, nstocks),
                   theta=rep(0, nstocks))
  }

  # Input data
  if(type=="ricker"){SR_type <- 0}
  if(type=="bev-holt"){SR_type <- 1}
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     StockID=as.factor(data$stockid),
                     B_t=dplyr::pull(data[,b_col]),
                     R_t=dplyr::pull(data[,r_col]),
                     Cov_t=dplyr::pull(data[,cov_col]),
                     SR_type=SR_type) # 0=Ricker, 1=Beverton-Holt

  # Compile TMB model
  #######################################

  # TMB installed?
  tmb_check <- require(TMB, quietly=TRUE)
  if(tmb_check==FALSE){stop("Please install package `TMB` from CRAN.")}

  # Directories
  origdir <- getwd()
  tmbdir <- system.file("tmb", package="splink")
  rundir <- tempfile(pattern="run_", tmpdir=tempdir(), fileext="/")

  # Compile TMB model
  dir.create(rundir)
  file.copy(from=file.path(tmbdir, "srmodel_linked_fixed.cpp"), to=file.path(rundir, "srmodel_linked_fixed.cpp"), overwrite=FALSE)
  on.exit(setwd(origdir), add=TRUE)
  setwd(rundir)
  compile("srmodel_linked_fixed.cpp")
  dyn.load(TMB::dynlib(file.path(rundir, "srmodel_linked_fixed")))

  # Fit model
  #######################################

  # Load TMB code
  # tmbdir <- "inst/tmb"
  # dyn.load(TMB::dynlib(file.path(tmbdir, "srmodel_linked_random")))

  # Initialize model
  model <- TMB::MakeADFun(data=input.data, parameters=params, DLL="srmodel_linked_fixed")

  # Fit model
  fit <- TMBhelper::fit_tmb(obj=model, lower=-Inf, upper=Inf, loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)

  # Calculate SD
  hess <- optimHess(par=fit$par, fn=model$fn, gr=model$gr)
  sd <- try(TMB::sdreport(model, hessian.fixed=hess))


  # Export fit
  #######################################

  # Package data
  output <- list(data=data, fit=fit, sd=sd, type=type, effect="fixed")

  # Return
  setwd(origdir)
  return(output)

}
