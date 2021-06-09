
#' Fit surplus production model using maximum likelihood estimation
#'
#' This function fits a surplus production model using maximum likelihood estimation in TMB.
#'
#' @param data A prepped data frame with the following required columns: stockid, year, biomass_scaled, sp_scaled
#' @param p The shape parameter
#' @return A list containing: (1) the data; (2) the model fit; and (3) the standard error report.
#' @examples
#' data <- splink::ram_ne
#' output <- splink::fit_sp(data=data, b_col="tb_scaled", sp_col="sp_scaled", p=0.2)
#' results <- splink::get_results(output)
#' splink::plot_results(results)
#' splink::plot_fits(output)
#' @export
fit_sp_base <- function(data, b_col, sp_col, p=1){

  # Parameters
  stocks <- unique(data$stockid)
  nstocks <- length(stocks)

  # Starting values
  params <- list(ln_B0=rep(1.5, nstocks),
                 ln_r=rep(log(0.4), nstocks), # r=0.4 is pretty medium
                 ln_sigmaP=rep(-2.5, nstocks)) # -3 before, -1.25 based on model fits

  # Input data
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     p=p,
                     StockID=as.factor(data$stockid),
                     B_t=pull(data[,b_col]),
                     P_t=pull(data[,sp_col]))

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
  file.copy(from=file.path(tmbdir, "pella.cpp"), to=file.path(rundir, "pella.cpp"), overwrite=FALSE)
  on.exit(setwd(origdir),add=TRUE)
  setwd(rundir)
  compile("pella.cpp")
  dyn.load(TMB::dynlib(file.path(rundir, "pella")))

  # Fit model
  #######################################

  # Load TMB code
  # tmbdir <- "inst/tmb"
  # # tmbdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/code/tmb_code"
  # dyn.load(TMB::dynlib(file.path(tmbdir, "pella")))

  # Initialize model
  model <- TMB::MakeADFun(data=input.data, parameters=params, DLL="pella")

  # Fit model
  fit <- TMBhelper::fit_tmb(obj=model, lower=-Inf, upper=Inf, loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)

  # Calculate SD
  hess <- optimHess(par=fit$par, fn=model$fn, gr=model$gr)
  sd <- try(TMB::sdreport(model, hessian.fixed=hess))


  # Export fit
  #######################################

  # Package data
  output <- list(data=data, fit=fit, sd=sd)

  # Return
  setwd(origdir)
  return(output)

}
