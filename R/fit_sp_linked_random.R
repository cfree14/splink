
#' Fit surplus production model using maximum likelihood estimation
#'
#' This function fits a surplus production model using maximum likelihood estimation in TMB.
#'
#' @param data Data
#' @param b_col Name of biomass column
#' @param sp_col Name of surplus production column
#' @param p Shape parameter
#' @param cov_col Name of covariate column
#' @return A list containing: (1) the data; (2) the model fit; and (3) the standard error report.
#' @examples
#' data <- splink::ram_ne
#' output <- splink::fit_sp_linked_random(data=data, b_col="tb_scaled", sp_col="sp_scaled", p=1, cov_col="sst_c_scaled")
#' results <- splink::get_results(output)
#' splink::plot_results(results)
#' splink::plot_thetas(results)
#' splink::plot_fits_sp_linked(output, b_col="tb_scaled", sp_col="sp_scaled", cov_col="sst_c_scaled")
#' @export
fit_sp_linked_random <- function(data, b_col, sp_col, p=1, cov_col){

  # Parameters
  stocks <- unique(data$stockid)
  nstocks <- length(stocks)

  # Starting values
  params <- list(ln_B0=rep(1.5, nstocks),
                 ln_r=rep(log(0.4), nstocks),
                 theta=rep(0.0, nstocks),
                 ln_sigmaP=rep(-2.5, nstocks),
                 mu_T=0.0,
                 ln_sd_T=-1.25) # -3 before, -1.25 based on model fits

  # Input data
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     p=p,
                     StockID=as.factor(data$stockid),
                     B_t=pull(data[,b_col]),
                     P_t=pull(data[,sp_col]),
                     Cov_t=pull(data[,cov_col]))

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
  file.copy(from=file.path(tmbdir, "pella_linked_random.cpp"), to=file.path(rundir, "pella_linked_random.cpp"), overwrite=FALSE)
  on.exit(setwd(origdir),add=TRUE)
  setwd(rundir)
  compile("pella_linked_random.cpp")
  dyn.load(TMB::dynlib(file.path(rundir, "pella_linked_random")))

  # Fit model
  #######################################

  # Load TMB code
  # tmbdir <- "inst/tmb"
  # # tmbdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/code/tmb_code"
  # dyn.load(TMB::dynlib(file.path(tmbdir, "pella")))

  # Initialize model
  model <- TMB::MakeADFun(data=input.data, parameters=params, random="theta", DLL="pella_linked_random")

  # Fit model
  fit <- TMBhelper::fit_tmb(obj=model, lower=-Inf, upper=Inf, loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)

  # Calculate SD
  hess <- optimHess(par=fit$par, fn=model$fn, gr=model$gr)
  sd <- try(TMB::sdreport(model, hessian.fixed=hess))


  # Export fit
  #######################################

  # Package data
  output <- list(data=data, fit=fit, sd=sd, p=p, cov_effect="random")

  # Return
  setwd(origdir)
  return(output)

}
