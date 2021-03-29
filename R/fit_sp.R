
#' Fit surplus production model using maximum likelihood estimation
#'
#' This function fits a surplus production model using maximum likelihood estimation in TMB.
#'
#' @param data A prepped data frame with the following required columns: stockid, year, biomass_scaled, sp_scaled
#' @param p The shape parameter
#' @return A list containing: (1) the data; (2) the model fit; and (3) the standard error report.
fit_sp <- function(data, p=1){

  # Parameters
  stocks <- unique(data$stockid)
  nstocks <- length(stocks)

  # Load TMB code
  tmbdir <- "inst/tmb"
  # tmbdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/forage_fish/code/tmb_code"
  dyn.load(TMB::dynlib(file.path(tmbdir, "pella")))

  # Starting values
  params <- list(ln_B0=rep(1.5, nstocks),
                 ln_r=rep(log(0.4), nstocks), # r=0.4 is pretty medium
                 ln_sigmaP=rep(-2.5, nstocks)) # -3 before, -1.25 based on model fits

  # Input data
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     p=p,
                     StockID=as.factor(data$stockid),
                     B_t=data$biomass_scaled,
                     P_t=data$sp_scaled)

  # Initialize model
  model <- TMB::MakeADFun(data=input.data, parameters=params, DLL="pella")

  # Fit model
  fit <- TMBhelper::fit_tmb(obj=model, lower=-Inf, upper=Inf, loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)

  # Calculate SD
  hess <- optimHess(par=fit$par, fn=model$fn, gr=model$gr)
  sd <- try(TMB::sdreport(model, hessian.fixed=hess))

  # Package data
  output <- list(data=data, fit=fit, sd=sd)

  # Return
  return(output)

}
