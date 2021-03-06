% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_sp.R
\name{calc_sp}
\alias{calc_sp}
\title{Calculate surplus production time series}
\usage{
calc_sp(biomass, catch)
}
\arguments{
\item{biomass}{A time series of biomass}

\item{catch}{A time series of catch (in same units as biomass)}
}
\value{
A time series of surplus production (in same units as biomass/catch)
}
\description{
This function calculates a time series of surplus production from time series of biomass and catch: SP(t) = TB(t+1) - TB(t) + C(t). Biomass and catch must be in the same units.
}
