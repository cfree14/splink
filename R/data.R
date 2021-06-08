
#' RAM biomass and catch time series
#'
#' RAM biomass and catch time series.
#'
#' @format A data frame with the following attributes::
#' \describe{
#'   \item{stockid}{Stock id}
#'   \item{year}{Year}
#'   \item{biomass_units}{Biomass units}
#'   \item{biomass}{Biomass}
#'   \item{catch_units}{Catch units}
#'   \item{catch}{Catch}
#' }
"ram"

#' New England fish stock data
#'
#' Biomass, production, and temperature time series for 25 fish stocks in New England.
#'
#' @format A data frame with the following attributes::
#' \describe{
#'   \item{stockid}{Stock id}
#'   \item{year}{Year}
#'   \item{catch_mt}{Catch, in metric tons}
#'   \item{tb_mt}{Total biomass, in metric tons}
#'   \item{sp_mt}{Surplus production, in metric tons}
#'   \item{sst_c}{Sea surface temperature, in degrees Celcius}
#'   \item{tb_scaled}{Total biomass, scaled to maximum biomass}
#'   \item{sp_scaled}{Surplus production, scaled to maximum biomass}
#'   \item{sst_c_scaled}{Sea surface temperature, centered}
#' }
"ram_ne"
