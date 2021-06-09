
#' Compare models with AIC
#'
#' This function compares models with AIC.
#'
#' @param models List with models
#' @param names Names of models
#' @return A table with AIC comparison
#' @export
compare_models <- function(models, names){

  # Loop through models
  data <- purrr::map_df(1:length(models), function(i){

    # Model
    name <- names[i]
    output <- models[[i]]$fit

    # Extract values
    k <- length(output[["par"]])
    lik <- output[["objective"]]
    aic_val <- TMBhelper::TMBAIC(output)

    # Record data
    df <- tibble(model=name,
                 k=k,
                 nll=lik,
                 aic=aic_val)

  })

  # Calculate delta AIC
  stats <- data %>%
    # Add delta AIC
    mutate(daic=aic-min(aic)) %>%
    # Sort by delta AIC
    arrange(daic)

  # Return
  return(stats)

}
