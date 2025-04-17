#' Adding quantile recipe for prediction
#' @description
#' This function adds quantile recipe to the data frame, prepares for prediction.
#'
#' @param x `<data.frame>` a new data frame for prediction
#' @param quantiles `<numeric()>` a vector of quantiles to predict, must be between 0 and 1
#' @return a `<data.frame>` with extra column of quantile for prediction
#' @export
add_quantiles <- function(x, quantiles){
  if ('q' %in% names(x) || 'lq' %in% names(x)) {
    stop('q and lq are reserved names for model and must not be existing in the dataset.')
  }
  stopifnot(all(quantiles>0) && all(quantiles<1))
  lapply(quantiles, function(Q){
    transform(x, q=Q, lq=qlogis(Q))
  }) |>
    do.call(rbind, args=_)
}
