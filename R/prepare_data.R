#' Prepare the data for the fit
#' @export
._prepare_data_ <- function(data, boundary, grain, include_median=TRUE){
  # Create a grid of quantiles
  Qs <- seq(boundary[[1]], boundary[[2]], length.out=grain)
  if (include_median && !0.5 %in% Qs) Qs <- sort(c(0.5, Qs))
  if ('q' %in% names(data) || 'lq' %in% names(data)) {
    stop('q and lq are reserved names for model and must not be existing in the dataset.')
  }
  data_stack <- lapply(
    Qs,
    function(Q){
      transform(data, q=Q, lq=qlogis(Q))
    }
  )
  do.call(rbind, args=data_stack)
}
