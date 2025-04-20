#' Prepare the data for the fit
#' @export
._prepare_data_ <- function(data, boundary, grain, include_median=TRUE, all_intr=FALSE, isp_df, isp_degree){
  boundary <- sort(abs(boundary))
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
 dt <- do.call(rbind, args=data_stack)

 if (!all_intr) return(dt)
 ispq <- cisp(dt$lq, df = isp_df, degree=isp_degree, intercept=TRUE) |>
   as.data.frame()
 names(ispq) <- paste0('slq', seq_len(ncol(ispq)))
 cbind(dt, ispq)
}
