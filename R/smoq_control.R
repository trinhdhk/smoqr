#' Setting controls for smoq
#' @description
#' This function setting control parameters specific for smoq
#' @param boundary_quantiles <integer(2)> the boundary quantiles to consider
#' @param grain <integer> how many intermediate quantile to discretise
#' @return object of class `SmoqCtrl`
#' @export
smoq_control <- function(boundary_quantiles = c(0.05, 0.95), grain=20L){
  default_control <- list(boundary_quantiles = c(0.05, 0.95), grain=20L)
  if (!is.numeric(grain)) grain<-20L
  if (!is.numeric(boundary_quantiles)) boundary_quantiles <- c(0.05, 0.95)
  structure(
    modifyList(default_control, list(boundary_quantiles=sort(boundary_quantiles), grain=round(grain))),
    class='SmoqCtrl'
  )
}
