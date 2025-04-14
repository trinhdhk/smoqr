#' Centred iSplines to ensure that the median is 0
#' @description
#' This function is a soft wrapper over [splines2::isp()] but try to centre the basis
#' @param x `<numeric()` a vector
#' @param ... passed to [splines2::isp()]
#' @seealso [splines2::isp()]
#' @importFrom splines2 isp
#' @export
cisp <- function(x,...){
  ncsp <- isp(x,...)
  med_ncsp <- head(ncsp[abs(x)==min(abs(x)), ],1)
  ncsp - matrix(rep(med_ncsp, each=nrow(ncsp)), nrow=nrow(ncsp))
}
