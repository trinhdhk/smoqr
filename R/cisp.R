#' Centred iSplines to ensure that the median is 0
#' @description
#' This function is a soft wrapper over [splines2::isp()] but try to centre the basis
#' @param x `<numeric()` a vector
#' @param center.point `<numeric(1)>` a scalar determining the center of x; leave as NULL for get the median
#' @param ... passed to [splines2::isp()]
#' @seealso [splines2::isp()]
#' @importFrom splines2 isp
#' @export
cisp <- function(x, center.point = 0, ...){
  ncsp <- isp(x,...)
  if (is.null(center.point)) center.point <- median(x)
  med_ncsp <- head(ncsp[x==center.point, ],1)
  y <- ncsp - matrix(rep(med_ncsp, each=nrow(ncsp)), nrow=nrow(ncsp))
  attributes(y) <- attributes(ncsp)
  # attr(y, 'knots') <- attr(y, 'knots') - x[abs(x)==min(abs(x))]
  # attr(y, 'Boundary.knots') <- attr(y, 'Boundary.knots') - x[abs(x)==min(abs(x))]
  attr(y, 'center.point') <- center.point
  class(y) <- c('centerISpline', class(ncsp))
  y
}

#' Helper for model.frame
#' @description
#'
#' Inspired by https://cran.r-project.org/web/packages/splines2/vignettes/splines2-intro.html
#' @method makepredictcall centerISpline
#' @seealso [stats::makepredictcall()]
#' @importFrom splines2 isp
#' @importFrom stats makepredictcall
#' @export
makepredictcall.centerISpline <- function(var, call){

  if (as.character(call)[1L] == "cisp" ||
      (is.call(call) && identical(eval(call[[1L]]), cisp))) {
    at <- attributes(var)[c("center.point", "knots", "Boundary.knots", "intercept",
                            "degree", "derivs")]
    call <- call[1L:2L]
    call[names(at)] <- at
  }
  call
}

