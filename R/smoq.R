#' Fit a semi-parametric smooth quantile regression using brms
#' @description
#' This function fit a non-crossing smooth quantile regression by using discretisation-interpolation on a spectrum of quantile
#' @param formula `<formula|list(<formula>(2))>`
#' the main linear formula of the model, can include mixed effects but need tested. Non-linear formula is not allowed.
#' An alternative is a named list of 2 formula. $main for the quantile-independent functional formula and $interaction for the interaction between features and quantiles
#' @param dist_formulae `<list(<formula>)>` a list of other distributional formula, except for quantile. See [brms::asym_laplace()]
#' @param data `<data.frame>` dataset
#' @param dist `<NA|character(1)|function>`
#' the assume distribution of response variable.
#' the default `NA` will use a monotonic cubic splines `splines::isp` with `isp_df` degrees of freedom to fit the
#' inverse cumulative probability density function of response over `plogis(quantile)`.
#' if is a string, there should exist a function `q<dist>` to calculate the inverse cumulative probability density function
#' if a function, must be a function that calculate the inverse cumulative probability density function
#' @param isp_df `<NA|integer>` the degrees of freedom to fit the monotonic splines, ignored if `!is.na(dist)`
#' @param isp_degree `<NA|integer>` the degrees of M-spline function to fit the monotonic splines, ignored if `!is.na(dist)`
#' @param smoq_control `<smoq_control>` generated from smoq_control.
#' Currently configurable are the boundary quantiles and the number of intermediate quantiles to discretise, governed by grain
#' @param .verbose `<logical(1)>` debug
#' @param ... other parameters passed to `brms::brm`, commonly used are: `prior, iter, warmup, cores, chains, threading`. See [brms::brm()].
#' Note that brms/Stan seldom needs more than 10,000 iterations. For simple models, the default 2000 is sufficient. For simple linear regression, 1000 is more than enough.
#' @return a `brmsfit` object
#' @import brms
#' @importFrom splines2 isp
#' @importFrom formula.tools lhs
#' @importFrom rlang new_formula
#' @seealso [brms::brm()]
#' @export
smoq <- function(
    formula,
    dist_formulae = list(),
    data,
    dist=NA,
    isp_df = 4,
    isp_degree = 3,
    smoq_control = NULL,
    ...){


  # Check distribution
  use_spline <- FALSE
  if (!is.function(dist)){
    use_spline <- is.na(dist)
    if (!is.na(dist) && is.character(dist)){
      dist <- tryCatch(get(paste0('q', dist)), error=function(e){
        stop(sprintf('q%s not found.', dist))
      })
    }
  }

  # Prepare data
  if(is.null(smoq_control)) smoq_control <- smoq_control()
  stopifnot(inherits(smoq_control, 'SmoqCtrl'))
  stack_data <- ._prepare_data_(data, smoq_control$boundary_quantiles, smoq_control$grain)

  # Prepare formulae
  if (!is.list(formula)) {
    formula$main <- formula$interaction <- formula
  }
  # if (is.list(formula)){
    formula_lp <- update(formula$main, lp ~ .)
    formula_interaction <- update(formula$interaction, intr ~ 0 + .)
  # }
  # else {
    # formula_lp <- update(formula, lp ~ .)
    # formula_interaction <- update(formula, intr ~ 0 + .)
  # }


  formula_quantile <-
    if (use_spline){
      if (smoq_control$grain <= 5 && isp_degree >= 3)
        warning('Using small number of grain with cubic spline may be unstable. It is advisable to set isp_degree to 2 or increase grain.')
        substitute(quant ~ 0 + cisp(lq, df=df, degree=degree), env = list(df=isp_df, degree=isp_degree))
    }
    else
      quant ~ 0 + dist(q)
      # substitute(quant ~ 0 + dist(q), env=list(dist=dist))
  main_form <- new_formula(lhs(formula$main), quote(lp + exp(intr)*quant))

  # Create the main formula
  bf_forms <- bf(main_form, nl=TRUE,
                 flist = list(
                   formula_lp,
                   formula_interaction,
                   formula_quantile,
                   quantile ~ 0 + offset(lq)
                 ))


  # Add more distributional formulae
  for (dform in dist_formulae){
    bf_forms <- bf_forms + dform
  }


  # Run backend
  fit <- brm(
    formula = bf_forms,
    data = stack_data,
    family=asym_laplace(),
    ...
  )
  class(fit) <- c('smoqfit', class(fit))
  fit
}
