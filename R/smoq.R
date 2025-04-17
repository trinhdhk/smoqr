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
#' @param interaction_type `<character(1)>` `"scale_only"` to assume the same skewness and kurtosis (default), otherwise `"all"`
#' Currently configurable are the boundary quantiles and the number of intermediate quantiles to discretise, governed by grain
#' @param smoq_control `<smoq_control>` generated from smoq_control.
#' @param .verbose `<logical(1)>` debug
#' @param ... other parameters passed to `brms::brm`, commonly used are: `prior, iter, warmup, cores, chains, threading`. See [brms::brm()].
#' Note that brms/Stan seldom needs more than 10,000 iterations. For simple models, the default 2000 is sufficient. For simple linear regression, 1000 is more than enough.
#' @return a `brmsfit` object
#' @import brms
#' @importFrom splines2 isp
#' @importFrom formula.tools lhs rhs
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
    interaction_type = c('scale_only', 'all'),
    smoq_control = NULL,
    ...){

  # Init
  interaction_type <- match.arg(interaction_type)

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

  if (interaction_type == 'all' && !use_spline){
    stop("Don't know how to parametrise kurtosis without spline.")
  }

  # Prepare data
  if(is.null(smoq_control)) smoq_control <- smoq_control()
  stopifnot(inherits(smoq_control, 'SmoqCtrl'))
  stack_data <- ._prepare_data_(data,
                                smoq_control$boundary_quantiles,
                                smoq_control$grain,
                                all_intr = interaction_type=='all',
                                isp_df = isp_df, isp_degree = isp_degree)

  # Prepare formulae
  if (!is.list(formula)) {
    formula$main <- formula$interaction <- formula
  }
  formula_lp <- update(formula$main, lp ~ .)


  formula_quantile <-
      if (use_spline){
        if (smoq_control$grain <= 5 && isp_degree >= 3)
          warning('Using small number of grain with cubic spline may be unstable. It is advisable to set isp_degree to 2 or increase grain.')
        substitute(quant ~ 0 + cisp(lq, df=df, degree=degree), env = list(df=isp_df, degree=isp_degree))
      }
    else
      quant ~ 0 + dist(q)
  if (interaction_type == 'scale_only'){
    # substitute(quant ~ 0 + dist(q), env=list(dist=dist))
    formula_interaction <- list(update(formula$interaction, intr ~ 0 + .))
    main_form <- new_formula(lhs(formula$main), quote(lp + exp(intr)*quant))
  } else {
    intr <- paste0('lp + ', paste0('exp(intr', 1:isp_df, ')*slq', 1:isp_df, collapse = '+')) |> str2lang()
    main_form <- new_formula(lhs(formula$main), intr)
    intr_rhs <- rhs(formula$interaction)
    formula_interaction <- lapply(seq_len(isp_df), function(i){
      new_formula(as.symbol(paste0('intr', i)), intr_rhs)
    })
  }


  # Create the main formula
  bf_forms <- bf(main_form, nl=TRUE, cmc=FALSE,
                 flist = list(
                   formula_lp,
                   sapply(formula_interaction, lf, cmc=FALSE),
                   quantile ~ 0 + offset(lq)
                 ))

  if (interaction_type == 'scale_only') bf_forms <- bf_forms + formula_quantile

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
