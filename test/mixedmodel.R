data(sleepstudy, package='lme4')
library(splines)

# Assuming a linear trend at median and flexible trend otherwise
# Not assuming an overall distribution
mixedmod <-
  smoq(
    list(
      main = Reaction ~ Days + (1 + Days | Subject),
      interaction = Reaction ~ s(lq, by=Subject, k=5) + ns(Days,df=3) + (1 + ns(Days,df=3) | Subject)
    ),
    iter=3000,
    cores=2, chains=2,
    isp_df=5,
    isp_degree=3,
    smoq_control=smoq_control(c(0.1,0.9), 5),
    data = sleepstudy,
    # interaction_type='all',
    init_r=1,
    threads=5,
    control=list(max_treedepth=14, adapt_delta=0.75))

# Same model but assuming a Gaussian process (still varying variance)
mixedmod_norm <-
  smoq(
    list(
      main = Reaction ~ Days + (1 + Days | Subject),
      interaction = Reaction ~ ns(Days,df=3) + (1 + ns(Days,df=3) | Subject)
    ),
    iter=3000,
    cores=2, chains=2,
    isp_degree=2, isp_df = 3,
    dist = 'norm', # Assuming normal error term at
    smoq_control=smoq_control(c(0.1,0.9), 10),
    data = sleepstudy,
    init_r=1,
    threads=5,
    control=list(max_treedepth=14, adapt_delta=0.75))

# lift all asumption
# Not assuming an overall distribution
mixedmod_flex <-
  smoq(
    Reaction ~ ns(Days,df=3) + (1 + ns(Days,df=3) | Subject),
    iter=3000,
    cores=2, chains=2,
    isp_df=3,
    isp_degree = 2,
    smoq_control=smoq_control(c(0.1,0.9), 4),
    data = sleepstudy,
    init_r=1,
    threads=5,
    control=list(max_treedepth=14, adapt_delta=0.75))

# Predict for the first subject
# At quantile = 0.1, 0.5, 0.9
sleepstudy |>
  dplyr::filter(Subject==308) |>
  add_quantiles(c(0.1, 0.5, 0.9)) |>
  # ._prepare_data_(c(0.1,0.9), 3) |> # This is a hidden function
  add_linpred_draws(mixedmod, ndraws=500) |>
  dplyr::summarise(.linpred=mean(.linpred)) |>
  ggplot(aes(x=Days)) +
  geom_line(aes(
    y=.linpred, color=q, group=q
  )) +
  geom_point(aes(y=Reaction), size=.5, shape=21, color='orange') +
  theme_minimal()

pred_sleep <- sleepstudy |>
  dplyr::filter(Subject==308) |>
  ._prepare_data_(c(0.1,0.9), 9) |>
  add_linpred_draws(mixedmod_norm, ndraws=500)

pred_sleep <-
  dplyr::summarise(pred_sleep, .linpred=mean(.linpred))

pred_sleep |>
  ggplot(aes(x=Days)) +
  geom_line(aes(
    y=.linpred, color=q, group=q
  )) +
  geom_point(aes(y=Reaction), size=.5, shape=21, color='orange') +
  theme_minimal()


sleepstudy |>
  ._prepare_data_(c(0.1,0.9), 2) |>
  add_linpred_draws(mixedmod, ndraws=500) |>
  dplyr::summarise(.linpred=mean(.linpred)) |>
  ggplot(aes(x=Days)) +
  geom_line(aes(
    y=.linpred, color=factor(q), group=factor(q)
  )) +
  geom_point(aes(y=Reaction), size=.5, shape=21, color='orange') +
  theme_minimal() +
  facet_wrap(~Subject)

