library(ggplot2)
library(tidybayes)
library(splines)
# Number of points to simulate
n <- 1000

# Generate x-values from a normal distribution
x <- rnorm(n, 5, 1)

# Generate y-values with variance depending on x
y <- 1 + 2*x + rnorm(n) * exp(5-x) + rchisq(n, df=1) * log(x)

# Combine x and y into a data frame
dt <- data.frame(x = x, y = y)

ggplot(dt, aes(x = x, y = y)) +
  geom_point(alpha = 0.6, color = "orange") +
  theme_minimal() +
  ggtitle("Distribution with Variable Variance") +
  xlab("X-Axis") +
  ylab("Y-Axis")


fit_flex <- smoq(
  y ~ s(x, k=5, fx=TRUE),
  data = dt,
  iter=2000,
  cores=2, chains=2,
  smoq_control=smoq_control(c(0.025,0.975), 10),
  init_r=1,
  threads=4,
  control=list(max_treedepth=12, adapt_delta=0.75)
)

fit <- smoq(
  list(main = y ~ x, interaction=y ~ s(x, k=4, fx=TRUE)),
  data = dt,
  isp_df = 5,
  iter=2000,
  cores=2, chains=2,
  smoq_control=smoq_control(c(0.025,0.975), 10),
  init_r=1,
  threads=4,
  control=list(max_treedepth=12, adapt_delta=0.75)
)

pred_dat0 <-
  dt |>
  ._prepare_data_(c(0.025,0.975), 10) |>
  add_linpred_draws(fit, ndraws=500)

pred_dat <-
  dplyr::summarise(pred_dat0, .linpred=mean(.linpred))


pred_dat |>
  ggplot(aes(x=x)) +
  geom_line(aes(
    y=.linpred, color=q, group=q
  )) +
  geom_point(aes(y=y), size=.5, shape=21, color='orange') +
  theme_minimal()

real_dt <- data.frame(x = rnorm(2e5, 5, 1)) |>
  transform(y = 3 + 2*x + rnorm(2e5) * exp(5-x)+ rchisq(n, df=1) * log(x))

library(quantreg)
Qs <- c(0.5, seq(0.025, 0.975, length.out=10)) |> unique() |> sort()
real_fit <- sapply(Qs, function(q){
  cat(q, '\n')
  rq(y ~ ns(x,10), data=real_dt, tau=q) |>
    predict()
}) |> cbind(real_dt)
colnames(real_fit) <- c(Qs, 'x', 'y')
real_pred  <- as.data.frame(real_fit) |>
  tidyr::pivot_longer(cols=1:11, names_to='q', values_to = '.linpred')  |>
  transform(q=as.numeric(q))

preds <- rbind(
  pred_dat |> dplyr::ungroup() |> dplyr::select(x, y, q,.linpred) |> transform(what='Proposed'),
  real_pred |> subset(x>=min(dt$x)& x<=max(dt$x)) |> transform(what='Real')
)
  # transform(what=factor(what, levels=c('Real', 'Proposed')))

preds |>
  ggplot(aes(x=x)) +
  geom_line(aes(
    y=.linpred, group=interaction(what, q), color=q, linetype=what
  ))
