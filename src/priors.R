##
##
##

library(brms)
here::i_am("src/priors.R")

priors <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 1), class = sigma),
  prior(normal(0, 1), class = sd),
  prior(lkj(2), class = cor)
)

priors_narrow_int <- c(
  prior(normal(5, 5), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 1), class = sigma),
  prior(normal(0, 1), class = sd),
  prior(lkj(2), class = cor)
)

priors_ni_big_sd <- c(
  prior(normal(5, 5), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 1), class = sigma),
  prior(normal(0, 3), class = sd),
  prior(lkj(2), class = cor)
)

priors_ni_big_sd_sigma <- c(
  prior(normal(5, 5), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 3), class = sigma),
  prior(normal(0, 3), class = sd),
  prior(lkj(2), class = cor)
)

priors_binom <- c(
  prior(normal(0, 1.5), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 1), class = sd),
  prior(lkj(2), class = cor)
)

priors_gamma <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 1), class = sd),
  prior(lkj(2), class = cor)
)
