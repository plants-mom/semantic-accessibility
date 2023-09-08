##
## mixture model in brms
##

here::i_am("src/mix.R")

library(here)
library(dplyr)
library(readr)
library(brms)
options(mc.cores = parallel::detectCores())


source(here("src/priors.R"))



r6 <- read_csv(here("results/region6.csv"))


tgdur <- r6 %>%
  filter(tgdur != 0) %>%
  select(subj, item, tgdur, quan_cond:last_col())

mix <- mixture(lognormal, lognormal)

frm <- bf(tgdur ~ 1 + typic + interf + quants +
  (1 | item) + (1 | subj))

make_stancode(frm,
  family = mix,
  prior = priors_mix,
  iter = 4000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  data = tgdur,
  file = here("models", "mixtest")
)


m1 <- brm(frm,
  family = mix,
  prior = priors_mix,
  iter = 4000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  data = tgdur,
  file = here("models", "mixtest")
)

## pp_check(m1, type = "stat_2d", stat = c("median", "sd"))

## pp_check(m1)
