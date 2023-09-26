##
## Diagnosing sampling problems
##

here::i_am("src/stan_diagnose.R")

library(here)
library(fs)
library(dplyr)
library(purrr)
library(rstan)
library(readr)
library(bayesplot)



mds <- dir_ls(here("models"), regexp =  "gdur_stan_region[5-9].rds") %>%
  map(readRDS)

  names(mds) <- names(mds) %>%
    path_file() %>%
    path_ext_remove()

rhats <- map(mds, rhat)

rhat_all <- compose(all, \(rh) rh < 1.1 )
map(rhats, rhat_all)
map(mds, check_divergences)

## model <- "models/gdur_stan_region8.rds"
model <- "models/tgdur_stan_region5.rds"

y <- read_csv(here("results/region5.csv")) %>%
  filter(gdur != 0) %>%
  pull(tgdur)

fit <- readRDS(here(model))

yrep <- extract(fit, pars = "yrep")$yrep


ppc_dens_overlay(y, yrep[1:200,]) +
  coord_cartesian(
    ## ylim = c(0, 0.005),
    xlim = c(0, 1500))

ggsave(here("figs/gdur-8-stan.png"))
ggsave(here("figs/gdur-8-stan-2d.png"))

ppc_stat_2d(y, yrep, stat = c("median", "sd"))
ppc_stat_2d(y, yrep, stat = c("mean", "sd"))
ppc_stat(y, yrep, stat = "median")
ppc_stat(y, yrep, stat = "sd")
ppc_stat_2d(y, yrep, stat = c("min", "max"))

traceplot(fit, pars = c("alpha", "b_typic", "prob", "tau_u", "tau_w", "delta"), inc_warmup = FALSE)


pairs(fit, pars = c("tau_w[2]", "w[1,2]", "w[2,2]", "w[3,2]"))
pairs(fit, pars = c("tau_u[2]", "u[1,2]", "u[2,2]", "u[3,2]"))




summary(fit, inc_warmup=FALSE)$summary[,"Rhat"] %>%
                               as.data.frame() %>%
                               filter(!is.na(.)) %>%
                               summary()

summary(fit, inc_warmup=FALSE)$summary[,"n_eff"] %>%
                               as.data.frame() %>%
                               filter(!is.na(.)) %>%
                               summary()


get_sampler_params(fit, inc_warmup=FALSE) %>%
  map_dfc(~ .[,'divergent__']) %>%
  map_dbl(., sum)
