##
##
##

here::i_am("src/stan_summary.R")

library(here)
library(dplyr)
library(readr)
library(rstan)
library(bayesplot)


y <- read_csv(here("results/region6.csv")) %>%
  filter(rpdur != 0) %>%
  pull(rpdur)



fit <- readRDS(here("models/rpdur_stan_r6.rds"))
yrep <- extract(fit, pars = "yrep")$yrep


ppc_dens_overlay(y, yrep[1:200,]) +
  coord_cartesian(
    ## ylim = c(0, 0.005),
    xlim = c(0, 1500))

ppc_stat_2d(y, yrep, stat = c("median", "sd"))
ppc_stat_2d(y, yrep, stat = c("mean", "sd"))
ppc_stat(y, yrep, stat = "median")
ppc_stat(y, yrep, stat = "sd")
ppc_stat_2d(y, yrep, stat = c("min", "max"))

summary(fit, pars = c("alpha", "b_typic", "delta", "prob", "sigma_e", "sigma_e_shift"))$summary

plot(function(x) dbeta(x, 10,2))

if (sys.nframe() == 0) {

  ggsave(here("figs/ppc_tgdur_stat2_stan.png"))
}

y <- read_csv(here("results/region8.csv")) %>%
  filter(gdur != 0) %>%
  pull(gdur)

fit <- readRDS(here("models/gdur_stan_region8.rds"))
yrep <- extract(fit, pars = "yrep")$yrep

ppc_dens_overlay(y, yrep[1:200,]) +
  coord_cartesian(
    ## ylim = c(0, 0.005),
    xlim = c(0, 1500))
