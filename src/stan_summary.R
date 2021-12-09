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
  filter(tgdur != 0) %>%
  pull(tgdur)


fit <- readRDS(here("models/mixture_int_typ.rds"))
yrep <- extract(fit, pars = "yrep")$yrep


ppc_dens_overlay(y, yrep[1:200,])
  ## coord_cartesian(
  ##   ## ylim = c(0, 0.005),
  ##   xlim = c(0, 1500))

ppc_stat_2d(y, yrep, stat = c("median", "sd"))
ppc_stat_2d(y, yrep, stat = c("mean", "sd"))
ppc_stat(y, yrep, stat = "median")
ppc_stat_2d(y, yrep, stat = c("min", "max"))

summary(fit, pars = c("alpha", "beta_typic", "delta", "prob", "sigma_e", "sigma_e2"))$summary

plot(function(x) dbeta(x, 2, 10))

ggsave(here("figs/ppc_tgdur_stat2_stan.png"))

traceplot(fit, pars = c("alpha", "beta_typic"), nrow = 2)
str(fit)

methods(class = "stanfit")
