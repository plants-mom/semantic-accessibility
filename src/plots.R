##
## plots
##

here::i_am("src/plots.R")

library(here)
library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(readr)
library(fs)
library(tidyr)
library(purrr)
library(ggtext)

##
## prior predictive check plots
##

ppc_rt_plots <- function(file_in, priors, save = FALSE) {

  ## The priors here do not determine the priors used for the simulations,
  ## only the caption.

  sim_summary <- read.csv(file_in)
  capt <- priors %>%
    select(class, prior) %>%
    kable(format = "simple")

  ccc <- with(priors, paste(class, prior, collapse = "<br>"))

  scsum_plot <- sim_summary %>%
    filter(stat != "ieff_rt") %>%
    ggplot(aes(rt)) +
    scale_x_continuous("Reaction times in ms",
      trans = "log",
      breaks = c(0.001, 1, 100, 1000, 10000, 10000000),
      labels = function(n) {
        format(n, scientific = 5, digits = 3)
      }
    ) +
    geom_histogram(bins = 50) +
    facet_wrap(~stat, ncol = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(caption = ccc) +
    theme(
      plot.caption.position = "plot",
      plot.caption = element_markdown(
        size = 11,
        ## lineheight = 1,
        padding = margin(5.5, 5.5, 5.5, 5.5),
        margin = margin(0, 0, 5.5, 0),
        ## linetype = 1
      )
    )


  inter <- int_eff <- sim_summary %>%
    filter(stat == "ieff_rt") %>%
    mutate(
      rt = replace(rt, rt >= 10000, 10000),
      rt = replace(rt, rt <= -10000, -10000)
    ) %>%
    ggplot(aes(rt)) +
    geom_histogram(bins = 100)

  pp_checks <- plot_grid(scsum_plot, inter)

  if (save == TRUE) {
    ggsave(here("figs/prior_pred_6.png"), pp_checks, width = 16, height = 8)
    return(pp_checks)
  } else {
    return(pp_checks)
  }
}

ppc_params_plots <- function(simdata) {

  ## ppc prior sample binomial

  simdata %>%
    read_csv() %>%
    modify(rethinking::inv_logit) %>%
    pivot_longer(cols = everything(), names_to = "parameter") %>%
    ggplot(., aes(value)) +
    geom_density() +
    facet_wrap(~parameter)

  ## plot(simd$true_params$sigma_u1)
  ## glimpse(inv_params)
  ## rethinking::dens(inter)
}

measure_by_cond <- function() {
  read_csv(here("results/region6.csv")) %>%
    select(gdur, tgdur, rpdur, cond) %>%
    pivot_longer(!cond, names_to = "measure") %>%
    mutate(cond = as.factor(cond), measure = as.factor(measure)) %>%
    filter(value != 0) %>%
    ggplot(aes(x = cond, y = log(value))) +
    geom_boxplot() +
    facet_wrap(~measure)
}

hist_r6 <- function(var) {
  read_csv(here("results/region6.csv")) %>%
    select({{ var }}, cond) %>%
    mutate(cond = as.factor(cond)) %>%
    filter(.data[[var]] != 0) %>%
    ggplot(aes(x = log(.data[[var]]))) +
    geom_histogram(binwidth = 0.075) +
    facet_wrap(~cond)
}


if (sys.nframe() == 0) {
  source(here("src/priors.R"))

  ## ppc_params_plots(here("results/ppc_params_sample_binom2021-11-29.csv"))
  pp <- ppc_rt_plots(here("results/sim_summary.csv"), priors)
  measure_by_cond()
  ggsave(here("figs/measure_by_cond.png"))
}
