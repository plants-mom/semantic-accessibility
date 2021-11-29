##
## plots
##

here::i_am("src/plots.R")

library(here)
library(knitr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(readr)
library(tidyr)
library(purrr)
library(ggtext)

##
## prior predictive check plots
##

ppc_rt_plots <- function() {
  source(here("src/priors.R"))
  sim_summary <- read.csv(here("results/sim_summary_bin.csv"))



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
  pp_checks

  ggsave(here("figs/prior_pred_6.png"), pp_checks, width = 16, height = 8)
}

ppc_params_plots <- function(simdata) {

  ## ppc prior sample binomial

  simdata %>%
    modify(rethinking::inv_logit) %>%
    pivot_longer(cols = everything(), names_to = "parameter") %>%
    ggplot(., aes(value)) +
    geom_density() +
    facet_wrap(~parameter)

  ## plot(simd$true_params$sigma_u1)
  ## glimpse(inv_params)
  ## rethinking::dens(inter)
}

if (sys.nframe() == 0) {

  ppc_params_plots()
}


