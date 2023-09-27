##
## plots
##

here::i_am("src/plots.R")

library(here)
library(forcats)
library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bayesplot)
library(cowplot)
library(readr)
library(fs)
library(tidyr)
library(purrr)
library(brms)
library(ggtext)

source(here("src/04-models_summary.R"))
##
## prior predictive check plots
##

cur_palette <- "Set2"

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

ms_r3 <- function() {
  f <- compose(readRDS, here)
  list(
    TFD = f("models/totfixdur_r3.rds"),
    RRDUR = f("models/rr_r3.rds")
  ) %>%
    map_dfr(make_plot_data, .id = "measure") %>%
    measure_summary()
}

ms_r1 <- function() {
  f <- compose(readRDS, here)
  list(
    TFD = f("models/totfixdur_r1.rds"),
    RRDUR = f("models/rr_r1.rds")
  ) %>%
    map_dfr(make_plot_data, .id = "measure") %>%
    measure_summary()
}

rts <- function(region) {
  ##
  ## TFD RB and RRD measures plot
  ##
  f <- compose(readRDS, here, paste0)
  list(
    TFD = make_plot_data(f("models/totfixdur_r", region, ".rds")),
    RB = make_plot_data_stan(f("models/tgdur_stan_region", region, ".rds")),
    RRD = make_plot_data(f("models/rrdur_r", region, ".rds"))
  ) %>%
    bind_rows(.id = "measure") %>%
    measure_summary()
}


neg_atypical_rts <- function() {
  regions_data <- function(region) {
    f <- compose(readRDS, here, paste0)
    list(
      TFD = make_plot_data(f(
        "models/totfixdur_geen_atypical_r",
        region, ".rds"
      )),
      RB = make_plot_data_stan(f(
        "models/tgdur_geen_atypical_stan_region",
        region, ".rds"
      ))
    ) %>%
      bind_rows(.id = "measure") %>%
      mutate(region = region)
  }

  list(
    regions_data(6),
    regions_data(8),
    regions_data(9)
  ) %>%
    bind_rows() %>%
    mutate(
      region = case_when(
        region == 6 ~ "critical",
        region == 8 ~ "post-critical",
        region == 9 ~ "wrap-up"
      ),
      region = as.factor(region)
    ) %>%
    measure_summary_regions()
}

regr_rr <- function(region) {
  ##
  ## regression and re-reading probability plot
  ##
  f <- compose(make_plot_data, readRDS, here)
  list(
    RR = f(paste0("models/rr_r", region, ".rds")),
    RP = f(paste0("models/gbck_r", region, ".rds"))
  ) %>%
    bind_rows(.id = "measure") %>%
    measure_summary()
}

measure_summary <- function(plot_data,
                            .measure = "measure",
                            pal = cur_palette) {
  dod <- 0.5

  plot_data %>%
    ggplot(., aes(fill = .data[[.measure]], color = .data[[.measure]])) +
    geom_linerange(aes(xmin = ll, xmax = hh, y = parameter),
      position = position_dodge(dod)
    ) +
    geom_linerange(aes(xmin = l, xmax = h, y = parameter),
      size = 1.5, show.legend = FALSE,
      position = position_dodge(dod)
    ) +
    geom_point(aes(x = m, y = parameter),
      size = 2, shape = 21,
      position = position_dodge(dod)
    ) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_color_brewer(palette = pal) +
    scale_fill_brewer(palette = pal)
}

measure_summary_regions <- function(plot_data,
                                    .measure = "measure",
                                    pal = cur_palette) {

  ## This should be refactored with measure_summary,
  ## but I don't have the time

  dod <- 0.5

  plot_data %>%
    ggplot(., aes(fill = .data[[.measure]], color = .data[[.measure]])) +
    geom_linerange(aes(ymin = ll, ymax = hh, x = region),
      position = position_dodge(dod)
    ) +
    geom_linerange(aes(ymin = l, ymax = h, x = region),
      size = 1.5, show.legend = FALSE,
      position = position_dodge(dod)
    ) +
    geom_point(aes(y = m, x = region),
      size = 2, shape = 21,
      position = position_dodge(dod)
    ) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_color_brewer(palette = pal) +
    scale_fill_brewer(palette = pal)
}


make_plot_data <- compose(
  ~ mcmc_intervals_data(.x, prob = 0.5, prob_outer = 0.95, point_est = "mean"),
  ~ relabel_samples(.),
  ~ posterior_samples(., pars = "b_[^I]") # this is deprecated
  ## should be: ~ as_draws_df(., variable = "b_[^I]", regex = TRUE)
  ## but need to check
)

make_plot_data_stan <- compose(
  ~ mutate(.,
    parameter = fct_recode(parameter,
      "intercept" = "alpha",
      "subj" = "b_typic",
      "obj" = "b_interf",
      "quant" = "b_quant",
      "subj x obj" = "b_interf_typic",
      "subj x quants" = "b_quant_typic",
      "obj x quants" = "b_interf_quant",
      "subj x obj x quants" = "b_interf_quant_typic",
      "theta" = "prob"
    )
  ),
  ~ mcmc_intervals_data(.,
    prob = 0.5, prob_outer = 0.95,
    point_est = "mean",
    regex_pars = "b_[^I]"
  ),
)

## this uses split models
## which are not correct
r6_split_plot <- function(pal = cur_palette) {
  dod <- 0.5

  f <- compose(readRDS, here)
  c(
    een = "models/totfixdur_een_r6.rds",
    geen = "models/totfixdur_geen_r6.rds"
  ) %>%
    map(compose(make_plot_data, f)) %>%
    bind_rows(.id = "quantifier") %>%
    ## filter(parameter %in% c("subj", "obj")) %>%
    filter(parameter == "obj") %>%
    measure_summary(.measure = "quantifier")
}


r6_by_cond <- function(measure, pal = cur_palette) {
  read_csv(here("results/region6.csv")) %>%
    mutate(
      quantifier = as.factor(if_else(quan_cond == "EEN",
        "positive quantifier", "negative quantifier"
      )),
      subject = as.factor(if_else(typic_cond == "typical", "match", "mis")),
      object = as.factor(if_else(interf_cond == "interf", "match", "mis"))
    ) %>%
    filter(.data[[measure]] != 0) %>%
    ggplot(aes(object, .data[[measure]], colour = subject)) +
    geom_boxplot() +
    facet_wrap(~quantifier) +
    theme_minimal() +
    ylab("reading time (ms)") +
    ## theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_color_brewer(palette = pal) +
    scale_fill_brewer(palette = pal)
}


main <- function() {
  dims <- c(7, 4.7)
  ggsave(
    file = here("figs/fig-critical2.pdf"),
    plot = r6_by_cond("tgdur"), width = dims[1], height = dims[2]
  )

  ggsave(
    file = here("figs/fig-region5.pdf"),
    plot = rts(6), width = dims[1], height = dims[2]
  )

  ggsave(
    file = here("figs/fig-region6.pdf"),
    plot = rts(8), width = dims[1], height = dims[2]
  )

  ggsave(
    file = here("figs/fig-region9.pdf"),
    plot = rts(9), width = dims[1], height = dims[2]
  )

  ggsave(
    file = here("figs/fig-rr-rp-region-5.pdf"),
    plot = regr_rr(6), width = dims[1], height = dims[2]
  )

  ggsave(
    file = here("figs/fig-rr-rp-region-6.pdf"),
    plot = regr_rr(8), width = dims[1], height = dims[2]
  )

  ggsave(
    file = here("figs/fig-rr-rp-region-7.pdf"),
    plot = regr_rr(9), width = dims[1], height = dims[2]
  )

  ## not used fig: fig-obj-quant-int
  ## to make it: r6_split_plot()

  ## what to do with this? this is a "split" model,
  ## not correct figure name: fig-cue-based
  ## to make it: neg_atypical_rts()

  measure_by_cond()
  ggsave(here("figs/measure_by_cond.png"))
}

if (sys.nframe() == 0) {
  main()
}
