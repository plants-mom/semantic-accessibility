##
## helper functions and summaries for the fitted brms models
##

here::i_am("src/models_summary.R")

library(here)
library(dplyr)
library(brms)
library(bayesplot)
library(purrr)
library(tibble)
library(ggplot2)
library(rstan)
library(readr)

list_rename <- function(.x, ..., .strict = TRUE) {
  # taken from: https://github.com/tidyverse/purrr/issues/804#issuecomment-729070112
  pos <- tidyselect::eval_rename(quote(c(...)), .x, strict = .strict)
  names(.x)[pos] <- names(pos)
  .x
}

msummary <- function(data_list, id = "region") {
  map(data_list, ps_rename) %>%
    bind_rows(.id = id)
}

ps_rename <- compose(
  ~ relabel_summary(.),
  ~ posterior_summary(., pars = "b_")
)

make_plot <- compose(
  ~ mcmc_intervals(.x, prob = 0.95, prob_outer = 1),
  ~ relabel_samples(.),
  ~ posterior_samples(., pars = "b_[^I]")
)

post_plots <- function(var_name, data_list,
                       make_plot_func = make_plot,
                       .return = c("full_models", "split_models")) {
  .return <- match.arg(.return)

  if (.return == "full_models") {

  fit_full(data_list, var_name) %>%
      map(make_plot_func)

  } else if (.return == "split_models") {

  fit_split(data_list, var_name) %>%
      map( ~ map(.x, ~ make_plot_func(.x)))
  }
  ## fit_models(data_list, var_name, ...) %>%
  ##   modify_at(
  ##     "full_models",
  ##     ~ map(.x, ~ make_plot_func(.x))
  ##   ) %>%
  ##   modify_at(
  ##     "split_models",
  ##     ~ map(.x, ~ map(.x, ~ make_plot_func(.x)))
  ##   )
}

relabel_samples <- function(labelled_smpls) {
  labelled_smpls %>%
    rename_with(recode,
      b_typic = "subj",
      b_interf = "obj",
      b_quants = "quant",
      "b_typic:interf" = "subj x obj",
      "b_typic:quants" = "subj x quants",
      "b_interf:quants" = "obj x quants",
      "b_typic:interf:quants" = "subj x obj x quants"
    )
}

relabel_stan_sum <- function(stan_summary) {
  stan_summary %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(
      rowname =
        case_when(
          rowname == "alpha" ~ "intercept",
          rowname == "b_typic" ~ "subj",
          rowname == "b_interf" ~ "obj",
          rowname == "b_quant" ~ "quant",
          rowname == "b_interf_typic" ~ "subj x obj",
          rowname == "b_quant_typic" ~ "subj x quants",
          rowname == "b_interf_quant" ~ "obj x quants",
          rowname == "b_interf_quant_typic" ~ "subj x obj x quants",
          rowname == "prob" ~ "theta",
          TRUE ~ rowname
        )
    )
}

relabel_summary <- function(ps_summary) {
  ps_summary %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(
      rowname =
        case_when(
          rowname == "b_Intercept" ~ "intercept",
          rowname == "b_typic" ~ "subj",
          rowname == "b_interf" ~ "obj",
          rowname == "b_quants" ~ "quant",
          rowname == "b_typic:interf" ~ "subj x obj",
          rowname == "b_typic:quants" ~ "subj x quants",
          rowname == "b_interf:quants" ~ "obj x quants",
          rowname == "b_typic:interf:quants" ~ "subj x obj x quants"
        )
    )
}

write_summary <- function(var_name, data_list = dfs, mods = c("full", "split"),
                          ...) {
  mods <- match.arg(mods)

  if (mods == "full") {
    list(full_models = fit_full(data_list, var_name)) %>%
      map(msummary) %>%
      iwalk(~ write_csv(.x, here(
        "results",
        paste0(var_name, "_", .y, ".csv")
      )))
  } else if (mods == "split") {
    list(split_models = fit_split(data_list, var_name)) %>%
      map(~ map_dfr(., ~ msummary(., id = "quant"),
        .id = "region"
      )) %>%
      iwalk(~
      write_csv(.x, here(
        "results",
        paste0(var_name, "_", .y, ".csv")
      )))
  }
}

if (sys.nframe() == 0) {
  source(here("src/priors.R"))
  source(here("src/models.R"))

  dfs <- list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .)))

  ## c("gdur", "tgdur", "rpdur") %>%
  ##   walk(~ write_summary(., dfs[6:8]))

  ## c("totfixdur", "rrdur", "gbck", "rr") %>%
  ##   walk(~ write_summary(., dfs, mods = "full"))

  ## c("totfixdur", "rrdur", "gbck", "rr") %>%
  ##   walk(~ write_summary(., dfs, mods = "split"))

  ## fit_split(dfs[6], "gbck",
  ##   split_by = "quant_typic",
  ##   .priors = priors_binom,
  ##   remove_zeros = FALSE
  ## ) %>%
  ##   map(msummary, id = "condition") %>%
  ##   pluck("region_6") %>%
  ##   write_csv(here("results/gbck_split_quant_typic_r6.csv"))

  ## fit_split(dfs[7], "totfixdur",
  ##   split_by = "quant_typic",
  ##   .priors = priors
  ## ) %>%
  ##   .[[1]] %>%
  ##   map(msummary, id = "condition") %>%
  ##   pluck("region_8") %>%
  ##   write_csv(here("results/totfixdur_split_quant_typic_r8.csv"))

  fit_split(dfs[1], "rrdur",
    split_by = "quant_typic",
    .priors = priors
  ) %>%
    map(msummary, id = "condition") %>%
    pluck("region_1") %>%
    write_csv(here("results/rrdur_split_quant_typic_r1.csv"))

  fit_split(dfs[5], "rrdur",
    split_by = "quant_typic",
    .priors = priors
  ) %>%
    map(msummary, id = "condition") %>%
    pluck("region_5") %>%
    write_csv(here("results/rrdur_split_quant_typic_r5.csv"))
}
