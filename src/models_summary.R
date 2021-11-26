
here::i_am("src/models_summary.R")

library(here)
library(dplyr)
library(brms)
library(bayesplot)
library(purrr)
library(tibble)
library(ggplot2)
library(readr)

source(here("src/models.R"))


msummary <- function(data_list, id = "region") {
  map(data_list, ~ posterior_summary(., pars = "b_")) %>%
    map(~ rownames_to_column(as.data.frame(.))) %>%
    bind_rows(.id = id)
}


post_plots <- function(var_name, data_list, ...) {
  make_plot <- compose(
    ~ mcmc_intervals(.x, prob = 0.95, prob_outer = 1),
    ~ posterior_samples(., pars = "b_[^I]")
  )
  fit_models(data_list, var_name, ...) %>%
    modify_at(
      "full_models",
      ~ map(.x, ~ make_plot(.x))
    ) %>%
    modify_at(
      "split_models",
      ~ map(.x, ~ map(.x, ~ make_plot(.x)))
    )
}

write_summary <- function(var_name, data_list = dfs, ...) {
  fit_models(data_list, var_name, ...) %>%
    modify_at("full_models", msummary) %>%
    modify_at("split_models", ~ map_dfr(., ~ msummary(., id = "quant"),
      .id = "region"
    )) %>%
    iwalk(~
    write_csv(.x, here(
      "results",
      paste0(var_name, "_", .y, ".csv")
    )))
}

if (sys.nframe() == 0) {
  ## this takes a lot of memory
  ## probably should be done smarter
  ## c("gdur", "tgdur", "rpdur") %>%
  ##   walk(~ write_summary(., dfs[6:8]))

  ## c("totfixdur", "rrdur", "gbck", "rr") %>%
  ##   walk(~ write_summary(., dfs))

  ## c("rr") %>%
  ##   walk(~ write_summary(., dfs, .return = "split_models"))

  c("rr") %>%
    walk(~ write_summary(., dfs, .return = "full_models"))
}
