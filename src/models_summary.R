
here::i_am("src/models_summary.R")

library(here)
library(dplyr)
library(brms)
library(bayesplot)
library(purrr)
library(tibble)
library(ggplot2)
library(readr)



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
  ~ posterior_samples(., pars = "b_[^I]")
)

post_plots <- function(var_name, data_list,
                       make_plot_func = make_plot, ...) {
  fit_models(data_list, var_name, ...) %>%
    modify_at(
      "full_models",
      ~ map(.x, ~ make_plot_func(.x))
    ) %>%
    modify_at(
      "split_models",
      ~ map(.x, ~ map(.x, ~ make_plot_func(.x)))
    )
}

relabel_samples <- function(labelled_smpls) {
  labelled_smpls %>%
    ## select(starts_with("b")) %>%
    rename(
      "subj" = "b_typic",
      "obj" = "b_interf",
      "quant" = "b_quants",
      "subj x obj" = "b_typic:interf",
      "subj x quants" = "b_typic:quants",
      "obj x quants" = "b_interf:quants",
      "subj x obj x quants" = "b_typic:interf:quants"
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
  source(here("src/models.R"))
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
