
here::i_am("src/models_summary.R")

library(here)
library(dplyr)
library(brms)
library(purrr)
library(tibble)
library(readr)
library(knitr)

source(here("src/models.R"))


msummary <- function(data_list, id = "region") {
  map(data_list, ~ posterior_summary(., pars = "b_")) %>%
    map(~ rownames_to_column(as.data.frame(.))) %>%
    bind_rows(.id = id)
}


write_summary <- function(var_name, data_list = dfs) {
  fm <- fit_models(data_list, var_name)
  write_csv(
    msummary(fm$full_models),
    here("results", paste0(var_name, "_full_models.csv"))
  )
  map_dfr(fm$split_models, ~ msummary(., id = "quant"),
    .id = "region"
  ) %>%
    write_csv(here("results", paste0(var_name, "_split_models.csv")))
}

if (sys.nframe() == 0) {
  ## this takes a lot of memory
  ## probably should be done smarter
  c("gdur", "tgdur") %>%
    walk(~ write_summary(., dfs[6:8]))

  c("totfixdur", "rrdur", "rpdur") %>%
    walk(~ write_summary(., dfs))
}
