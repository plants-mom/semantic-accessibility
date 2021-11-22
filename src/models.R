##
## bayesian models
## NB: this might sometimes crash, just run it one more time
##

here::i_am("src/models.R")

library(here)
library(dplyr)
library(readr)
library(brms)
library(purrr)

options(mc.cores = parallel::detectCores())

source(here("src/priors.R"))

files <- list.files(here("results"), pattern = "region[0-9].csv")
dfs <- map(files, ~ read_csv(here("results", .)))

fit_models <- function(data_list, dv_name, optimize_mem = FALSE) {
  frm <- formula(~ 1 + typic * interf * quants +
    (1 + typic * interf * quants | item) +
    (1 + typic * interf * quants | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  nms <- data_list %>%
    map(~ select(., region)) %>%
    map_dfr(unique) %>%
    pull()

  sel_data <- map(
    data_list,
    ~ select(., region:item, quan_cond:last_col(), {{ dv_name }})
  ) %>%
    set_names(paste0("region_", nms))

  full_ms <- sel_data %>%
    map(~ brm(frm,
      family = lognormal(),
      prior = priors,
      iter = 4000, data = .x,
      file = here("models", paste0(dv_name, "_r", .x$region[1]))
    ))

  if (optimize_mem == TRUE) {
    rm(full_ms)
    gc()
  }

  frm <- formula(~ 1 + typic * interf +
    (1 + typic * interf | item) +
    (1 + typic * interf | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  sel_data <- map(sel_data, ~ select(., -quants))

  split_ms <- map(sel_data, ~ split(., .$quan_cond)) %>%
    map(~ imap(., ~ brm(frm,
      family = lognormal(),
      prior = priors,
      iter = 4000,
      data = .x,
      file = here(
        "models",
        paste0(
          dv_name, "_",
          tolower(.y), "_r", .x$region[1]
        )
      )
    )))

  if (optimize_mem == TRUE) {
    rm(split_ms, sel_data, frm)
    gc()
  } else {
    return(list(full_models = full_ms, split_models = split_ms))
  }
}


if (sys.nframe() == 0) {
  fit_models(dfs, "totfixdur", optimize_mem = TRUE)

  map(dfs, ~ filter(., rrdur > 0)) %>%
    fit_models("rrdur", optimize_mem = TRUE)

  map(dfs[6:8], ~ filter(., rpdur > 0)) %>%
    fit_models("rpdur", optimize_mem = TRUE)

  c("gdur", "tgdur") %>%
    walk(~ fit_models(dfs[6:8], ., optimize_mem = TRUE))
}
