##
## bayesian models
##

here::i_am("src/models.R")

library(here)
library(dplyr)
library(readr)
library(readr)
library(brms)
library(purrr)

options(mc.cores = parallel::detectCores())

source(here("src/priors.R"))

files <- list.files(here("results"), pattern = "region[0-9].csv")
dfs <- map(files, ~ read_csv(here("results", .)))

fit_models <- function(data_list, dv_name) {
  frm <- formula(~ 1 + typic * interf * quants +
    (1 + typic * interf * quants | item) +
    (1 + typic * interf * quants | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  sel_data <- map(data_list,
                  ~ select(., region:item, quan_cond:last_col(), {{ dv_name }}))

  full_ms <- sel_data %>%
    map(~ brm(frm,
      family = lognormal(),
      prior = priors,
      iter = 4000, data = .x,
      file = here("models", paste0(dv_name, "_r", .x$region[1]))
    ))


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
                        file = here("models",
                                    paste0(dv_name, "_",
                                           tolower(.y), "_r", .x$region[1]))
    )))
  return(list(full_models = full_ms, split_models = split_ms))
}

fit_models(dfs, "rrdur")

vars_late <- c("gdur", "tgdur", "rpdur")
map(vars_late, ~ fit_models(dfs[6:8], .))
