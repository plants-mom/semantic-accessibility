##
## fitting stan models
##

here::i_am("src/stan_models.R")

library(here)
library(fs)
library(dplyr)
library(purrr)
library(rstan)
library(readr)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


prepare_data <- function(var, region) {
  region %>%
    filter(.data[[var]] != 0) %>%
    select(subj, item, {{ var }}, quan_cond:last_col()) %>%
    with(., list(
      rt = .[[var]],
      N = length(.[[var]]),
      N_subj = max(subj), N_item = max(item),
      subj = subj, item = item, quant = quants,
      typic = typic, interf = interf
    ))
}

if (sys.nframe() == 0) {
  dfs <- dir_ls(here("results"), regexp = "region[0-9].csv") %>%
    map(read_csv)

  names(dfs) <- names(dfs) %>%
    path_file() %>%
    path_ext_remove()

  dfs %>%
    map(~ filter(., quants == 1)) %>% # GEEN
    map(~ prepare_data("tgdur", .)) %>%
    map(~ within(., rm("quant"))) %>%
    map(~ stan(
      file = here("src/mixture_everywhere_split.stan"),
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      iter = 4000
    )) %>%
    iwalk(~ saveRDS(.x, here("models", paste0("tgdur_geen_stan_", .y, ".rds"))))

  dfs %>%
    map(~ filter(., quants == -1)) %>% # EEN
    map(~ prepare_data("tgdur", .)) %>%
    map(~ within(., rm("quant"))) %>%
    map(~ stan(
      file = here("src/mixture_everywhere_split.stan"),
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      iter = 4000
    )) %>%
    iwalk(~ saveRDS(.x, here("models", paste0("tgdur_een_stan_", .y, ".rds"))))

  dfs %>%
    map(~ filter(., quants == 1)) %>% # GEEN
    map(~ prepare_data("gdur", .)) %>%
    map(~ within(., rm("quant"))) %>%
    map(~ stan(
      file = here("src/mixture_everywhere_split.stan"),
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      iter = 4000
    )) %>%
    iwalk(~ saveRDS(.x, here("models", paste0("gdur_geen_stan_", .y, ".rds"))))

  dfs %>%
    map(~ filter(., quants == -1)) %>% # EEN
    map(~ prepare_data("gdur", .)) %>%
    map(~ within(., rm("quant"))) %>%
    map(~ stan(
      file = here("src/mixture_everywhere_split.stan"),
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      iter = 4000
    )) %>%
    iwalk(~ saveRDS(.x, here("models", paste0("gdur_een_stan_", .y, ".rds"))))

  dfs %>%
    map(~ prepare_data("tgdur", .)) %>%
    map(~ stan(
      file = here("src/mixture_everywhere.stan"),
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      iter = 4000
    )) %>%
    iwalk(~ saveRDS(.x, here("models", paste0("tgdur_stan_", .y, ".rds"))))

  dfs %>%
    map(~ prepare_data("gdur", .)) %>%
    map(~ stan(
      file = here("src/mixture_everywhere.stan"),
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      iter = 4000
    )) %>%
    iwalk(~ saveRDS(.x, here("models", paste0("gdur_stan_", .y, ".rds"))))
}
