##
## fitting stan models
##

here::i_am("src/04-stan_models.R")

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

fit_main_measures <- function(data_list) {
  data_and_model <- compose(
    \(region_data) stan(
      file = here("src/mixture_everywhere.stan"),
      data = region_data,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      iter = 4000
    ),
    \(var, region) prepare_data(var, region)
  )

  ## map(data_list, \(region) data_and_model("tgdur", region)) %>%
  ##   iwalk(\(model, region_no)
  ##   saveRDS(
  ##     model,
  ##     here("models", paste0("tgdur_stan_", region_no, ".rds"))
  ##   ))

  map(data_list, \(region) data_and_model("gdur", region)) %>%
    iwalk(\(model, region_no)
    saveRDS(
      model,
      here("models", paste0("gdur_stan_", region_no, ".rds"))
    ))
}

main <- function() {
  dfs <- dir_ls(here("results"), regexp = "region[0-9].csv") %>%
    map(read_csv)

  names(dfs) <- names(dfs) %>%
    path_file() %>%
    path_ext_remove()

  fit_main_measures(dfs)
  ## dfs %>%
  ##   map(~ filter(., quants == 1)) %>% # GEEN
  ##   map(~ prepare_data("tgdur", .)) %>%
  ##   map(~ within(., rm("quant"))) %>%
  ##   map(~ stan(
  ##     file = here("src/mixture_everywhere_split.stan"),
  ##     data = .,
  ##     control = list(adapt_delta = 0.99, max_treedepth = 15),
  ##     iter = 4000
  ##   )) %>%
  ##   iwalk(~ saveRDS(.x, here("models", paste0("tgdur_geen_stan_", .y, ".rds"))))

  ## dfs %>%
  ##   map(~ filter(., quants == -1)) %>% # EEN
  ##   map(~ prepare_data("tgdur", .)) %>%
  ##   map(~ within(., rm("quant"))) %>%
  ##   map(~ stan(
  ##     file = here("src/mixture_everywhere_split.stan"),
  ##     data = .,
  ##     control = list(adapt_delta = 0.99, max_treedepth = 15),
  ##     iter = 4000
  ##   )) %>%
  ##   iwalk(~ saveRDS(.x, here("models", paste0("tgdur_een_stan_", .y, ".rds"))))

  ## dfs %>%
  ##   map(~ filter(., quants == 1)) %>% # GEEN
  ##   map(~ prepare_data("gdur", .)) %>%
  ##   map(~ within(., rm("quant"))) %>%
  ##   map(~ stan(
  ##     file = here("src/mixture_everywhere_split.stan"),
  ##     data = .,
  ##     control = list(adapt_delta = 0.99, max_treedepth = 15),
  ##     iter = 4000
  ##   )) %>%
  ##   iwalk(~ saveRDS(.x, here("models", paste0("gdur_geen_stan_", .y, ".rds"))))

  ## dfs %>%
  ##   map(~ filter(., quants == -1)) %>% # EEN
  ##   map(~ prepare_data("gdur", .)) %>%
  ##   map(~ within(., rm("quant"))) %>%
  ##   map(~ stan(
  ##     file = here("src/mixture_everywhere_split.stan"),
  ##     data = .,
  ##     control = list(adapt_delta = 0.99, max_treedepth = 15),
  ##     iter = 4000
  ##   )) %>%
  ##   iwalk(~ saveRDS(.x, here("models", paste0("gdur_een_stan_", .y, ".rds"))))


  ## dfs %>%
  ##   ## GEEN, subject mismatch (atypical)
  ##   map(~ filter(., quants == 1, typic == -1)) %>%
  ##   map(~ prepare_data("tgdur", .)) %>%
  ##   map(~ within(., rm("quant", "typic"))) %>%
  ##   map(~ stan(
  ##     file = here("src/mixture_everywhere_split_interf_only.stan"),
  ##     data = .,
  ##     control = list(adapt_delta = 0.99, max_treedepth = 15),
  ##     iter = 4000
  ##   )) %>%
  ##   iwalk(~ saveRDS(.x, here(
  ##     "models",
  ##     paste0("tgdur_geen_atypical_stan_", .y, ".rds")
  ##   )))
}

if (sys.nframe() == 0) {
  main()
}
