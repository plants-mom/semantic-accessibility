##
## fitting stan models
##

here::i_am("src/stan_models.R")

library(here)
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
  r6 <- read_csv(here("results/region6.csv"))

  c(tgdur = "tgdur", rpdur = "rpdur", gdur = "gdur") %>%
    map(~ prepare_data(., r6)) %>%
    map(~ stan(
      file = here("src/mixture_everywhere.stan"),
      data = .,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      iter = 4000
    )) %>%
    imap(~ saveRDS(.x, here("models", paste0(.y, "_stan_r6", ".rds"))))
}
