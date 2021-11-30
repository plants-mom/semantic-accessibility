##
## Bayes' factor exploration
##

here::i_am("src/bf_explore.R")

library(here)
library(dplyr)
library(brms)
library(readr)
library(purrr)
library(bridgesampling)
options(mc.cores = parallel::detectCores())

compute_bf <- function(dv_name, model) {

  frm <- formula(model)
  ## %>% update.formula(., . ~ . - typic:interf:quants) # this is just to explore, remove later!
  print(frm)

  m_mlk <- update(model, iter = 10000, warmup = 2000,
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  save_all_pars = TRUE) %>%
    bridge_sampler()

  frm <- update.formula(frm, paste0(". ~ . ", dv_name))
  print(frm)

  mn_mlk <- update(model, formula. = frm, iter = 10000, warmup = 2000,
                   control = list(adapt_delta = 0.99, max_treedepth = 15),
                   save_all_pars = TRUE) %>%
    bridge_sampler()

  return(bayes_factor(m_mlk, mn_mlk))
}


if (sys.nframe() == 0) {
  source(here("src/models.R"))

  ## results_bf <- data.frame(
  ##   matrix(vector(), 2, 2, dimnames = list(c(), c("comparison", "bf_10"))))

  bf2 <- list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .))) %>%
    .[6] %>%
    fit_models("totfixdur",
               .return = "full_models") %>%
    pluck("full_models", "region_6") %>%
    compute_bf("- typic:quants", .)

  print(bf2)

  ## bf1 <- compute_bf("- typic:quants", m1)
  ## print(bf1)
  ## from stdout: Estimated Bayes factor in favor of x1 over x2: 0.06223
  ## results_bf[1, ] <- c("with threeway", bf1$bf) # update to include formula

  ## bf2 <- compute_bf("- typic:quants - typic:interf:quants", m1)
  ## print(bf2)
  ## results_bf[2, ] <- c("without threeway", bf2$bf)
  ## Estimated Bayes factor in favor of x1 over x2: 0.00997
  ## write_csv(results_bf, here("results/bf_comparison.csv"))

}
