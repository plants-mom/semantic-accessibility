

library(here)
library(dplyr)
library(brms)
library(readr)
library(purrr)
library(fs)
library(bridgesampling)
options(mc.cores = parallel::detectCores())


bf_stability <- function(model, formula_term, iters, stan_iters) {
  results <- data.frame(matrix(vector(), iters, 3,
    dimnames = list(c(), c("iter", "bf_10", "stan_iterations"))
  ))

  frm <- formula(model) %>%
    update.formula(., paste0(". ~ . ", formula_term))

  outpath <- here("results/bf_stability.csv")

  for (n in seq(iters)) {
    message(paste("iteration:", n, "/", iters))

    m1_mlk <- update(model,
      iter = stan_iters, warmup = 2000,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      save_all_pars = TRUE,
      file = here("models", paste0("bf_stability_full_", n))
    ) %>%
      bridge_sampler()

    m1n_mlk <- update(model,
      formula. = frm, iter = stan_iters, warmup = 2000,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      save_all_pars = TRUE,
      file = here("models", paste0("bf_stability_null_", n))
    ) %>%
      bridge_sampler()

    bf_score <- bayes_factor(m1_mlk, m1n_mlk)
    results[n, ] <- c(n, bf_score$bf, stan_iters)
    print(results)

    if (file_exists(outpath)) {
      read_csv(outpath) %>%
        bind_rows(results) %>%
        write_csv(outpath)
    } else {
      write_csv(results, outpath)
    }
  }
}

if (sys.nframe() == 0) {
  source(here("src/models.R"))

  get_model("totfixdur", 6) %>%
    bf_stability(., "- typic:quants", 10, 20000)
}
