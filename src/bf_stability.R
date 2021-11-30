

library(here)
library(brms)
library(purrr)
library(bridgesampling)
options(mc.cores = parallel::detectCores())





bf_stability <- function(model, formula_term, iters, stan_iters) {
  results <- data.frame(matrix(vector(), iters, 3,
    dimnames = list(c(), c("iter", "bf_10", "stan_iterations"))
  ))

  frm <- formula(model) %>%
    update.formula(., paste0(". ~ . ", formula_term))

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
    write.csv(results, here("results/bf_stability.csv"), row.names = FALSE)
  }
}

if (sys.nframe() == 0) {
  source(here("src/models.R"))

  list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .))) %>%
    .[6] %>%
    fit_models(., "totfixdur",
      .return = "full_models"
    ) %>%
    pluck("full_models", "region_6") %>%
    bf_stability(., "- typic:quants", 10, 15000)
}
