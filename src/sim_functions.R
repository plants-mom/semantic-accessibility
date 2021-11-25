##
## helper functions for prior predictive checks
##

here::i_am("src/sim_functions.R")

library(here)
library(dplyr)
library(designr)
library(purrr)

source(here("src/SimFromPrior.R"))
source(here("src/priors.R"))


msimulate <- function(nsim, simdata, priors, nbetas, nsigmas, no_lm_coefs,
                      family, formula, intercept_above_zero = TRUE) {
  ## TODO nbetas, nsigmas, no_lm_coefs, shoudl be determined from the formula
  betas <- vector(mode = "list", length = nbetas) %>%
    set_names(paste0("beta", seq(nbetas)))

  sigma_u <- vector(mode = "list", length = nsigmas) %>%
    set_names(paste0("sigma_u", seq(nsigmas)))

  sigma_w <- vector(mode = "list", length = nsigmas) %>%
    set_names(paste0("sigma_w", seq(nsigmas)))

  rho_u <- rho_w <- NULL # we assume two grouping factors
  # TODO generalise

  if (family == "gaussian") {
    sigma <- NULL
  }

  lm_coefs <- matrix(NA, no_lm_coefs, nsim)

  rtsimmat <- matrix(NA, nrow(simdata), nsim)

  for (i in 1:nsim) {
    if (i %% 100 == 0) {
      message("iter no", i)
    }
    if (intercept_above_zero == TRUE) {
      tmp <- -1
      while (tmp < 0) { # sample from a half-normal distribution
        tmp <- SimFromPrior(priors, class = "Intercept", coef = "")
      }
      betas$beta1[i] <- tmp
    } else {
      betas$beta1[i] <- SimFromPrior(priors, class = "Intercept", coef = "")
    }
    for (j in seq(2, nbetas)) {
      betas[[j]][i] <- SimFromPrior(priors, class = "b")
    }

    for (s in seq(1, nsigmas)) {
      sigma_u[[s]][i] <- SimFromPrior(priors, class = "sd")
      sigma_w[[s]][i] <- SimFromPrior(priors, class = "sd")
    }
    rho_u[i] <- SimFromPrior(priors, class = "cor")
    rho_w[i] <- SimFromPrior(priors, class = "cor")

    if (family == "gaussian") {
      sigma[i] <- SimFromPrior(priors, class = "sigma")
    }


    fixef <- map_dbl(betas, i)


    if (family == "gaussian") {
      vc <- list(map_dbl(sigma_u, i), map_dbl(sigma_w, i), sigma[i])
    } else {
      vc <- list(map_dbl(sigma_u, i), map_dbl(sigma_w, i))
    }

    rtsimmat[, i] <- simLMM(
      formula = formula,
      dat = simdata,
      Fixef = fixef,
      VC_sd = vc,
      CP = c(rho_u[i], rho_w[i]), empirical = FALSE, mtol = 10, verbose = FALSE,
      family = family
    )


    tmp_results <- bind_cols(simdata, rtsimmat[, i],
      .name_repair = "minimal"
    ) %>%
      rename(result = last_col())

    frm <- update.formula(formula, "result ~ .")
    frm <- lme4::nobars(frm) # lm doesn't accept random effects

    lm_coefs[, i] <- coef(lm(frm,
      data = tmp_results
    ))
  }

  colnames(rtsimmat) <- colnames(rtsimmat, do.NULL = FALSE, prefix = "sim")
  rtsimmat <- as_tibble(rtsimmat)
  colnames(lm_coefs) <- colnames(lm_coefs, do.NULL = FALSE, prefix = "sim")
  lm_coefs <- as_tibble(lm_coefs)
  true_pars <- as_tibble(c(betas, sigma_u, sigma_w))
  true_pars["rho_u"] <- rho_u
  true_pars["rho_w"] <- rho_w

  return(list(
    fake_data = rtsimmat,
    true_params = true_pars,
    lm_coefs = lm_coefs
  ))
}
