##
## simulating data
##

here::i_am("src/simulations.R")

library(here)
library(dplyr)
library(tidyr)
library(designr)

source(here("src/priors.R"))
source(here("src/SimFromPrior.R"))

set.seed(123)
number_sim <- 1000

expdesign <- fixed.factor("quan_cond", levels = c("EEN", "GEEN")) +
  fixed.factor("subj_cond", levels = c("MATCH", "MIS")) +
  fixed.factor("obj_cond", levels = c("MATCH", "MIS")) +
  random.factor("subj", instances = 48) +
  random.factor("item", instances = 32)

simdata <- design.codes(expdesign) %>%
  mutate(
    cond = case_when(
      quan_cond == "EEN" & subj_cond == "MATCH" & obj_cond == "MATCH" ~ "a",
      quan_cond == "EEN" & subj_cond == "MATCH" & obj_cond == "MIS" ~ "b",
      quan_cond == "EEN" & subj_cond == "MIS" & obj_cond == "MATCH" ~ "c",
      quan_cond == "EEN" & subj_cond == "MIS" & obj_cond == "MIS" ~ "d",
      quan_cond == "GEEN" & subj_cond == "MATCH" & obj_cond == "MATCH" ~ "e",
      quan_cond == "GEEN" & subj_cond == "MATCH" & obj_cond == "MIS" ~ "f",
      quan_cond == "GEEN" & subj_cond == "MIS" & obj_cond == "MATCH" ~ "g",
      quan_cond == "GEEN" & subj_cond == "MIS" & obj_cond == "MIS" ~ "h"
    ),
    quants = ifelse(quan_cond == "EEN", 0.5, -0.5),
    typic = ifelse(cond %in% c("a", "b", "e", "f"), 0.5, -0.5),
    interf = ifelse(cond %in% c("a", "c", "e", "g"), 0.5, -0.5),
    quant_x_typic = ifelse(cond %in% c("a", "b", "g", "h"), 0.5, -0.5),
    quant_x_inter = ifelse(cond %in% c("a", "c", "f", "h"), 0.5, -0.5),
    quant_x_interf_TYP = ifelse(cond %in% c("a", "f"), 0.5,
      ifelse(cond %in% c("b", "e"), -0.5, 0)
    ),
    quant_x_interf_ATY = ifelse(cond %in% c("c", "h"), 0.5,
      ifelse(cond %in% c("d", "g"), -0.5, 0)
    )
  )


my_simulate <- function(nsim, simdata) {
  beta0 <- beta1 <- beta2 <- beta3 <- beta4 <- beta5 <- beta6 <- beta7 <- NA
  sigma_u0 <- sigma_u1 <- sigma_u2 <- sigma_u3 <- sigma_u4 <- sigma_u5 <- sigma_u6 <- sigma_u7 <- NA
  sigma_w0 <- sigma_w1 <- sigma_w2 <- sigma_w3 <- sigma_w4 <- sigma_w5 <- sigma_w6 <- sigma_w7 <- NA
  rho_u <- rho_w <- sigma <- NA
  lm_coefs <- matrix(NA, 8, nsim)

  rtsimmat <- matrix(NA, nrow(simdata), nsim)

  for (i in 1:nsim) {
    message("iter no", i)
    tmp <- -1
    while (tmp < 0) { # sample from a half-normal distribution
      tmp <- SimFromPrior(priors, class = "Intercept", coef = "")
    }
    beta0[i] <- tmp
    beta1[i] <- SimFromPrior(priors, class = "b")
    beta2[i] <- SimFromPrior(priors, class = "b")
    beta3[i] <- SimFromPrior(priors, class = "b")
    beta4[i] <- SimFromPrior(priors, class = "b")
    beta5[i] <- SimFromPrior(priors, class = "b")
    beta6[i] <- SimFromPrior(priors, class = "b")
    beta7[i] <- SimFromPrior(priors, class = "b")
    sigma_u0[i] <- SimFromPrior(priors, class = "sd")
    sigma_u1[i] <- SimFromPrior(priors, class = "sd")
    sigma_u2[i] <- SimFromPrior(priors, class = "sd")
    sigma_u3[i] <- SimFromPrior(priors, class = "sd")
    sigma_u4[i] <- SimFromPrior(priors, class = "sd")
    sigma_u5[i] <- SimFromPrior(priors, class = "sd")
    sigma_u6[i] <- SimFromPrior(priors, class = "sd")
    sigma_u7[i] <- SimFromPrior(priors, class = "sd")
    sigma_w0[i] <- SimFromPrior(priors, class = "sd")
    sigma_w1[i] <- SimFromPrior(priors, class = "sd")
    sigma_w2[i] <- SimFromPrior(priors, class = "sd")
    sigma_w3[i] <- SimFromPrior(priors, class = "sd")
    sigma_w4[i] <- SimFromPrior(priors, class = "sd")
    sigma_w5[i] <- SimFromPrior(priors, class = "sd")
    sigma_w6[i] <- SimFromPrior(priors, class = "sd")
    sigma_w7[i] <- SimFromPrior(priors, class = "sd")
    rho_u[i] <- SimFromPrior(priors, class = "cor")
    rho_w[i] <- SimFromPrior(priors, class = "cor")
    sigma[i] <- SimFromPrior(priors, class = "sigma")


    rtsimmat[, i] <- exp(simLMM(
      formula = ~ 1 + quants + typic + interf + quant_x_typic + quant_x_inter +
        quant_x_interf_TYP + quant_x_interf_ATY +
        (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
          quant_x_interf_TYP + quant_x_interf_ATY | subj) +
        (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
          quant_x_interf_TYP + quant_x_interf_ATY | item),
      dat = simdata,
      Fixef = c(beta0[i], beta1[i], beta2[i], beta3[i], beta4[i], beta5[i], beta6[i],
                beta7[i]),
      VC_sd = list(c(
        sigma_u0[i], sigma_u1[i], sigma_u2[i], sigma_u3[i],
        sigma_u4[i], sigma_u5[i], sigma_u6[i], sigma_u7[i]
      ), c(
        sigma_w0[i], sigma_w1[i], sigma_w2[i], sigma_w3[i],
        sigma_w4[i], sigma_w5[i], sigma_w6[i], sigma_w7[i]
      ), sigma[i]),
      CP = c(rho_u[i], rho_w[i]), empirical = FALSE, mtol = 10, verbose = FALSE
    ))


    tmp_results <- bind_cols(simdata, rtsimmat[, i]) %>%
      rename(rts = ...14)
    lm_coefs[, i] <- coef(lm(rts ~ quants + typic +
                               interf + quant_x_typic +
                               quant_x_inter + quant_x_interf_TYP +
                               quant_x_interf_ATY,
                             data = tmp_results))
  }

  colnames(rtsimmat) <- colnames(rtsimmat, do.NULL = FALSE, prefix = "sim")
  rtsimmat <- as_tibble(rtsimmat)
  colnames(lm_coefs) <- colnames(lm_coefs, do.NULL = FALSE, prefix = "sim")
  lm_coefs <- as_tibble(lm_coefs)

  return(list(
    fake_data = rtsimmat,
    ## true_params = true_pars,
    lm_coefs = lm_coefs
  ))
}

simd <- my_simulate(number_sim, simdata)




tmp_ie <- simd$lm_coef[6, ] %>%
  pivot_longer(
    cols = starts_with("sim"),
    names_to = "simulation", values_to = "ieff_rt"
  )


sim_summary <- simd$fake_data %>%
  pivot_longer(
    cols = starts_with("sim"),
    names_to = "simulation", values_to = "rt"
  ) %>%
  group_by(simulation) %>%
  summarize(
    min_rt = min(rt),
    max_rt = max(rt),
    mean_rt = mean(rt),
    sd_rt = sd(rt),
    median_rt = median(rt)
  ) %>%
  left_join(tmp_ie) %>%
  pivot_longer(
    cols = ends_with("rt"),
    names_to = "stat", values_to = "rt"
  )

write.csv(sim_summary, here("results/sim_summary.csv"),
  row.names = FALSE
)
