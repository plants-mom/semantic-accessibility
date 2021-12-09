##
## simulating data
##

here::i_am("src/simulations.R")

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(extraDistr)
library(designr)

source(here("src/sim_functions.R"))

ppc_lognorm <- function(.number_sim = number_sim,
                        .simdata = simdata,
                        .priors = priors,
                        write = TRUE) {
  frm <- "~ quants * typic * interf +
 (quants * typic * interf | subj) + (quants * typic * interf | item)"
  simd <- msimulate(
    .number_sim, .simdata, .priors, 8, 8, 8,
    "gaussian", frm
  )

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

  if (write == TRUE) {
    write.csv(sim_summary,
      here(
        "results",
        paste0("sim_summary_lognorm_", Sys.Date(), ".csv")
      ),
      row.names = FALSE
    )
  }
}

ppc_binom <- function(.number_sim = number_sim, .simdata = simdata) {
  ## prior predictive checks for the binomial models

  frm <- "~ quants * typic * interf +
 (quants * typic * interf | subj) + (quants * typic * interf | item)"
  simd <- msimulate(
    .number_sim, .simdata, priors_binom, 8, 8, 8,
    "binomial", frm,
    intercept_above_zero = FALSE, estimates = FALSE
  )

  write_csv(
    simd$true_params,
    here(
      "results",
      paste0("ppc_params_sample_binom_", Sys.Date(), ".csv")
    )
  )
  ## return(simd)
}

simdataf <- function() {
  design.codes(
    fixed.factor("quan_cond", levels = c("EEN", "GEEN")) +
      fixed.factor("subj_cond", levels = c("MATCH", "MIS")) +
      fixed.factor("obj_cond", levels = c("MATCH", "MIS")) +
      random.factor("subj", instances = 48) +
      random.factor("item", instances = 32)
  ) %>%
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
      quants = ifelse(quan_cond == "EEN", -1, 1),
      typic = ifelse(cond %in% c("a", "b", "e", "f"), 1, -1),
      interf = ifelse(cond %in% c("a", "c", "e", "g"), 1, -1),
    )
}

mixture <- function(alpha = 5.8, gamma = 5.2, .simdata = simdataf(),
                    beta = 0.05, beta2 = 0.05, sigma1 = .4, sigma2 = .5) {
  N <- nrow(.simdata)
  ## Parameters true values
  ## p_task <- .8
  .simdata %>%
    mutate(rt = if_else(
             ## quan_cond == "EEN",
             cond %in% c("a", "e"),
             rlnorm(N,
                    meanlog = gamma + typic * beta + interf * beta2,
                    sdlog = sigma1),
             rlnorm(N,
                    meanlog = alpha + typic * beta + interf * beta2,
                    sdlog = sigma2
      )
    ))
}

if (sys.nframe() == 0) {
  library(ggplot2)
  source(here("src/priors.R"))
  set.seed(123)
  ## ppc_binom(1e3)
  ## ppc_lognorm(1000, .priors = priors_ni_big_sd, .simdata = simdataf())

  mixture(alpha = 5.3, gamma = 5.97, sigma1 = 0.2) %>%
    ggplot(aes(rt)) +
    geom_density()
}
