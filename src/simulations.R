##
## simulating data
##

here::i_am("src/simulations.R")

library(here)
library(dplyr)
library(tidyr)
library(designr)

source(here("src/sim_functions.R"))

set.seed(123)

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
    quants = ifelse(quan_cond == "EEN", -1, 1),
    typic = ifelse(cond %in% c("a", "b", "e", "f"), 1, -1),
    interf = ifelse(cond %in% c("a", "c", "e", "g"), 1, -1),
  )




pp_lognorm <- function(.number_sim = number_sim,
                       .simdata = simdata, write = TRUE) {
  frm <- "~ quants * typic * interf +
 (quants * typic * interf | subj) + (quants * typic * interf | item)"
  simd <- msimulate(
    .number_sim, .simdata, priors, 8, 8, 8,
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
    write.csv(sim_summary, here("results/sim_summary_lognorm.csv"),
      row.names = FALSE
    )
  }
}
