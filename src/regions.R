##
## writing different regions
##

here::i_am("src/regions.R")

library(here)
library(readr)
library(purrr)
library(dplyr)


dataf <- read_csv(here("results/allACTFiles_vp_clean.csv")) %>%
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
    ## additional variables
    rrdur = totfixdur - (gdur - gsacc), # re-reading duration
    rr = as.numeric(rrdur > 0)
  )


vars <- c("subj", "item", "rpdur", "tgdur", "totfixdur", "gbck", "gdur", "rr", "rrdur")

dataf %>%
  select(
    region,
    all_of(vars),
    quan_cond,
    cond:interf
  ) %>%
  split(.$region) %>%
  walk(~ write_csv(.x, here("results", paste0("region", .$region[1], ".csv"))))
