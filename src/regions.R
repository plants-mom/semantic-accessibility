##
## writing different regions
##

here::i_am("src/regions.R")

library(here)
library(readr)
library(purrr)
library(dplyr)
library(hypr)


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
    quants = ifelse(quan_cond == "EEN", 0.5, -0.5),
    typic = ifelse(cond %in% c("a", "b", "e", "f"), 0.5, -0.5),
    interf = ifelse(cond %in% c("a", "c", "e", "g"), 0.5, -0.5),
    quant_x_typic = ifelse(cond %in% c("a", "b", "g", "h"), 0.5, -0.5),
    quant_x_inter = ifelse(cond %in% c("a", "c", "f", "h"), 0.5, -0.5),
    quant_x_interf_TYP = ifelse(cond %in% c("a", "f"), 0.5,
                      ifelse(cond %in% c("b", "e"), -0.5, 0)),
    quant_x_interf_ATY = ifelse(cond %in% c("c", "h"), 0.5,
                         ifelse(cond %in% c("d", "g"), -0.5, 0)),
    ## additional variables
    rrdur = totfixdur - (gdur - gsacc), # re-reading duration
    rr = rrdur > 0
  )


## contr_mat <- unique(select(dataf, quants:quant_x_interf_ATY))
## h <- hypr()
## cmat(h, add_intercept = TRUE) <- as.matrix(contr_mat)

vars <- c("subj", "item", "ffdur", "totfixdur", "gbck", "gdur", "rr", "rrdur")

dataf %>%
  filter(ffdur != 0) %>%
  select(region,
         all_of(vars),
    cond:quant_x_interf_ATY
    ) %>%
  split(.$region) %>%
  walk(~ write_csv(.x, here("results", paste0("region", .$region[1],".csv"))))
