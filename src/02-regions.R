##
## writing different regions
##

here::i_am("src/regions.R")

library(here)
library(readr)
library(purrr)
library(dplyr)

make_regions <- function(data_in) {
  dataf <- data_in %>%
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
      typic = ifelse(cond %in% c("a", "b", "e", "f"), 1, -1), # subj
      interf = ifelse(cond %in% c("a", "c", "e", "g"), 1, -1), # obj
      typic_cond = ifelse(typic == 1, "typical", "atypical"),
      interf_cond = ifelse(interf == 1, "interf", "no_interf"),
      ## additional variables
      ## rrdur = totfixdur - (gdur - gsacc), # re-reading duration
      ## corrected re-reading duration;
      ## we don't subtract gsacc because that was already done
      rrdur = totfixdur - gdur,
      rr = as.numeric(rrdur > 0)
    )


  vars <- c(
    "subj", "item", "rpdur", "tgdur", "totfixdur", "gbck",
    "gdur", "rr", "rrdur"
  )

  dataf %>%
    select(
      region,
      all_of(vars),
      quan_cond,
      typic_cond,
      interf_cond,
      cond:interf
    ) %>%
    split(.$region)
}

main <- function() {
  make_regions(
    read_csv(here("results/allACTFiles_clean_sacccadtetime_removed.csv"))
  ) %>%
    walk(~ write_csv(
      .x,
      here("results", paste0("region", .$region[1], ".csv"))
    ))
}

if (sys.nframe() == 0) {
  main()
}
