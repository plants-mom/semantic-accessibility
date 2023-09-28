##
## bayesian models
##

here::i_am("src/03-models.R")

library(here)
library(dplyr)
library(readr)
library(brms)
library(purrr)

options(mc.cores = parallel::detectCores())


full_models <- function(data_list, dv_name, .priors, remove_zeros = TRUE,
                        .family = NULL,
                        optimize_mem = FALSE,
                        unique_name = FALSE) {
  frm <- formula(~ 1 + typic * interf * quants +
    (1 + typic * interf * quants | item) +
    (1 + typic * interf * quants | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  if (is.null(.family)) {
    .family <- ifelse(dv_name %in% c("gbck", "rr"), "bernoulli", "lognormal")
  }

  nms <- data_list %>%
    map(~ select(., region)) %>%
    map_dfr(unique) %>%
    pull()

  sel_data <- prepare_data(data_list, dv_name, remove_zeros)

  if (unique_name == TRUE) {
    mname <- paste(format(Sys.time(), "%s"), dv_name, "r", sep = "_")
  } else {
    mname <- paste(dv_name, "r", sep = "_")
  }

  full_ms <- sel_data %>%
    map(\(dat) brm(frm,
      family = .family,
      prior = .priors,
      iter = 4000, data = dat,
      file = here("models", paste0(mname, dat$region[1]))
    ))


  if (optimize_mem == TRUE) {
    rm(full_ms, sel_data, nms)
    gc()
  } else {
    rm(sel_data, nms)
    return(full_ms)
  }
}

prepare_data <- function(data_list, dv_name, remove_zeros) {
  nms <- data_list %>%
    map(~ select(., region)) %>%
    map_dfr(unique) %>%
    pull()

  if (remove_zeros == TRUE) {
    map(
      data_list,
      ~ select(., region:item, quan_cond:last_col(), all_of(dv_name))
    ) %>%
      map(.x = ., ~ filter(., .data[[dv_name]] > 0)) %>%
      set_names(paste0("region_", nms))
  } else {
    map(
      data_list,
      ~ select(., region:item, quan_cond:last_col(), all_of(dv_name))
    ) %>% set_names(paste0("region_", nms))
  }
}


## nested_models
## we want to compare effects of subj and obj
## and their interaction within each quantifier
## first postion: subj, second: obj
##
## |      |         | subj_een | obj_een | subj_geen | obj_geen |
## | een  | m - m   |        1 |       1 |         0 |        0 |
## |      | m - mm  |        1 |      -1 |         0 |        0 |
## |      | mm - m  |       -1 |       1 |         0 |        0 |
## |      | mm - mm |       -1 |      -1 |         0 |        0 |
## | geen | m - m   |        0 |       0 |         1 |        1 |
## |      | m - mm  |        0 |       0 |         1 |       -1 |
## |      | mm - m  |        0 |       0 |        -1 |        1 |
## |      | mm - mm |        0 |       0 |        -1 |       -1 |

nested_models <- function(data_list,
                          dv_name,
                          .priors = priors,
                          remove_zeros = TRUE,
                          .family = NULL,
                          optimize_mem = FALSE,
                          unique_name = FALSE) {
  if (is.null(.family)) {
    .family <- ifelse(dv_name %in% c("gbck", "rr"), "bernoulli", "lognormal")
  }

  if (unique_name == TRUE) {
    mname <- paste(format(Sys.time(), "%s"), dv_name, "r", sep = "_")
  } else {
    mname <- paste("nested", dv_name, "r", sep = "_")
  }


  mdata <- prepare_data(data_list, dv_name, remove_zeros) %>%
    map(\(dat)
    mutate(dat,
      subj_een = case_when(
        subj_cond == "MATCH" & obj_cond == "MATCH" & quan_cond == "EEN" ~ 1,
        subj_cond == "MATCH" & obj_cond == "MIS" & quan_cond == "EEN" ~ 1,
        subj_cond == "MIS" & obj_cond == "MATCH" & quan_cond == "EEN" ~ -1,
        subj_cond == "MIS" & obj_cond == "MIS" & quan_cond == "EEN" ~ -1,
        quan_cond == "GEEN" ~ 0
      ),
      obj_een = case_when(
        subj_cond == "MATCH" & obj_cond == "MATCH" & quan_cond == "EEN" ~ 1,
        subj_cond == "MATCH" & obj_cond == "MIS" & quan_cond == "EEN" ~ -1,
        subj_cond == "MIS" & obj_cond == "MATCH" & quan_cond == "EEN" ~ 1,
        subj_cond == "MIS" & obj_cond == "MIS" & quan_cond == "EEN" ~ -1,
        quan_cond == "GEEN" ~ 0
      ),
      subj_geen = case_when(
        subj_cond == "MATCH" & obj_cond == "MATCH" & quan_cond == "GEEN" ~ 1,
        subj_cond == "MATCH" & obj_cond == "MIS" & quan_cond == "GEEN" ~ 1,
        subj_cond == "MIS" & obj_cond == "MATCH" & quan_cond == "GEEN" ~ -1,
        subj_cond == "MIS" & obj_cond == "MIS" & quan_cond == "GEEN" ~ -1,
        quan_cond == "EEN" ~ 0
      ),
      obj_geen = case_when(
        subj_cond == "MATCH" & obj_cond == "MATCH" & quan_cond == "GEEN" ~ 1,
        subj_cond == "MATCH" & obj_cond == "MIS" & quan_cond == "GEEN" ~ -1,
        subj_cond == "MIS" & obj_cond == "MATCH" & quan_cond == "GEEN" ~ 1,
        subj_cond == "MIS" & obj_cond == "MIS" & quan_cond == "GEEN" ~ -1,
        quan_cond == "EEN" ~ 0
      )
    ))

  frm <- formula(~ 1 + subj_een * obj_een + subj_geen * obj_geen +
    (1 + subj_een * obj_een + subj_geen * obj_geen | item) +
    (1 + subj_een * obj_een + subj_geen * obj_geen | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  nested_ms <- mdata %>%
    map(~ select(., -c(
      quants, quan_cond,
      typic_cond, interf_cond,
      cond, quants, typic,
      interf, subj_cond, obj_cond
    ))) %>%
    map(\(dat) brm(frm,
      family = .family,
      prior = .priors,
      iter = 4000,
      data = dat,
      file = here("models", paste0(mname, dat$region[1]))
    ))

  if (optimize_mem == TRUE) {
    rm(nested_ms)
  } else {
    return(nested_ms)
  }
}


##
## this is just for the models split on
## typicality and quantifier, to illustrate the cue-based effect
##
split_models <- function(data_list,
                         dv_name,
                         .priors = priors,
                         remove_zeros = TRUE,
                         .family = NULL,
                         optimize_mem = FALSE,
                         unique_name = FALSE) {
  if (is.null(.family)) {
    .family <- ifelse(dv_name %in% c("gbck", "rr"), "bernoulli", "lognormal")
  }

  if (unique_name == TRUE) {
    mname <- paste(format(Sys.time(), "%s"), dv_name, "r", sep = "_")
  } else {
    mname <- paste(dv_name, "r", sep = "_")
  }

  mdata <- prepare_data(data_list, dv_name, remove_zeros)

  frm <- formula(~ 1 + interf +
    (1 + interf | item) +
    (1 + interf | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  split_ms <- mdata %>%
    map(~ select(., -quants, -typic)) %>%
    map(~ split(., list(.$quan_cond, .$typic_cond), sep = "_")) %>%
    map(~ imap(., ~ brm(frm,
      family = .family,
      prior = .priors,
      iter = 4000,
      data = .x,
      file = here(
        "models",
        paste0(
          dv_name, "_",
          tolower(.y), "_r", .x$region[1]
        )
      )
    )))

  return(split_ms)
}



fit_main_measures <- function(data_list) {
  c("rrdur", "totfixdur") %>%
    walk(~ full_models(data_list, ., .priors = priors, optimize_mem = TRUE))

  c("tgdur", "gdur") %>%
    walk(~ full_models(data_list[seq(5, 8)],
      .,
      .priors = priors, optimize_mem = TRUE
    ))
}


fit_count_measures <- function(data_list) {
  ## 0s here are missing data
  map(data_list, ~ filter(., gbck != 0)) %>%
    map(., ~ mutate(., gbck = abs(gbck - 2))) %>%
    full_models(., "gbck",
      .priors = priors_binom,
      remove_zeros = FALSE, optimize_mem = TRUE
    )

  ## 0s here are meaningful
  full_models(data_list, "rr",
    .priors = priors_binom,
    remove_zeros = FALSE, optimize_mem = TRUE
  )
}

fit_count_measures_nested <- function(data_list) {
  prepare_gbck <- compose(
    \(data) mutate(data, gbck = abs(gbck - 2)),
    \(data) filter(data, gbck != 0)
  )

  map(data_list, prepare_gbck) %>%
    nested_models(., "gbck",
      .priors = priors_binom,
      remove_zeros = FALSE, optimize_mem = TRUE
    )

  nested_models(data_list, "rr",
    .priors = priors_binom,
    remove_zeros = FALSE, optimize_mem = TRUE
  )
}

fit_main_measures_nested <- function(data_list) {
  c("rrdur", "totfixdur") %>%
    walk(~ nested_models(data_list, ., .priors = priors, optimize_mem = TRUE))

  c("tgdur", "gdur", "rpdur") %>%
    walk(\(var) nested_models(data_list[seq(5, 8)],
      var,
      .priors = priors, optimize_mem = TRUE
    ))
}

## models needed for cue-based plot
fit_split_models <- function(data_list) {
  c("tgdur", "totfixdur") %>%
    walk(\(var) split_models(data_list[seq(6, 8)], var, .priors = priors))
}

main <- function() {
  source(here("src/priors.R"))

  dfs <- list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .)))

  ## this might run out of memory
  ## if this happens, run again
  fit_main_measures(dfs)
  fit_count_measures(dfs)
  fit_count_measures_nested(dfs)
  fit_main_measures_nested(dfs)
  fit_split_models(dfs)
}

if (sys.nframe() == 0) {
  main()
}
