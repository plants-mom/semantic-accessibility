##
## bayesian models
##

here::i_am("src/models.R")

library(here)
library(dplyr)
library(readr)
library(brms)
library(purrr)

options(mc.cores = parallel::detectCores())


fit_full <- function(data_list, dv_name, .priors, remove_zeros = TRUE,
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

  sel_data <- rem_zeros(data_list, dv_name, remove_zeros)

  if (unique_name == TRUE) {
    mname <- paste(format(Sys.time(), "%s"), dv_name, "r", sep = "_")
  } else {
    mname <- paste(dv_name, "r", sep = "_")
  }

  full_ms <- sel_data %>%
    map(~ brm(frm,
      family = .family,
      prior = .priors,
      iter = 4000, data = .x,
      file = here("models", paste0(mname, .x$region[1]))
    ))


  if (optimize_mem == TRUE) {
    rm(full_ms, sel_data, nms)
    gc()
  } else {
    rm(sel_data, nms)
    return(full_ms)
  }
}

rem_zeros <- function(data_list, dv_name, remove_zeros) {
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


fit_split <- function(data_list, dv_name, split_by = c("quant", "quant_typic"),
                      .priors = priors,
                      remove_zeros = TRUE, .family = NULL, optimize_mem = FALSE,
                      unique_name = FALSE) {

  ##
  ## this is just for two cases: either splitting by quantifier
  ## or splitting by typicality and quantifier
  ## would look much better if made more general
  ##

  if (is.null(.family)) {
    .family <- ifelse(dv_name %in% c("gbck", "rr"), "bernoulli", "lognormal")
  }

  if (unique_name == TRUE) {
    mname <- paste(format(Sys.time(), "%s"), dv_name, "r", sep = "_")
  } else {
    mname <- paste(dv_name, "r", sep = "_")
  }

  sel_data <- rem_zeros(data_list, dv_name, remove_zeros)

  split <- match.arg(split_by)

  if (split == "quant") {
    frm <- formula(~ 1 + typic * interf +
      (1 + typic * interf | item) +
      (1 + typic * interf | subj)) %>%
      update.formula(paste0(dv_name, "~ . "))

    split_ms <- sel_data %>%
      map(~ select(., -quants)) %>%
      map(~ split(., .$quan_cond)) %>%
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
  } else if (split == "quant_typic") {
    frm <- formula(~ 1 + interf +
      (1 + interf | item) +
      (1 + interf | subj)) %>%
      update.formula(paste0(dv_name, "~ . "))

    ## message(dv_name, typeof(dv_name))
    ## message(str(data_list))

    split_ms <- sel_data %>%
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
  } else {
    message("not implemented")
    return()
  }

  if (optimize_mem == TRUE) {
    rm(split_ms, sel_data, frm)
    gc()
  } else {
    return(split_ms)
  }
}

fit_models <- function(data_list, dv_name, .priors,
                       .return = c("full_models", "split_models", "all"),
                       remove_zeros = TRUE,
                       .family = NULL,
                       optimize_mem = FALSE,
                       unique_name = FALSE) {
  .return <- match.arg(.return)
  if (.return == "full_models") {
    return(fit_full(
      data_list, dv_name, .priors, remove_zeros,
      .family, optimize_mem, unique_name
    ))
  } else if (.return == "split_models") {
    return(fit_split(
      data_list = data_list, dv_name = dv_name, split_by = "quant",
      .priors = .priors, remove_zeros = remove_zeros, .family = .family
    ))
  } else if (.return == "all") {
    full_ms <- fit_full(
      data_list, dv_name, .priors, remove_zeros,
      .family, optimize_mem, unique_name
    )
    split_ms <- fit_split(
      data_list = data_list, dv_name = dv_name, split_by = "quant",
      .priors = .priors, remove_zeros = remove_zeros, .family = .family
    )
    return(list(full_models = full_ms, split_models = split_ms))
  }
}


get_model <- function(dv_name, region, type = "full_models") {
  fit_model_func <- ifelse(type == "full_models", fit_full, fit_split)
  list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .))) %>%
    .[region] %>%
    fit_model_func(., dv_name) %>%
    pluck(paste0("region_", region))
}


if (sys.nframe() == 0) {
  source(here("src/priors.R"))

  dfs <- list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .)))

  map(dfs[6], ~ filter(., gbck != 0)) %>%
    map(., ~ mutate(., gbck = abs(gbck - 2))) %>%
    fit_split(., "gbck",
      split_by = "quant_typic",
      .priors = priors_binom,
      remove_zeros = FALSE, optimize_mem = TRUE
    )

  "totfixdur" %>%
    walk(~ fit_split(dfs[7], .,
      split_by = "quant_typic",
      .priors = priors
    ))
}
