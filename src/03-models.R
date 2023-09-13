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


split_models <- function(data_list,
                         dv_name,
                         split_by = c("quant", "quant_typic"),
                         .priors = priors,
                         remove_zeros = TRUE,
                         .family = NULL,
                         optimize_mem = FALSE,
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
    return(split_models(
      data_list = data_list, dv_name = dv_name, split_by = "quant",
      .priors = .priors, remove_zeros = remove_zeros, .family = .family
    ))
  } else if (.return == "all") {
    full_ms <- fit_full(
      data_list, dv_name, .priors, remove_zeros,
      .family, optimize_mem, unique_name
    )
    split_ms <- split_models(
      data_list = data_list, dv_name = dv_name, split_by = "quant",
      .priors = .priors, remove_zeros = remove_zeros, .family = .family
    )
    return(list(full_models = full_ms, split_models = split_ms))
  }
}


fit_main_measures <- function(data_list) {
  c("rrdur", "totfixdur") %>%
    walk(~ full_models(data_list, ., .priors = priors, optimize_mem = TRUE))
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

fit_count_measures_split <- function(data_list) {
  prepare_gbck <- compose(
    \(data) mutate(data, gbck = abs(gbck - 2)),
    \(data) filter(data, gbck != 0)
  )

  map(data_list, prepare_gbck) %>%
    split_models(., "gbck",
      split_by = "quant",
      .priors = priors_binom,
      remove_zeros = FALSE, optimize_mem = TRUE
    )

  map(data_list[c(6, 8)], prepare_gbck) %>%
    split_models(., "gbck",
      split_by = "quant_typic",
      .priors = priors_binom,
      remove_zeros = FALSE, optimize_mem = TRUE
    )

  split_models(data_list, "rr",
    split_by = "quant",
    .priors = priors_binom,
    remove_zeros = FALSE, optimize_mem = TRUE
  )
}

main <- function() {
  source(here("src/priors.R"))

  dfs <- list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .)))

  ## uncomment in the final: fit_main_measures(dfs)
  fit_count_measures(dfs)
  fit_count_measures_split(dfs)
}

if (sys.nframe() == 0) {
  main()

  ## "totfixdur" %>%
  ##   walk(~ fit_split(dfs[7], .,
  ##     split_by = "quant_typic",
  ##     .priors = priors
  ##   ))

  ## "totfixdur" %>%
  ##   walk(~ fit_split(dfs[8], .,
  ##     split_by = "quant_typic",
  ##     .priors = priors
  ##   ))
}
