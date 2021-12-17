##
## bayesian models
## NB: this might sometimes crash, just run it one more time
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

  ## message(dv_name, typeof(dv_name))
  ## message(str(data_list))

  if (remove_zeros == TRUE) {
    sel_data <- map(
      data_list,
      ~ select(., region:item, quan_cond:last_col(), {{ dv_name }})
    ) %>%
      map(.x = ., ~ filter(., .data[[dv_name]] > 0)) %>%
      set_names(paste0("region_", nms))
  } else {
    sel_data <- map(
      data_list,
      ~ select(., region:item, quan_cond:last_col(), {{ dv_name }})
    ) %>% set_names(paste0("region_", nms))
  }

  ## return(sel_data)
  ##
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


fit_split <- function(data_list, dv_name, split_by, .priors = priors,
                      remove_zeros = TRUE, .family = NULL, optimize_mem = FALSE,
                      unique_name = FALSE,
                      ) {

  frm <- formula(~ 1 + typic * interf +
    (1 + typic * interf | item) +
    (1 + typic * interf | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  if (is.null(.family)) {
    .family <- ifelse(dv_name %in% c("gbck", "rr"), "bernoulli", "lognormal")
  }

  nms <- data_list %>%
    map(~ select(., region)) %>%
    map_dfr(unique) %>%
    pull()

  ## message(dv_name, typeof(dv_name))
  ## message(str(data_list))

  if (remove_zeros == TRUE) {
    sel_data <- map(
      data_list,
      ~ select(., region:item, quan_cond:last_col(), {{ dv_name }})
    ) %>%
      map(.x = ., ~ filter(., .data[[dv_name]] > 0)) %>%
      set_names(paste0("region_", nms))
  } else {
    sel_data <- map(
      data_list,
      ~ select(., region:item, quan_cond:last_col(), {{ dv_name }})
    ) %>% set_names(paste0("region_", nms))
  }

  ## return(sel_data)
  ##
  if (unique_name == TRUE) {
    mname <- paste(format(Sys.time(), "%s"), dv_name, "r", sep = "_")
  } else {
    mname <- paste(dv_name, "r", sep = "_")
  }


  sel_data <- map(sel_data, ~ select(., -quants))

  split_ms <- map(sel_data, ~ split(., .$quan_cond)) %>%
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

  if (optimize_mem == TRUE) {
    rm(split_ms, sel_data, frm)
    gc()
  } else {
    return(split_ms)
  }
}


fit_models <- function(data_list, dv_name, .priors, remove_zeros = TRUE,
                       .return = c(
                         "both", "none", "split_models",
                         "full_models"
                       ),
                       .family = NULL,
                       optimize_mem = FALSE,
                       unique_name = FALSE) {
  ##
  ## I leave optimize_mem so it lack won't bite me somewhere
  ##

  .return <- match.arg(.return)

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

  ## message(dv_name, typeof(dv_name))
  ## message(str(data_list))

  if (remove_zeros == TRUE) {
    sel_data <- map(
      data_list,
      ~ select(., region:item, quan_cond:last_col(), {{ dv_name }})
    ) %>%
      map(.x = ., ~ filter(., .data[[dv_name]] > 0)) %>%
      set_names(paste0("region_", nms))
  } else {
    sel_data <- map(
      data_list,
      ~ select(., region:item, quan_cond:last_col(), {{ dv_name }})
    ) %>% set_names(paste0("region_", nms))
  }

  ## return(sel_data)
  ##
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


  if (optimize_mem == TRUE || .return %in% c("none", "split_models")) {
    rm(full_ms)
    gc()
  } else if (optimize_mem == TRUE && .return == "full_models") {
    rm(full_ms)
    return()
  } else if (.return == "full_models") {
    rm(sel_data, nms)
    return(list(full_models = full_ms))
  }

  frm <- formula(~ 1 + typic * interf +
    (1 + typic * interf | item) +
    (1 + typic * interf | subj)) %>%
    update.formula(paste0(dv_name, "~ . "))

  sel_data <- map(sel_data, ~ select(., -quants))

  split_ms <- map(sel_data, ~ split(., .$quan_cond)) %>%
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

  if (optimize_mem == TRUE || .return == "none") {
    rm(split_ms, sel_data, frm)
    gc()
  } else if (.return == "split_models") {
    return(list(split_models = split_ms))
  } else if (.return == "both") {
    return(list(full_models = full_ms, split_models = split_ms))
  }
}

get_model <- function(dv_name, region, type = "full_models") {
  list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .))) %>%
    .[region] %>%
    fit_models(., dv_name,
      .return = type
    ) %>%
    pluck(type, paste0("region_", region))
}


if (sys.nframe() == 0) {
  source(here("src/priors.R"))

  dfs <- list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .)))

  ## c("rrdur", "totfixdur") %>%
  ##   walk(~ fit_models(dfs, ., .priors = priors, optimize_mem = TRUE))

  ## map(dfs, ~ filter(., gbck != 0)) %>%
  ##   map(., ~ mutate(., gbck = abs(gbck - 2))) %>%
  ##   fit_models(., "gbck",
  ##     .priors = priors_binom,
  ##     remove_zeros = FALSE, optimize_mem = TRUE
  ##   )

  ## fit_models(dfs, "rr",
  ##   .priors = priors_binom,
  ##   remove_zeros = FALSE, optimize_mem = TRUE
  ## )

  c("gdur", "tgdur", "rpdur") %>%

gd <- "gbck" %>%
    map(~ fit_full(dfs[5:6], .,
      .priors = priors
    ))

glimpse(gd)


  c("gdur", "tgdur", "rpdur") %>%
    walk(~ fit_models(dfs[6], .,
      .priors = priors_gamma,
      ## .family = "shifted_lognormal",
      .family = "Gamma",
      optimize_mem = TRUE, .return = "full_models", unique_name = TRUE
    ))

  ##   c("gdur", "tgdur", "rpdur") %>%
  ##     walk(~ fit_models(dfs[6:8], .,
  ##       .priors = priors_ni_big_sd_sigma,
  ##       optimize_mem = TRUE, .return = "full_models", unique_name = TRUE
  ##     ))
}

frm <- formula(~ 1 + typic * interf +
  (1 + typic * interf | item) +
    (1 + typic * interf | subj))



update.formula(frm, . ~ . - interf)
frm


%>%
  update.formula(paste0(dv_name, "~ . "))
