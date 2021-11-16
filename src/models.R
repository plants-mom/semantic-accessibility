##
## bayesian models
##

here::i_am("src/models.R")

library(here)
library(dplyr)
library(readr)
library(readr)
library(brms)
library(purrr)

options(mc.cores = parallel::detectCores())

source(here("src/priors.R"))

## split_by_quant(data_list) <-

files <- list.files(here("results"), pattern = "region[0-9].csv")
dfs <- map(files, ~ read_csv(here("results", .)))

frm <- formula(totfixdur ~ 1 + quants + typic + interf + quant_x_typic +
    quant_x_inter + quant_x_interf_TYP + quant_x_interf_ATY +
    (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
      quant_x_interf_TYP + quant_x_interf_ATY | subj) +
    (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
      quant_x_interf_TYP + quant_x_interf_ATY | item))

frm <- formula(totfixdur ~ 1 + typic * interf * quants + (1 + typic * interf * quants | item) +
                 (1 + typic * interf * quants | subj))

map(dfs, ~ split(., .$quan_cond)) %>%
 map(~ map(., ~ lm(tgdur ~ 1, data = .)))

totfixdurs <- map(dfs, ~ select(., region:item, totfixdur, cond, typic, interf, quants)) %>%
  ## map(dfs, ~ filter(., quan_cond == "EEN")) %>%
  imap(~ brm(frm,
  family = lognormal(),
  prior = priors,
  iter = 4000,
  data = .x,
  file = here("models", paste0("totfixdur_r", .y))
  ))

totfixdurs_geen <- map(dfs, ~ filter(., quan_cond == "GEEN")) %>%
map(~ select(., region:item, totfixdur, cond, typic, interf)) %>%
  imap(~ brm(frm,
  family = lognormal(),
  prior = priors,
  iter = 4000,
  data = .x,
  file = here("models", paste0("totfixdur_geen_r", .y))
  ))

frm <- update.formula(frm, ffdur ~ .)

## ffdurs_een <- dfs[6:8] %>%
##   map(~ filter(., quan_cond == "EEN")) %>%
##   map(~ select(., region:item, ffdur, cond, typic, interf)) %>%
##   imap(~ brm(frm,
##   family = lognormal(),
##   prior = priors,
##   iter = 4000,
##   data = .x,
##   file = here("models", paste0("ffdur_een_r", .y))
##   ))


## ffdurs_geen <- dfs[6:8] %>%
##   map(~ filter(., quan_cond == "GEEN")) %>%
##   map(~ select(., region:item, ffdur, cond, typic, interf)) %>%
##   imap(~ brm(frm,
##   family = lognormal(),
##   prior = priors,
##   iter = 4000,
##   data = .x,
##   file = here("models", paste0("ffdur_geen_r", .y))
##   ))



## rrdurs <- map(dfs, ~ select(., -(ffdur:rr))) %>%
##   map( ~ mutate(., rrdur = if_else(rrdur == 0, 1, rrdur))) %>%
##   imap(~ brm(rrdur ~ 1 + quants + typic + interf + quant_x_typic +
##     quant_x_inter + quant_x_interf_TYP + quant_x_interf_ATY +
##     (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
##       quant_x_interf_TYP + quant_x_interf_ATY | subj) +
##     (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
##       quant_x_interf_TYP + quant_x_interf_ATY | item),
##   family = lognormal(),
##   prior = priors,
##   iter = 4000,
##   data = .x,
##   file = here("models", paste0("rrdur_r", .y))
##   ))

## REMEMBER different priors
##
## rrs <- map(dfs, ~ select(., -(ffdur:gdur), -rrdur)) %>%
##   imap(~ brm(rr ~ 1 + quants + typic + interf + quant_x_typic + quant_x_inter +
##     quant_x_interf_TYP + quant_x_interf_ATY +
##     (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
##       quant_x_interf_TYP + quant_x_interf_ATY | subj) +
##     (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
##       quant_x_interf_TYP + quant_x_interf_ATY | item),
##   family = binomial(link = "probit"),
##   prior = priors,
##   iter = 4000,
##   data = .x,
##   file = here("models", paste0("rr_r", .y))
##   ))


l2 <- list(
  list(num = 1:3,     letters[1:3]),
  list(num = 101:103, letters[4:6]),
  list()
)

l2 %>%
