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


files <- list.files(here("results"), pattern = "region[0-9].csv")
dfs <- map(files, ~ read_csv(here("results", .)))


totfixdurs <- map(dfs, ~ select(., -ffdur, -(gbck:rrdur))) %>%
  imap(~ brm(totfixdur ~ 1 + quants + typic + interf + quant_x_typic +
    quant_x_inter + quant_x_interf_TYP + quant_x_interf_ATY +
    (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
      quant_x_interf_TYP + quant_x_interf_ATY | subj) +
    (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
      quant_x_interf_TYP + quant_x_interf_ATY | item),
  family = lognormal(),
  prior = priors,
  iter = 4000,
  data = .x,
  file = here("models", paste0("totfixdur_r", .y))
  ))

ffdurs <- dfs[6:8] %>%
  map(~ select(., -(totfixdur:rrdur))) %>%
  imap(~ brm(ffdur ~ 1 + quants + typic + interf + quant_x_typic +
               quant_x_inter + quant_x_interf_TYP + quant_x_interf_ATY +
    (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
      quant_x_interf_TYP + quant_x_interf_ATY | subj) +
    (1 + quants + typic + interf + quant_x_typic + quant_x_inter +
      quant_x_interf_TYP + quant_x_interf_ATY | item),
  family = lognormal(),
  prior = priors,
  iter = 4000,
  data = .x,
  file = here("models", paste0("ffdur_r", .y))
  ))




## rrdurs <- map(dfs, ~ select(., -(ffdur:rr))) %>%
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
