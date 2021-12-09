##
## fitting stan models
##

here::i_am("src/stan_models.R")

library(here)
library(dplyr)
library(purrr)
library(rstan)
library(readr)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


r6 <- read_csv(here("results/region6.csv"))


tgdur <- r6 %>%
  filter(tgdur != 0) %>%
  mutate(cond = map_dbl(cond, ~ which(letters == .))) %>%
  select(subj, item, tgdur, quan_cond:last_col())

tgdur_data <- with(tgdur, list(
  rt = tgdur, J = max(subj), K = max(item),
  subj = subj, item = item, quant = quants,
  typic = typic, interf = interf
))

tgdur_data$N <- nrow(tgdur)

model <- "mixture_int_typ"

fit <- stan(
  file = here("src", paste0(model, ".stan")),
  data = tgdur_data,
  control = list(adapt_delta = 0.99),
  iter = 4000
)
saveRDS(fit, here("models", paste0(model, ".rds")))
