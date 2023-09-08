##
##
##

here::i_am("src/explore.R")

library(here)
library(dplyr)
library(purrr)
library(readr)
library(brms)
library(fs)
library(bayesplot)
library(cowplot)
library(ggplot2)

  source(here("src/models.R"))

  r6_ffdur <- read_csv(here("results/r6_ffdur.csv"))

  ggplot(r6_ffdur, aes(y = log(ffdur), x = cond)) +
    geom_boxplot() +
    geom_jitter()

  plot(log(r6_ffdur$ffdur), data = r6_ffdur)

  ## understanding contrasts again

  sim <- data.frame(
    cond = rep(c("A", "B"), each = 5),
    res = c(rnorm(5, 200, 10), rnorm(5, 100, 10))
  )

  sim$contr <- ifelse(sim$cond == "A", 0.5, -0.5)
  sim$contr2 <- ifelse(sim$cond == "A", 1, -1)

  contrasts(sim$cond)

  lm(res ~ cond, data = sim)
  lm(res ~ contr2, data = sim)
  lm(res ~ contr, data = sim)
  mean(sim$res)

  r6 <- read_csv(here("results/region6.csv"))
  glimpse(r6)

  r6 %>%
    pull(totfixdur) %>%
    rank(., ties.method = "min")

  rank(r6$totfixdur, ties.method = "average")


  dfs <- list.files(here("results"), pattern = "region[0-9].csv") %>%
    map(~ read_csv(here("results", .)))

  rpdur <- fit_models(dfs[6], "rpdur", .return = "full_models")
  rpdur <- rpdur$full_models$region_6
  pp_check(rpdur, type = "stat", stat = "median")
  pp_check(rpdur, type = "dens_overlay", nsamples = 100)

source(here("src/models.R"))

  totf <- get_model("totfixdur", 6)

totf

ff <- fit_full(dfs[6], "totfixdur")

glimpse(ff)

  tgdur <- readRDS(here("models/old/tgdur_r6.rds"))
  pp_check(tgdur)
  pp_check(tgdur, type = "stat_2d", stat = c("median", "sd"))



  gbck <- get_model("gbck", 3)
  pp_check(gbck, nsamples = 100)

  gbck$data$gbck


  dataf <- data.frame(x = seq(0, 2000))

  ggplot(dataf, aes(x)) +
    stat_function(fun = function(x) dlnorm(x, meanlog = 5, sdlog = 10)) +
    ylab("density")


  plot(rpdur)

  tgdur %>%
    pluck("data") %>%
    ggplot(., aes(tgdur)) +
    geom_histogram()


source(here("src/models.R"))

rpdur <- get_model("rpdur", 6)
tgdur <- get_model("tgdur", 6)
gdur <- get_model("gdur", 6)

frm <- formula(rpdur ~ 1)

update(rpdur, formula. = frm, file = here("models/rpdur_test2"))

mtest <- readRDS(here("models/rpdur_test2.rds"))
pp_check(mtest, nsamples = 100, stat = "sd", type = "stat")

pp_check(tgdur, nsamples = 100, type = "scatter_avg_grouped", group = "quants")

ppc0 <- pp_check(tgdur, nsamples = 1000, type = "dens_overlay")
ppc1 <- pp_check(tgdur, nsamples = 1000, type = "stat", stat = "median")
ppc2 <- pp_check(tgdur, nsamples = 1000, type = "stat", stat = "max")
ppc3 <- pp_check(tgdur, nsamples = 1000, type = "stat", stat = "min")
ppc4 <- pp_check(tgdur, nsamples = 1000, type = "stat", stat = "mean")
ppc5 <- pp_check(tgdur, nsamples = 1000, type = "stat", stat = "sd")



ppc0 <- pp_check(gdur, nsamples = 1000, type = "dens_overlay")
ppc1 <- pp_check(gdur, nsamples = 1000, type = "stat", stat = "median")
ppc2 <- pp_check(gdur, nsamples = 1000, type = "stat", stat = "max")
ppc3 <- pp_check(gdur, nsamples = 1000, type = "stat", stat = "min")
ppc4 <- pp_check(gdur, nsamples = 1000, type = "stat", stat = "mean")
ppc5 <- pp_check(gdur, nsamples = 1000, type = "stat", stat = "sd")


pp <- plot_grid(ppc1, ppc2, ppc3, ppc4, ppc5)

ggsave(here("figs/gdur_ppcs.png"), pp)

## pp_check(rpdur, nsamples = 100, stat = "sd", type = "stat")

## 'bars', 'bars_grouped', 'boxplot', 'data', 'dens', 'dens_overlay',
## 'ecdf_overlay', 'error_binned', 'error_hist', 'error_hist_grouped',
## 'error_scatter', 'error_scatter_avg', 'error_scatter_avg_vs_x', 'freqpoly',
## 'freqpoly_grouped', 'hist', 'intervals', 'intervals_data', 'intervals_grouped',
## 'loo_intervals', 'loo_pit', 'loo_pit_overlay', 'loo_pit_qq', 'loo_ribbon', 'ribbon',
## 'ribbon_data', 'ribbon_grouped', 'rootogram', 'scatter', 'scatter_avg',
## 'scatter_avg_grouped', 'stat', 'stat_2d', 'stat_freqpoly_grouped',
## 'stat_grouped', 'violin_grouped'

min(tgdur$data$tgdur)

r6 <- read_csv(here("results/region6.csv"))

with(r6, mean(gdur))

r6 %>%
  group_by(subj) %>%
  summarise(sd_tg = sd(tgdur), mean_tg = mean(tgdur),
            sd_g = sd(gdur), mean_g = mean(gdur)
            ) %>%
  summarise(mean())


res <- dir_ls(here("models"), regexp =  "[0-9]+.*_r[6].rds") %>%
  map(readRDS) %>%
  set_names(~ path_file(.))


map(res, pp_check)

res %>%
  map("prior")

  map(head)


res <- dir_ls(here("models"), regexp =  "163853[0-9]{4}_[a-z]+_r[6].rds") %>%
  map(readRDS) %>%
  set_names(~ path_ext_remove(path_file(.)))

m1 <- readRDS(here("models/1638529737_tgdur_r6.rds"))
m2 <- readRDS(here("models/1638470105_tgdur_r6.rds"))

pp_check(m1, type = "stat_2d", stat = c("median", "sd"))
pp_check(m1, type = "stat", stat = "sd")
pp_check(m1)
pp_check(res[[1]], type = "stat", stat = "median")

res[[ 1 ]]

  map(~ pp_check(., nsamples = 100, type = "stat", stat = "median"))

prior_summary(res$`1638466346_gdur_r6.rds`) %>%
  filter(prior != "") %>%
  as_tibble()

  select(prior)


  mutate(source = path_file(source))

  map(readRDS)

  map_dfr(read_csv, .id = "source") %>%
  mutate(source = path_file(source))

res[[1]]$data

  he

res <- dir_ls(here("models"), regexp =  "[0-9]+.*_r[6-9].rds")

res %>%
  set_names(~ path_file(.))

tgdur <- readRDS(here("models/1639049329_tgdur_r6.rds"))
typeof(tgdur)
class(tgdur)
summary(tgdur)
pp_check(tgdur)
pp_check(tgdur, type = "stat_2d", stat = c("median", "sd"))
sort(unique(tgdur$data$item))


## check if the regions are of similar count

glimpse(dataf)

r6 <- read_csv(here("results/region6.csv"))

glimpse(r6)

  split(., list(.$quants, .$typic)) %>%
  map(~ select(., quants))

dataf %>%
  group_by(region) %>%
  summarise(n = n())

dataf %>%
  split(., list(dataf$subj_cond, dataf$obj_cond))

somef <- function(arg = list("a", "b")) {
  arg <- match.arg(arg)
  return(arg)

}

somef(c("a", "b"))


rem_zeros <- function(data_list, dv_name, remove_zeros){

  nms <- data_list %>%
    map(~ select(., region)) %>%
    map_dfr(unique) %>%
    pull()

  if (remove_zeros == TRUE) {
    map(data_list,
      ~ select(., region:item, quan_cond:last_col(), all_of(dv_name))
    ) %>%
      map(.x = ., ~ filter(., .data[[dv_name]] > 0)) %>%
      set_names(paste0("region_", nms))
  } else {
    map(data_list,
      ~ select(., region:item, quan_cond:last_col(), {{ dv_name }})
    ) %>% set_names(paste0("region_", nms))
  }
}

dfs <- list.files(here("results"), pattern = "region[0-9].csv") %>%
  map(~ read_csv(here("results", .)))

tgd <- map(dfs[6], ~ select(., tgdur))

tgd[[1]] %>%
  filter(tgdur == 0)

tgd2 <- rem_zeros(dfs[6], "tgdur", TRUE)

dd <- "tgdur"

tgd2 <- rem_zeros(dfs[6], dd, TRUE)

glimpse(tgd2)

tgd2[[1]] %>%
  filter(tgdur == 0)

get_model()

region <- 5
fit_model_func <- fit_full

th <- list.files(here("results"), pattern = "region[0-9].csv") %>%
  map(~ read_csv(here("results", .))) %>%
  .[region] %>%
    fit_model_func(., "totfixdur"
                   ) %>%
    pluck(paste0("region_", region))

names(th)
class(th)

mtot <- get_model("totfixdur", 6)
names(posterior_samples(mtot))

msummary(dfs[5:6])

mm <- fit_full(dfs[4:6],"totfixdur",.priors = priors)
msummary(mm)
names(mm)
ps_rename(mm$region_4)

posterior_summary(mm$region_4, pars = "b_") %>%
  relabel_summary()

t <- posterior_samples(mm$region_4, pars = "b_") %>%
  relabel()

posterior_summary

glimpse(t)





source(here("src/models.R"))


dfs <- list.files(here("results"), pattern = "region[0-9].csv") %>%
  map(~ read_csv(here("results", .)))


tgd <- fit_full(dfs[5], "tgdur", .priors = priors)
pp_check(tgd[[1]], type = "stat_2d", stat = c("median", "sd"))
gd <- fit_full(dfs[7], "gdur", .priors = priors) # 8th region
pp_check(gd[[1]], nsamples = 100)

## , type = "stat_2d", stat = c("median", "sd"))
library(ggplot2)
ggsave(here("figs/gdur-8-brms.png"))


## experimenting with plots
##

ttd <- readRDS(here("models/totfixdur_r1.rds"))
rrdur <- readRDS(here("models/rrdur_r1.rds"))
rr <- readRDS(here("models/rr_r1.rds"))

mcmc_intervals(ttd, regex_pars = "b_[^I]")

my_mcmc_inter <- function (data, pars = character(), regex_pars = character(), transformations = list(),
    ..., prob = 0.5, prob_outer = 0.9, point_est = c("median",
        "mean", "none"), rhat = numeric())
{
    bayesplot:::check_ignored_arguments(...)
    color_by_rhat <- rlang::has_name(data, "rhat_rating")
    no_point_est <- all(data$point_est == "none")
    x_lim <- range(c(data$ll, data$hh))
    x_range <- diff(x_lim)
    x_lim[1] <- x_lim[1] - 0.05 * x_range
    x_lim[2] <- x_lim[2] + 0.05 * x_range
    layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
        vline_0(color = "gray90", size = 0.5)
    }
    else {
        geom_ignore()
    }
    args_outer <- list(mapping = aes_(x = ~ll, xend = ~hh, y = ~parameter,
        yend = ~parameter), color = bayesplot:::get_color("mid"))
    args_inner <- list(mapping = aes_(x = ~l, xend = ~h, y = ~parameter,
        yend = ~parameter), size = 2, show.legend = FALSE)
    args_point <- list(mapping = aes_(x = ~m, y = ~parameter),
        data = data, size = 4, shape = 21)
    if (color_by_rhat) {
        args_inner$mapping <- args_inner$mapping %>% modify_aes_(color = ~rhat_rating)
        args_point$mapping <- args_point$mapping %>% modify_aes_(color = ~rhat_rating,
            fill = ~rhat_rating)
    }
    else {
        args_inner$color <- bayesplot:::get_color("dark")
        args_point$color <- bayesplot:::get_color("dark_highlight")
        args_point$fill <- bayesplot:::get_color("light")
    }
    point_func <- geom_point
    layer_outer <- do.call(geom_segment, args_outer)
    layer_inner <- do.call(geom_segment, args_inner)
    layer_point <- do.call(point_func, args_point)
    if (color_by_rhat) {
        scale_color <- scale_color_diagnostic("rhat")
        scale_fill <- scale_fill_diagnostic("rhat")
    }
    else {
        scale_color <- bayesplot:::geom_ignore()
        scale_fill <- bayesplot:::geom_ignore()
    }
    ggplot(data, aes(parameter, color = as.factor(var))) + layer_vertical_line + layer_outer + layer_inner +
        layer_point + scale_color + scale_fill + scale_y_discrete(limits = unique(rev(data$parameter))) +
        xlim(x_lim) + bayesplot_theme_get() + legend_move(ifelse(color_by_rhat,
        "top", "none")) + yaxis_text(face = "bold") + yaxis_title(FALSE) +
        yaxis_ticks(size = 1) + xaxis_title(FALSE)
}


ttd <- readRDS(here("models/totfixdur_r1.rds"))
rrdur <- readRDS(here("models/rrdur_r1.rds"))
rr <- readRDS(here("models/rr_r1.rds"))
dat_ttd <- mcmc_intervals_data(ttd, regex_pars = "b_[^I]", prob = 0.95, prob_outer = 1)
dat_rr <- mcmc_intervals_data(rrdur, regex_pars = "b_[^I]", prob = 0.95, prob_outer = 1)
dat_rrd <- mcmc_intervals_data(rr, regex_pars = "b_[^I]", prob = 0.95, prob_outer = 1)

dd <- bind_rows(ttd = dat_ttd, rr = dat_rr, rrdur=dat_rrd, .id = "var") %>%
  mutate(var = as.factor(var))

dod <- 0.5

ggplot(dd, aes(fill = var, color = var, group = var)) +
  geom_linerange(aes(xmin = ll, xmax = hh, y = parameter), position = position_dodge(dod)) +
  geom_linerange(aes(xmin = l, xmax = h, y = parameter), size = 1.5, show.legend = FALSE, position = position_dodge(dod)) +
  geom_point(aes(x = m, y = parameter), data = dd, size = 2, shape = 21, position = position_dodge(dod))
  ##
  ## geom_linerange(aes(x = ll, xend = hh, y = parameter,
  ##                             yend = parameter), position = position_dodge(2)) +


    ## args_inner <- list()
    ## args_point <- list()

## + layer_outer + layer_inner +
##     layer_point + scale_color + scale_fill + scale_y_discrete(limits = unique(rev(data$parameter)))

ttes <- readRDS(here("models/totfixdur_een_r6.rds"))
