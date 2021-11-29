## SimFromPrior
## Daniel J. Schad, Dez. 13, 2018
## modified by j winkowski
## requires rethinking library

SimFromPrior <- function(priors, class = "b", coef = "") {
  priors_par <- priors$prior[priors$class == class & priors$coef == coef]
  priors_par <- strsplit(priors_par, "(", fixed = TRUE)[[1]]
  par <- strsplit(priors_par[2], ")")[[1]]
  par <- as.numeric(strsplit(par, ",")[[1]])
  priors_par <- priors_par[1]
  if (priors_par == "normal") {
    # message(paste0("rnorm(1,",par[1],",",par[2],")\n"))
    par_samp <- rnorm(1, par[1], par[2])
    if (class %in% c("sigma", "sd")) {
      while (par_samp <= 0) par_samp <- rnorm(1, par[1], par[2])
    }
  }
  if (priors_par == "cauchy") {
    par_samp <- rcauchy(1, par[1], par[2])
  }
  if (priors_par == "lkj") {
    # message(paste0("rethinking::rlkjcorr(1,2,",par[1],")\n"))
    par_samp <- rethinking::rlkjcorr(1, 2, par[1])[1, 2]
  }
  return(par_samp)
}
