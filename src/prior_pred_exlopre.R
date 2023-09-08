##
##
##

here::i_am("src/prior_pred_exlopre.R")

library(here)
library(dplyr)

## this needs sim_functions and simulations sourced


frm <- "~ quants * typic * interf +
 (quants * typic * interf | subj) + (quants * typic * interf | item)"
simd <- msimulate(
    1000, simdata, priors_binom, 8, 8, 8,
    "binomial", frm, intercept_above_zero = FALSE
  )



betas <- list(beta1 = c(1,2), beta2 = c(3,4))
sims <- list(sims1= c(34,56), sims2 = c(67, 78))
rho <- c(34,5)

sigma_u <- vector(mode = "list", length = nsigmas) %>%
    set_names(paste0("beta", seq(nsigmas)))

df <- as_tibble(c(betas, sims))
df["rho"] <- rho

library(rethinking)
data(chimpanzees)
d <- chimpanzees

m11.1 <- quap(alist(
  pulled_left ~ dbinom( 1 , p ) ,
  logit(p) <- a ,
  a ~ dnorm( 0 , 10 )
) , data=d )

set.seed(1999)

prior <- extract.prior( m11.1 , n=1e3 )
dens(inv_logit(prior$a))
dens(inv_logit(simd$true_params$beta1))

inv_logit(0.001)
