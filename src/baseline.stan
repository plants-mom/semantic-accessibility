/*

Baseline, simple model.

*/

data {
  int<lower=1> N;                 //number of data points
  real rt[N];                     //reading time
  int<lower=1> J;                 //number of subjects
  int<lower=1> K;                 //number of items
  int<lower=1> subj[N];           //subject id
  int<lower=1, upper=K> item[N];  //item id
  // predictor values
  vector[N] quant;
  vector[N] typic;
  vector[N] interf;
}

parameters {
  real alpha;
  real beta_quant;               //means for quantifier
  real beta_typic;               //means for typicality
  real beta_interf;              //means for interference
  real beta_interf_quant;        //means for interference
  real beta_interf_typic;        //means for interference
  real beta_quant_typic;         //means for interference
  real beta_interf_quant_typic;  //means for interference
  vector[J] u;                   //subject intercepts
  vector[K] w;                   //item intercepts
  real<lower=0> sigma_e;         //error sd
  real<lower=0> sigma_u;         //subj sd
  real<lower=0> sigma_w;         //item sd
}

model {

  //priors

  target += normal_lpdf(alpha | 5, 5);
  target += normal_lpdf(beta_quant | 0, 1);
  target += normal_lpdf(beta_typic | 0, 1);
  target += normal_lpdf(beta_interf | 0, 1);
  target += normal_lpdf(beta_interf_quant | 0, 1);
  target += normal_lpdf(beta_interf_typic | 0, 1);
  target += normal_lpdf(beta_quant_typic | 0, 1);
  target += normal_lpdf(beta_interf_quant_typic | 0, 1);
  target += normal_lpdf(sigma_e | 0, 1) -
    normal_lccdf(0 | 0, 2);
  target += normal_lpdf(sigma_u | 0, 1) -
    normal_lccdf(0 | 0, 1);
  target += normal_lpdf(sigma_w | 0, 1) -
    normal_lccdf(0 | 0, 1);
  target += normal_lpdf(u | 0, sigma_u);          //subj random effects
  target += normal_lpdf(w | 0, sigma_w);          //subj random effects

  // likelihood
  for (i in 1:N){
    real mu = alpha +
      beta_typic * typic[i] +
      beta_interf * interf[i] +
      beta_quant * quant[i] +
      beta_interf_quant * quant[i] * interf[i] +
      beta_interf_typic * quant[i] * typic[i] +
      beta_quant_typic * quant[i] * typic[i] +
      beta_interf_quant_typic * quant[i] * typic[i] * interf[i]+
      u[subj[i]] + w[item[i]];

      target += lognormal_lpdf(rt[i] | mu, sigma_e);
  }
}

generated quantities{
  vector[N] yrep;

  for (i in 1:N){
    real pred_mu = alpha +
      beta_typic * typic[i] +
      beta_interf * interf[i] +
      beta_quant * quant[i] +
      beta_interf_quant * quant[i] * interf[i] +
      beta_interf_typic * quant[i] * typic[i] +
      beta_quant_typic * quant[i] * typic[i] +
      beta_interf_quant_typic * quant[i] * typic[i] * interf[i]+
      u[subj[i]] + w[item[i]];

      yrep[i] = lognormal_rng(pred_mu, sigma_e);

  }
}
