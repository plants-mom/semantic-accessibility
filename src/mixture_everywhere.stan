/*

Model which assumes mixture distribution over all the conditions.

*/

data {
  int<lower=1> N;                 //number of data points
  real rt[N];                     //reading time
  int<lower=1> N_subj;                 //number of subjects
  int<lower=1> N_item;                 //number of items
  int<lower=1, upper=N_subj> subj[N];           //subject id
  int<lower=1, upper=N_item> item[N];  //item id
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
  real<lower=0> sigma_e;         //error sd
  real<lower=0> sigma_e_shift;   //error sd for the shifted component
  real<lower=0,upper=1> prob;    //probability of extreme values
  real<lower=0> delta;

  vector<lower = 0>[2]  tau_u;
  vector<lower = 0>[2]  tau_w;
  matrix[2, N_subj] z_u;
  matrix[2, N_item] z_w;
  cholesky_factor_corr[2] L_u;
  cholesky_factor_corr[2] L_w;
}

transformed parameters {
  matrix[N_subj, 2] u;
  matrix[N_item, 2] w;
  u = (diag_pre_multiply(tau_u, L_u) * z_u)';
  w = (diag_pre_multiply(tau_w, L_w) * z_w)';
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
  target += beta_lpdf(prob | 10, 2);
  target += normal_lpdf(delta | 2, 1);
  target += normal_lpdf(sigma_e | 0, 1) -
    normal_lccdf(0 | 0, 2);
  target += normal_lpdf(sigma_e_shift | 0, 1) -
    normal_lccdf(0 | 0, 2);
  target += normal_lpdf(tau_u | 0, 20) -
    2 * normal_lccdf(0 | 0, 20);
 target += normal_lpdf(tau_w | 0, 20) -
    2* normal_lccdf(0 | 0, 20);
  target += lkj_corr_cholesky_lpdf(L_u | 2);
  target += lkj_corr_cholesky_lpdf(L_w | 2);
  target += std_normal_lpdf(to_vector(z_u));
  target += std_normal_lpdf(to_vector(z_w));

  // likelihood
  for (i in 1:N){
    real mu = alpha + u[subj[i], 1] + w[item[i], 1] +
      typic[i] * (beta_typic + u[subj[i], 2]) +
      interf[i] * (beta_interf + u[subj[i], 2]) +
      quant[i] * (beta_quant + u[subj[i], 2])  +
      quant[i] * interf[i] * (beta_interf_quant + u[subj[i], 2]) +
      quant[i] * typic[i] * (beta_interf_typic + u[subj[i], 2]) +
      quant[i] * typic[i] * (beta_quant_typic + u[subj[i], 2]) +
      quant[i] * typic[i] * interf[i] * (beta_interf_quant_typic + u[subj[i], 2]);

      target += log_sum_exp(log(prob) +
                            lognormal_lpdf(rt[i] | mu + delta, sigma_e_shift),
                            log1m(prob) +
                            lognormal_lpdf(rt[i] | mu, sigma_e));
  }
}

generated quantities{
  vector[N] yrep;
  real<lower=0,upper=1> test;  //probability of mixtures

  for (i in 1:N){
    real mu = alpha + u[subj[i], 1] + w[item[i], 1] +
      typic[i] * (beta_typic + u[subj[i], 2]) +
      interf[i] * (beta_interf + u[subj[i], 2]) +
      quant[i] * (beta_quant + u[subj[i], 2])  +
      quant[i] * interf[i] * (beta_interf_quant + u[subj[i], 2]) +
      quant[i] * typic[i] * (beta_interf_typic + u[subj[i], 2]) +
      quant[i] * typic[i] * (beta_quant_typic + u[subj[i], 2]) +
      quant[i] * typic[i] * interf[i] * (beta_interf_quant_typic + u[subj[i], 2]);

    test = uniform_rng(0, 1); // draw from uniform(0, 1)

    // draw below prob: sample from the moved model
    if (test < prob) {
      yrep[i] = lognormal_rng(pred_mu + delta, sigma_e_shift);

    // draw above prob: sample from the simple model
    } else yrep[i] = lognormal_rng(pred_mu, sigma_e);
  }
}
