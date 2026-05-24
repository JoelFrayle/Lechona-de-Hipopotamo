// modelo_dm_rw_param.stan - autogenerado por 06_backtest_historico.R
functions {
  real dm_lpmf(array[] int y, vector alpha) {
    int K = num_elements(alpha);
    real A = sum(alpha);
    int n = sum(y);
    real lp = lgamma(n + 1) + lgamma(A) - lgamma(n + A);
    for (k in 1:K)
      lp += lgamma(y[k] + alpha[k]) - lgamma(alpha[k]) - lgamma(y[k] + 1);
    return lp;
  }
}
data {
  int<lower=1> J; int<lower=2> K; int<lower=1> T; int<lower=1> H;
  array[J, K] int<lower=0> Y;
  array[J] int<lower=1, upper=T> t_idx;
  array[J] int<lower=1, upper=H> h_idx;
  int<lower=0> S;
  vector<lower=0>[S > 0 ? S : 1] dt;
  vector<lower=0>[J] days_ago;
  vector<lower=1>[J] lambda_freq;
  vector<lower=1>[J] lambda_i;
  real<lower=0> time_decay_rate;
  real<lower=0> T_pred;
  // Hiperparámetros parametrizados:
  real sigma_rw_loc;
  real<lower=0> sigma_rw_scale;
  real<lower=0> sigma_house_rate;
  real kappa_loc;
  real<lower=0> kappa_scale;
}
parameters {
  vector[K-1] phi_init;
  matrix[S > 0 ? S : 1, K-1] eta;
  real<lower=0> sigma_rw;
  array[H] vector[K-1] beta_house_raw;
  real<lower=0> sigma_house;
  vector<lower=0>[H] kappa_h;
}
transformed parameters {
  array[T] vector[K-1] phi;
  phi[1] = phi_init * 1.5;
  if (S > 0) {
    for (t in 2:T)
      phi[t] = phi[t-1] + sigma_rw * sqrt(dt[t-1]) * to_vector(eta[t-1]);
  }
  array[H] vector[K-1] beta_house;
  for (h in 1:H)
    beta_house[h] = sigma_house * beta_house_raw[h];
}
model {
  phi_init ~ normal(0, 1);
  to_vector(eta) ~ normal(0, 1);
  sigma_rw ~ lognormal(sigma_rw_loc, sigma_rw_scale);
  sigma_house ~ exponential(sigma_house_rate);
  kappa_h ~ lognormal(kappa_loc, kappa_scale);
  for (h in 1:H) beta_house_raw[h] ~ normal(0, 1);
  for (j in 1:J) {
    vector[K] log_p_j;
    log_p_j[1:(K-1)] = phi[t_idx[j]] + beta_house[h_idx[j]];
    log_p_j[K] = 0.0;
    log_p_j = log_softmax(log_p_j);
    vector[K] alpha_j = kappa_h[h_idx[j]] * exp(log_p_j);
    real w_j = exp(-time_decay_rate * days_ago[j]) /
               (lambda_freq[j] * lambda_i[j]);
    target += w_j * dm_lpmf(Y[j] | alpha_j);
  }
}
generated quantities {
  vector[K] p_election;
  vector[K] log_p_pred;
  for (k in 1:(K-1)) {
    real eta_pred = normal_rng(0, 1);
    log_p_pred[k] = phi[T][k] + sigma_rw * sqrt(T_pred) * eta_pred;
  }
  log_p_pred[K] = 0.0;
  p_election = softmax(log_p_pred);
}
