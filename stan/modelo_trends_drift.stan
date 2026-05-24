// =====================================================================
// modelo_trends_drift.stan
// =====================================================================
// State-space model con momentum AR(1) multi-escala para Google Trends.
//
// Estado latente:
//   phi[t,c] = phi[t-1,c] + gamma_fast[t-1,c] + gamma_slow[t-1,c] + eps_rw
//
// Drifts AR(1) con rho pre-fijado por vida media:
//   gamma_fast[t,c] = rho_fast * gamma_fast[t-1,c] + eps_fast   (vida ~2 días)
//   gamma_slow[t,c] = rho_slow * gamma_slow[t-1,c] + eps_slow   (vida ~7 días)
//
// Observación:
//   y[n] = phi[dia[n], cand[n]] + nu     (y = log(hits + 1))
//
// PRIORS REGULARIZADOS (clave para evitar explosión de varianza):
//   sigma_fast, sigma_slow ~ double_exponential(0, scale)   ← Laplace
//   sigma_rw, sigma_obs    ~ normal+(0, scale)              ← half-normal
//
// El Laplace en sigma_fast/sigma_slow es shrinkage AGRESIVO: el momentum
// colapsa a 0 a menos que la evidencia sea fuerte. Esto controla la
// explosión de varianza al proyectar al día E.
// =====================================================================

data {
  int<lower=1> T;            // número de días observados
  int<lower=1> C;            // número de candidatos
  int<lower=1> N_obs;        // observaciones totales de Trends
  array[N_obs] int<lower=1, upper=T> dia;
  array[N_obs] int<lower=1, upper=C> cand;
  vector[N_obs] y;           // log(hits + 1)

  int<lower=1> T_extrap;     // días a extrapolar hacia el futuro

  // Hiperparámetros pre-fijados
  real<lower=0, upper=1> rho_fast;   // ~0.7071 (vida 2 días)
  real<lower=0, upper=1> rho_slow;   // ~0.9057 (vida 7 días)

  // Escalas de los priors (optimizadas por LFO-CV externo)
  real<lower=0> sigma_fast_scale;
  real<lower=0> sigma_slow_scale;
  real<lower=0> sigma_rw_scale;
  real<lower=0> sigma_obs_scale;
}

parameters {
  // Parametrización NO-CENTRADA para evitar funnel y mejorar mixing
  matrix[T, C] z_phi;
  matrix[T, C] z_gamma_fast;
  matrix[T, C] z_gamma_slow;
  vector[C] phi_init;

  // Varianzas (compartidas across candidatos y tiempo — partial pooling)
  real<lower=0> sigma_fast;
  real<lower=0> sigma_slow;
  real<lower=0> sigma_rw;
  real<lower=0> sigma_obs;
}

transformed parameters {
  matrix[T, C] phi;
  matrix[T, C] gamma_fast;
  matrix[T, C] gamma_slow;

  // Inicialización t=1
  for (c in 1:C) {
    gamma_fast[1, c] = sigma_fast * z_gamma_fast[1, c];
    gamma_slow[1, c] = sigma_slow * z_gamma_slow[1, c];
    phi[1, c]        = phi_init[c];
  }

  // Recursión t = 2..T (loop por tiempo, vectorizado en candidatos)
  for (t in 2:T) {
    gamma_fast[t] = rho_fast * gamma_fast[t-1] + sigma_fast * z_gamma_fast[t];
    gamma_slow[t] = rho_slow * gamma_slow[t-1] + sigma_slow * z_gamma_slow[t];
    phi[t]        = phi[t-1] + gamma_fast[t-1] + gamma_slow[t-1]
                    + sigma_rw * z_phi[t];
  }
}

model {
  // PRIORS REGULARIZADOS — el corazón del shrinkage
  //   Laplace (double_exponential) para los DRIFTS: colapsan a 0 sin evidencia
  sigma_fast ~ double_exponential(0, sigma_fast_scale);
  sigma_slow ~ double_exponential(0, sigma_slow_scale);
  //   Half-normal para nivel y observación (menos restrictivo)
  sigma_rw   ~ normal(0, sigma_rw_scale);
  sigma_obs  ~ normal(0, sigma_obs_scale);

  // Estado inicial (log(hits+1) típicamente entre 0 y 5)
  phi_init ~ normal(0, 3);

  // Z-scores estándares normales
  to_vector(z_phi)        ~ std_normal();
  to_vector(z_gamma_fast) ~ std_normal();
  to_vector(z_gamma_slow) ~ std_normal();

  // Likelihood (vectorizado)
  vector[N_obs] mu_obs;
  for (n in 1:N_obs) {
    mu_obs[n] = phi[dia[n], cand[n]];
  }
  y ~ normal(mu_obs, sigma_obs);
}

generated quantities {
  // Forecast a T_extrap pasos
  matrix[T_extrap, C] phi_proj;
  vector[C] phi_final;
  vector[C] share_pred;

  {
    vector[C] gf = gamma_fast[T]';
    vector[C] gs = gamma_slow[T]';
    vector[C] ph = phi[T]';

    for (t in 1:T_extrap) {
      for (c in 1:C) {
        gf[c] = rho_fast * gf[c] + sigma_fast * normal_rng(0, 1);
        gs[c] = rho_slow * gs[c] + sigma_slow * normal_rng(0, 1);
        ph[c] = ph[c] + gf[c] + gs[c] + sigma_rw * normal_rng(0, 1);
      }
      phi_proj[t] = ph';
    }
    phi_final = ph;
  }

  // Share predicho = softmax(phi_final)
  // Esto convierte el estado latente (en escala log-hits) a proporciones
  // que suman 1, comparables con el share del voto entre candidatos.
  {
    vector[C] expphi = exp(phi_final);
    share_pred = expphi / sum(expphi);
  }
}
