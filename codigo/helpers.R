# =========================================================
# HELPERS.R - Funciones reutilizables del modelo electoral 2026
# =========================================================
# Carga: source("helpers.R") DESPUÉS de cargar config.R
# Requiere: dplyr, tidyr, purrr, tibble, lubridate
# =========================================================

# ---------------------------------------------------------
# Conversión segura
# ---------------------------------------------------------
safe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# ---------------------------------------------------------
# Parser robusto de fechas (acepta YMD, DMY, MDY)
# ---------------------------------------------------------
parse_date_multi <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  d <- suppressWarnings(lubridate::ymd(x, quiet = TRUE))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::dmy(x, quiet = TRUE))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x, quiet = TRUE))
  as.Date(d)
}

# ---------------------------------------------------------
# Matriz de covarianza compound-symmetric (para correlacionar
# house effects entre casas con parámetro rho)
# ---------------------------------------------------------
make_Sigma_cs <- function(var_diag_vec, rho) {
  K <- length(var_diag_vec)
  if (K == 0) return(matrix(0, 0, 0))
  if (K == 1) return(matrix(var_diag_vec[1], 1, 1))
  sds    <- sqrt(pmax(var_diag_vec, 0))
  D      <- diag(sds, K)
  R_corr <- (1 - rho) * diag(K) + rho * matrix(1, K, K)
  D %*% R_corr %*% D
}

# ---------------------------------------------------------
# Concentración de mercado (Hill-1 / share kernel-ponderado)
# ---------------------------------------------------------
# Para cada poll i, calcula el share kernel-ponderado del pollster
# que la publicó vs un share "justo" basado en Hill-1.
#
# Salidas:
#   share_actual: peso del pollster en t_i
#   share_fair:   1/Hill1 (peso justo si todos balanceados)
#   H1:           número efectivo de pollsters activos
# ---------------------------------------------------------
compute_market_share <- function(t_i, pollster_i, all_polls_df, tau) {
  delta <- abs(as.numeric(all_polls_df$t_poll - t_i))
  w     <- exp(-delta / tau)
  total <- sum(w)

  if (total <= 0) return(list(share_actual = 1, share_fair = 1, H1 = 1))

  agg           <- tapply(w, all_polls_df$pollster, sum)
  agg           <- ifelse(is.na(agg), 0, agg)
  shares        <- as.numeric(agg) / total
  names(shares) <- names(agg)

  shares_pos <- shares[shares > 0]
  H1 <- exp(-sum(shares_pos * log(shares_pos)))

  share_actual <- as.numeric(shares[pollster_i])
  share_fair   <- 1 / H1

  list(share_actual = share_actual, share_fair = share_fair, H1 = H1)
}

# ---------------------------------------------------------
# Recycler de paletas
# ---------------------------------------------------------
recycle <- function(v, n) v[((seq_len(n) - 1) %% length(v)) + 1]

# =========================================================
# FILTRO DE KALMAN v7
# =========================================================
# Modelo: estado latente mu_t (logit) + house effects correlacionados
# Estima por max-likelihood: Q (drift), tau (varianza inicial casa),
# Q_house (drift del house effect), rho_house (herding).
#
# Argumentos:
#   df: data frame con columnas z, R, t, n, pollster, candidato
#   m_min, n_pseudo, P_inicial_logit, var_last_cap, min_obs_full_var
#   Q_bounds, tau_bounds, Q_house_bounds, R_MAX
#   rho_init, rho_prior_mean, rho_prior_sd
#   track_trayectoria: si TRUE, devuelve trayectoria filtrada (default FALSE)
#
# Salida: tibble con mu_hat, lo, hi, hyperparams, estado_final, cov_final
# =========================================================
kalman_house_rw_fit <- function(df,
                                m_min = 1, n_pseudo = 2,
                                Q_bounds = c(1e-8, 1),
                                tau_bounds = c(1e-6, 2),
                                Q_house_bounds = c(1e-8, 1e-2),
                                R_MAX = 1e3,
                                P_inicial_logit = 1.5,
                                var_last_cap = 0.5,
                                min_obs_full_var = 3,
                                rho_init = 0.3,
                                rho_prior_mean = 0.3,
                                rho_prior_sd = 0.3,
                                track_trayectoria = FALSE,
                                # ── Devaluación inteligente (Kalman robusto) ──
                                # Si la innovación normalizada v²/S supera el umbral
                                # chi2 de 1 grado de libertad, se infla el R de esa
                                # observación proporcionalmente. Intuición: encuestas
                                # que son outliers respecto al random walk se creen
                                # menos. El polo de atracción (prior de outlier):
                                #   R_eff = R × max(1, (v²/S) / chi2_threshold)
                                # chi2_threshold = 3.84 ≡ p=0.05 del chi²(1)
                                # Desactivar con innovation_decay = FALSE o
                                # chi2_threshold = Inf (default conservador).
                                innovation_decay    = FALSE,
                                chi2_threshold      = 3.84,
                                # ── Devaluación temporal ──────────────────────
                                # Infla R de observaciones antiguas por
                                # exp(time_decay_rate * dias_atras).
                                # dias_atras = días antes de la encuesta más
                                # reciente. La encuesta más nueva: factor=1.
                                # Con rate=0.012 (semivida 58 días):
                                #   60 días antes  → R × 2.05  (~½ peso)
                                #   120 días antes → R × 4.22  (~¼ peso)
                                # 0.0 = sin devaluación (default conservador).
                                time_decay_rate     = 0.0) {

  cand_name_safe <- if (length(unique(df$candidato)) > 0) df$candidato[1] else NA_character_
  df <- df %>% dplyr::filter(is.finite(z), is.finite(R), !is.na(t),
                             is.finite(n), n > 0) %>% dplyr::arrange(t)

  empty_tray <- tibble::tibble(candidato = character(0), t = as.Date(character(0)),
                               mu_logit = numeric(0), var_mu = numeric(0))

  if (nrow(df) == 0) {
    return(tibble::tibble(
      candidato = cand_name_safe, mu_hat = NA_real_,
      lo = NA_real_, hi = NA_real_,
      Q_hat = NA_real_, tau_hat = NA_real_, Q_house_hat = NA_real_,
      rho_house_hat = NA_real_, n_obs = 0L,
      estado_final = list(NA_real_),
      cov_final    = list(matrix(NA_real_, 1, 1)),
      t_last       = as.Date(NA),
      houses_track = list(character(0)),
      house_effects = list(numeric(0)),
      trayectoria   = list(empty_tray)
    ))
  }

  cand_name <- df$candidato[1]
  n_obs     <- nrow(df)
  tvec <- df$t
  dt   <- c(0, as.numeric(diff(tvec)))
  z    <- df$z
  R    <- df$R

  z_init <- median(z[seq_len(min(3, length(z)))])

  # ── Devaluación temporal: R inflado según antigüedad ─────────────────
  # Ref = última encuesta. Encuestas viejas tienen R mayor (menos peso).
  # Aplicado TANTO al MLE (nll_full) COMO al forward pass.
  # Esto hace que el Q_hat estimado sea más alto cuando hay caídas recientes,
  # permitiendo que el RW acepte cambios estructurales sin pelearlos.
  if (time_decay_rate > 0 && n_obs > 1) {
    t_ref_decay <- max(tvec)
    days_ago    <- as.numeric(t_ref_decay - tvec)
    R_decay     <- R * exp(time_decay_rate * days_ago)
  } else {
    R_decay <- R
  }

  counts          <- table(df$pollster)
  houses_inc      <- names(counts[counts >= m_min])
  K               <- length(houses_inc)
  count_per_house <- if (K > 0) as.numeric(counts[houses_inc]) else numeric(0)
  shrink_factor   <- if (K > 0) count_per_house / (count_per_house + n_pseudo) else numeric(0)

  make_H <- function(pol) {
    h <- rep(0, 1 + K); h[1] <- 1
    idx <- match(pol, houses_inc)
    if (!is.na(idx)) h[1 + idx] <- 1
    matrix(h, nrow = 1)
  }

  nll_full <- function(log_params) {
    Q       <- exp(log_params[1])
    tau     <- exp(log_params[2])
    Q_house <- exp(log_params[3])
    rho_h   <- plogis(log_params[4])

    d <- 1 + K
    x <- rep(0, d); x[1] <- z_init
    P <- diag(d); P[1, 1] <- P_inicial_logit
    if (K > 0) {
      house_var_diag <- tau^2 * shrink_factor
      P[2:d, 2:d] <- make_Sigma_cs(house_var_diag, rho_h)
    }

    penalty <- 0.5 * (tau / 0.5)^2 +
               0.5 * (Q_house / 0.01)^2 +
               0.5 * ((rho_h - rho_prior_mean) / rho_prior_sd)^2
    ll <- penalty

    for (i in seq_along(z)) {
      Qproc <- matrix(0, d, d)
      Qproc[1, 1] <- Q * dt[i]
      if (K > 0) Qproc[2:d, 2:d] <- make_Sigma_cs(rep(Q_house * dt[i], K), rho_h)
      P_pred <- P + Qproc
      H <- make_H(df$pollster[i])
      S <- as.numeric(H %*% P_pred %*% t(H) + R_decay[i])
      v <- z[i] - as.numeric(H %*% x)
      ll <- ll + 0.5 * (log(2 * pi * S) + (v^2) / S)
      Kk <- (P_pred %*% t(H)) / S
      x  <- x + as.numeric(Kk) * v
      P  <- P_pred - (Kk %*% t(Kk)) * S
    }
    ll
  }

  init_par <- c(log(1e-4), log(0.1), log(1e-5), qlogis(rho_init))
  opt <- tryCatch(
    optim(init_par, nll_full, method = "L-BFGS-B",
          lower = c(log(Q_bounds[1]), log(tau_bounds[1]),
                    log(Q_house_bounds[1]), -5),
          upper = c(log(Q_bounds[2]), log(tau_bounds[2]),
                    log(Q_house_bounds[2]),  5)),
    error = function(e) NULL
  )

  if (is.null(opt)) {
    Q_hat <- 1e-4; tau_hat <- 0.1; Q_house_hat <- 1e-5; rho_house_hat <- rho_init
  } else {
    Q_hat         <- exp(opt$par[1])
    tau_hat       <- exp(opt$par[2])
    Q_house_hat   <- exp(opt$par[3])
    rho_house_hat <- plogis(opt$par[4])
  }

  d <- 1 + K
  x <- rep(0, d); x[1] <- z_init
  P <- diag(d); P[1, 1] <- P_inicial_logit
  if (K > 0) P[2:d, 2:d] <- make_Sigma_cs(tau_hat^2 * shrink_factor, rho_house_hat)

  filas_tray <- if (track_trayectoria) vector("list", length(z)) else NULL

  for (i in seq_along(z)) {
    Qproc <- matrix(0, d, d)
    Qproc[1, 1] <- Q_hat * dt[i]
    if (K > 0) Qproc[2:d, 2:d] <- make_Sigma_cs(rep(Q_house_hat * dt[i], K), rho_house_hat)
    P_pred <- P + Qproc
    H <- make_H(df$pollster[i])
    # R efectivo: ya incluye devaluación temporal (R_decay)
    # La devaluación por innovación puede inflarlo más si es outlier
    S <- as.numeric(H %*% P_pred %*% t(H) + R_decay[i])
    v <- z[i] - as.numeric(H %*% x)

    # ── Devaluación inteligente: inflar R si la innovación es un outlier ──
    # v²/S sigue χ²(1) si el modelo es correcto.
    # Si v²/S >> chi2_threshold, el poll contradice el RW → inflamos R.
    # Esto reduce el gain K y cree menos ese dato atípico.
    # Factor de inflación = max(1, (v²/S) / chi2_threshold)
    # → polls consistentes con el RW: factor ≈ 1 (sin cambio)
    # → polls que fueron outliers:    factor > 1 (creídos menos)
    R_eff <- R_decay[i]
    if (isTRUE(innovation_decay) && is.finite(chi2_threshold) &&
        chi2_threshold > 0 && S > 0) {
      innov_ratio <- (v^2 / S) / chi2_threshold
      if (innov_ratio > 1) {
        R_eff <- R[i] * innov_ratio          # inflar
        S     <- as.numeric(H %*% P_pred %*% t(H) + R_eff)  # recalcular S
      }
    }

    Kk <- (P_pred %*% t(H)) / S
    x  <- x + as.numeric(Kk) * v
    P  <- P_pred - (Kk %*% t(Kk)) * S
    if (track_trayectoria) {
      filas_tray[[i]] <- tibble::tibble(candidato = cand_name, t = tvec[i],
                                        mu_logit = x[1], var_mu = P[1, 1])
    }
  }

  # Cap varianza final:
  # (1) Pocas obs: var_last_cap
  # (2) Near-zero (<2%): IC superior <= 5x mu_hat
  mu_hat_est <- plogis(x[1])
  if (mu_hat_est < 0.02 && n_obs >= min_obs_full_var) {
    max_upper_nz <- min(mu_hat_est * 5.0, 0.10)
    if (max_upper_nz > mu_hat_est) {
      var_cap_nz <- ((qlogis(max_upper_nz) - x[1]) / 1.96)^2
      P[1, 1] <- min(P[1, 1], var_cap_nz)
    }
  }
  if (n_obs < min_obs_full_var) P[1, 1] <- min(P[1, 1], var_last_cap)

  he_vec <- if (K > 0) x[2:d] else numeric(0)
  if (K > 0) names(he_vec) <- houses_inc
  trayectoria <- if (track_trayectoria) dplyr::bind_rows(filas_tray) else empty_tray

  media_empirica_pp <- mean(df$y_pct, na.rm = TRUE)
  media_reciente_pp <- {
    df_rec <- df[df$t >= (max(df$t) - 60), ]
    if (nrow(df_rec) == 0) media_empirica_pp else mean(df_rec$y_pct, na.rm = TRUE)
  }

  tibble::tibble(
    candidato = cand_name, mu_hat = plogis(x[1]),
    lo = plogis(x[1] - 1.96 * sqrt(P[1, 1])),
    hi = plogis(x[1] + 1.96 * sqrt(P[1, 1])),
    media_empirica_pp = media_empirica_pp,
    media_reciente_pp = media_reciente_pp,
    Q_hat = Q_hat, tau_hat = tau_hat, Q_house_hat = Q_house_hat,
    rho_house_hat = rho_house_hat, n_obs = n_obs,
    estado_final = list(x), cov_final = list(P), t_last = max(tvec),
    houses_track = list(houses_inc),
    house_effects = list(he_vec),
    trayectoria   = list(trayectoria)
  )
}

# ---------------------------------------------------------
# Construye la tabla de errores combinados de cada pollster
# usando los pesos del config y las tablas históricas.
# ---------------------------------------------------------
build_errores_combinados <- function(errores_hist, errores_cons,
                                     w_h, w_c, w_shrink, sigma_reg) {
  raw <- dplyr::full_join(errores_hist, errores_cons, by = "pollster")
  raw <- raw %>%
    dplyr::mutate(
      error_historico = ifelse(is.na(error_historico),
                               mean(error_historico, na.rm = TRUE),
                               error_historico),
      error_consulta  = ifelse(is.na(error_consulta),
                               mean(error_consulta,  na.rm = TRUE),
                               error_consulta)
    )

  market_baseline <- with(raw, mean(w_h * error_historico + w_c * error_consulta))

  raw %>%
    dplyr::mutate(
      error_pre_shrink = w_h * error_historico + w_c * error_consulta,
      error_shrunk     = (1 - w_shrink) * error_pre_shrink + w_shrink * market_baseline,
      error_combinado  = sqrt(error_shrunk^2 + sigma_reg^2)
    )
}

cat("[helpers.R] Funciones cargadas.\n")

# =========================================================
# compute_var_ou_total()
# =========================================================
# Varianza OU desde t_last hasta t_objetivo, con bump de volatilidad
# en los últimos hot_zone_days antes de fecha_eleccion.
# Salida: varianza total (escalar)
# =========================================================
compute_var_ou_total <- function(var_last, Q_hat, kappa_OU,
                                  t_last, t_objetivo, fecha_eleccion,
                                  hot_zone_days, hot_zone_mult,
                                  ruido_sistemico = 0) {
  d_total <- pmax(0, as.numeric(t_objetivo - t_last))
  if (d_total == 0) return(var_last + ruido_sistemico)

  t_hot_start <- fecha_eleccion - hot_zone_days

  d_normal   <- pmin(pmax(0, as.numeric(pmin(t_objetivo, t_hot_start) - t_last)),
                     d_total)
  d_caliente <- pmin(pmax(0, d_total - d_normal),
                     as.numeric(hot_zone_days))

  var_OU_normal   <- (Q_hat / (2 * kappa_OU)) *
                      (1 - exp(-2 * kappa_OU * d_normal))
  var_OU_caliente <- (Q_hat * hot_zone_mult / (2 * kappa_OU)) *
                      (1 - exp(-2 * kappa_OU * d_caliente))

  var_last + var_OU_normal + var_OU_caliente + ruido_sistemico
}

# =========================================================
# compute_pendiente_credible()
# =========================================================
# Pendiente WLS de la trayectoria Kalman, shrinkada por SNR vs RW.
# Con encuestas espaciadas el SNR tiende a ser bajo (shrink ~ 0).
# Solo se extrapola si la pendiente es sistematicamente mayor al
# drift esperado del random walk en ese mismo span de tiempo.
# Salida: pendiente shrinkada en logit/dia
# =========================================================
compute_pendiente_credible <- function(trayectoria, Q_hat,
                                        n_trend_obs = 5L,
                                        umbral_SNR  = 1.5) {
  if (is.null(trayectoria) || nrow(trayectoria) < 2) return(0)

  tray  <- tail(trayectoria, max(2L, as.integer(n_trend_obs)))
  t_num <- as.numeric(tray$t - tray$t[1])
  y     <- tray$mu_logit
  w     <- 1 / pmax(tray$var_mu, 1e-10)

  t_total <- max(t_num)
  if (t_total < 1) return(0)

  wsum   <- sum(w)
  t_bar  <- sum(w * t_num) / wsum
  y_bar  <- sum(w * y)     / wsum
  var_t  <- sum(w * (t_num - t_bar)^2) / wsum
  cov_ty <- sum(w * (t_num - t_bar) * (y - y_bar)) / wsum

  if (var_t < 1e-10) return(0)
  pendiente <- cov_ty / var_t

  sigma_pendiente_rw <- sqrt(Q_hat / t_total)
  if (sigma_pendiente_rw <= 0) return(0)

  SNR    <- abs(pendiente) / sigma_pendiente_rw
  shrink <- pmax(0, 1 - umbral_SNR / SNR)

  pendiente * shrink
}
