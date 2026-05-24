# ======================================================================
# Lechona de Hipopotamo — Joel Frayle Moreno
# Concurso Recetas Electorales 2026, Primera Vuelta
# Contacto: joel.frayle13@gmail.com
#
# Modelo MCMC bayesiano Dirichlet-Multinomial con caminata aleatoria.
# Integración de señal Google Trends via momentum direccional (Modo C).
# Factor de integración calibrado via backtest 2018+2022 (MSE -51.6%).
# Bloques ideológicos: el momentum solo redistribuye entre candidatos
# del mismo espacio político (validado empíricamente con drift histórico).
# ======================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble)
  library(readr); library(cmdstanr); library(posterior)
})
source("config.R")
source("helpers.R")

OUTPUT_DIR <- "outputs"
dir.create(OUTPUT_DIR, showWarnings = FALSE)
t0 <- Sys.time()
cat("=== Lechona de Hipopotamo — Iniciando ===\n")

# -----------------------------------------------------------------------
# PARÁMETROS
# -----------------------------------------------------------------------
FACTOR_C     <- 1.2    # calibrado via backtest 2018+2022
VENTANA_DIAS <- 14     # 7d tiene señal demasiado débil con hits bajos
EXTRAPOLATION_SCALE <- 0.20  # fuera del IC80: solo 20% del exceso pasa

MCMC_CHAINS   <- 4
MCMC_WARMUP   <- 1000
MCMC_SAMPLING <- 1500
MCMC_ADAPT_DELTA <- 0.95

MCMC_HYPERPARAMS <- list(
  sigma_rw_loc     = -3.00, sigma_rw_scale   = 0.40,
  sigma_house_rate = 10.67,
  kappa_loc        = 5.37,  kappa_scale      = 0.67,
  time_decay_rate  = 0.0172
)

# Candidatos del concurso
CANDS_CONCURSO <- c("Cepeda", "DeLaEspriella", "Valencia", "Fajardo", "ClaudiaLopez")

# Bloques ideológicos: momentum redistribuye SOLO dentro de cada bloque.
# Base empírica: en 2018 y 2022 el líder se movió ±2pp; los challengers
# despegaron tomando votos del vecino ideológico, NUNCA del extremo opuesto.
BLOQUES_2026 <- list(
  IZQUIERDA = c("Cepeda"),                      # base dura inamovible
  CENTRO    = c("Fajardo", "ClaudiaLopez"),
  DERECHA   = c("Valencia", "DeLaEspriella")    # Paloma puede ceder a Abelardo
  # Residuales (BlancoNulo, Otro_agr) no reciben ajuste Trends
)

# -----------------------------------------------------------------------
# PASO 1: Preparar encuestas 2026
# -----------------------------------------------------------------------
CSV_2026 <- "encuestas_presidenciales_2026.csv"
if (!file.exists(CSV_2026)) stop("No se encontró ", CSV_2026)

datos <- read.csv(CSV_2026, header = FALSE, skip = 1, stringsAsFactors = FALSE)
colnames(datos) <- trimws(as.character(
  read.csv(CSV_2026, header = FALSE, nrows = 1, stringsAsFactors = FALSE)[1, ]
))

meta_cols <- intersect(c("poll_id","pollster","mode","field_start",
                          "field_end","n_total","moe_95_pp"), names(datos))
cand_cols <- setdiff(names(datos), meta_cols)
cand_cols <- cand_cols[nchar(cand_cols) > 0 & !grepl("^V[0-9]+$", cand_cols)]

datos$poll_id <- safe_num(datos$poll_id)
datos <- datos %>%
  filter(!is.na(poll_id)) %>%
  mutate(pollster = trimws(as.character(pollster)),
         across(all_of(cand_cols), ~ replace_na(safe_num(.x), 0)))

# Errores históricos de casas encuestadoras
ec_path <- file.path(OUTPUT_DIR, "error_consulta.rds")
errores_consulta_curr <- if (file.exists(ec_path)) readRDS(ec_path) else ERRORES_CONSULTA_FALLBACK
errores_historicos <- build_errores_combinados(
  errores_hist = ERRORES_HISTORICOS_PRE, errores_cons = errores_consulta_curr,
  w_h = w_hist, w_c = w_consulta,
  w_shrink = w_market_shrink, sigma_reg = sigma_regime_change_pp
)
err_min <- min(errores_historicos$error_combinado, na.rm = TRUE)
err_max <- max(errores_historicos$error_combinado, na.rm = TRUE)

datos <- datos %>%
  left_join(errores_historicos %>% select(pollster, error_combinado),
            by = "pollster") %>%
  mutate(
    error_combinado = coalesce(error_combinado, err_max),
    lambda_i        = pmax(1.0, (error_combinado / err_min)^1.2)
  )

polls_long <- datos %>%
  select(all_of(meta_cols), lambda_i, all_of(cand_cols)) %>%
  pivot_longer(cols = all_of(cand_cols), names_to = "candidato", values_to = "y_pct") %>%
  filter(!is.na(y_pct), y_pct >= 0) %>%
  mutate(
    n       = safe_num(n_total),
    end     = parse_date_multi(field_end),
    se_moe  = (safe_num(moe_95_pp) / 100) / 1.96,
    se_max_srs = sqrt(0.25 / pmax(n, 1)),
    deff_star = if_else(is.finite(se_moe) & se_max_srs > 0,
                        (se_moe / se_max_srs)^2, NA_real_),
    deff_default = case_when(
      grepl("online|web|internet|panel", mode, ignore.case = TRUE) ~ 2.5,
      grepl("telef|phone",              mode, ignore.case = TRUE) ~ 1.5,
      TRUE ~ 1.2),
    DEFF  = case_when(!is.na(deff_star) & deff_star >= 1 & deff_star <= 20 ~ deff_star,
                      TRUE ~ deff_default),
    n_eff = pmax(n / DEFF, 1)
  ) %>%
  filter(!is.na(end), !is.na(n), n > 0)

if (exists("CANDIDATOS_RETIRADOS") && length(CANDIDATOS_RETIRADOS) > 0) {
  for (cand_ in names(CANDIDATOS_RETIRADOS)) {
    fr_ <- CANDIDATOS_RETIRADOS[[cand_]]
    polls_long <- polls_long %>% filter(!(candidato == cand_ & end > fr_))
  }
}

# Separar competitivos y marginales (→ Otro_agr)
fecha_ref  <- max(polls_long$end, na.rm = TRUE)
cands_protegidos <- intersect("BlancoNulo", cand_cols)
umbral_use <- if (exists("umbral_competitivo_pct")) umbral_competitivo_pct else 1.0
cands_comp <- polls_long %>%
  filter(end >= fecha_ref - 30) %>%
  group_by(candidato) %>%
  summarise(m = mean(y_pct, na.rm = TRUE), .groups = "drop") %>%
  filter(m >= umbral_use) %>% pull(candidato) %>% union(cands_protegidos)
cands_no_comp <- setdiff(cand_cols, cands_comp)

polls_wide <- polls_long %>%
  select(poll_id, pollster, end, n_total, mode, DEFF, n_eff, lambda_i,
         candidato, y_pct) %>%
  pivot_wider(names_from = candidato, values_from = y_pct, values_fill = 0) %>%
  arrange(end)

if (length(cands_no_comp) > 0) {
  nc_present <- intersect(cands_no_comp, names(polls_wide))
  polls_wide$Otro_agr <- if (length(nc_present) > 0)
    rowSums(polls_wide[, nc_present, drop = FALSE]) else 0
  polls_wide <- polls_wide[, !names(polls_wide) %in% nc_present]
} else polls_wide$Otro_agr <- 0

# lambda_freq (concentración de mercado por encuestadora)
polls_wide$lambda_freq <- 1.0
if (exists("compute_market_share") && exists("tau_market_share") && exists("gamma_share")) {
  tryCatch({
    poll_dates <- polls_wide %>% distinct(poll_id, pollster, end) %>%
      mutate(t_poll = end) %>% arrange(t_poll)
    poll_market <- poll_dates %>% rowwise() %>%
      mutate(info = list(compute_market_share(t_poll, pollster, poll_dates, tau_market_share)),
             share_actual = info$share_actual, share_fair = info$share_fair) %>%
      ungroup() %>% select(-info) %>%
      mutate(lambda_freq = pmax(1, (share_actual / share_fair)^gamma_share)) %>%
      distinct(poll_id, .keep_all = TRUE)
    polls_wide <- polls_wide %>% select(-any_of("lambda_freq")) %>%
      left_join(poll_market %>% select(poll_id, lambda_freq),
                by = "poll_id", relationship = "many-to-one") %>%
      mutate(lambda_freq = replace_na(lambda_freq, 1.0))
  }, error = function(e) message("lambda_freq: usando 1.0"))
}

K_cols <- intersect(c(cands_comp, "Otro_agr"), names(polls_wide))
K <- length(K_cols); J <- nrow(polls_wide)

pct_to_counts <- function(pct, n_eff) {
  n <- max(round(n_eff), 1L)
  if (sum(pct) <= 0) return(rep(0L, length(pct)))
  raw <- pct * n / sum(pct); y <- floor(raw); deficit <- n - sum(y)
  if (deficit > 0) { top <- order(raw - y, decreasing = TRUE)[seq_len(deficit)]; y[top] <- y[top] + 1L }
  as.integer(pmax(y, 0L))
}
Y_mat <- matrix(0L, J, K, dimnames = list(NULL, K_cols))
for (j in 1:J)
  Y_mat[j, ] <- pct_to_counts(as.numeric(polls_wide[j, K_cols]), polls_wide$n_eff[j])

fechas_u <- sort(unique(polls_wide$end))
T_ <- length(fechas_u); S <- max(T_ - 1L, 0L)
T_pred <- max(0, as.numeric(FECHA_PRIMERA_VUELTA - max(polls_wide$end)))

stan_data <- list(
  J = J, K = K, T = T_, H = length(unique(polls_wide$pollster)),
  Y = Y_mat,
  t_idx = as.array(match(polls_wide$end, fechas_u)),
  h_idx = as.array(match(polls_wide$pollster, sort(unique(polls_wide$pollster)))),
  S = S, dt = if (S > 0) as.numeric(diff(fechas_u)) else array(1.0, 1),
  days_ago         = as.numeric(max(polls_wide$end) - polls_wide$end),
  lambda_freq      = polls_wide$lambda_freq,
  lambda_i         = polls_wide$lambda_i,
  T_pred           = T_pred,
  sigma_rw_loc     = MCMC_HYPERPARAMS$sigma_rw_loc,
  sigma_rw_scale   = MCMC_HYPERPARAMS$sigma_rw_scale,
  sigma_house_rate = MCMC_HYPERPARAMS$sigma_house_rate,
  kappa_loc        = MCMC_HYPERPARAMS$kappa_loc,
  kappa_scale      = MCMC_HYPERPARAMS$kappa_scale,
  time_decay_rate  = MCMC_HYPERPARAMS$time_decay_rate
)
cat(sprintf("Datos: J=%d, K=%d, T=%d, T_pred=%.0f días\n", J, K, T_, T_pred))

# -----------------------------------------------------------------------
# PASO 2: MCMC baseline
# -----------------------------------------------------------------------
STAN_FILE <- "modelo_dm_rw_param.stan"
if (!file.exists(STAN_FILE)) stop("No se encontró ", STAN_FILE)
modelo <- cmdstan_model(normalizePath(STAN_FILE), compile = TRUE)

cat(sprintf("MCMC: %d chains × (%d warmup + %d sampling)...\n",
            MCMC_CHAINS, MCMC_WARMUP, MCMC_SAMPLING))
fit <- modelo$sample(
  data = stan_data, chains = MCMC_CHAINS, parallel_chains = MCMC_CHAINS,
  iter_warmup = MCMC_WARMUP, iter_sampling = MCMC_SAMPLING,
  seed = 42, refresh = 500, adapt_delta = MCMC_ADAPT_DELTA,
  max_treedepth = 12, show_messages = TRUE
)
diag <- fit$diagnostic_summary()
cat(sprintf("Diagnóstico: %d divergencias, BFMI_min=%.2f\n",
            sum(diag$num_divergent), min(diag$ebfmi, na.rm = TRUE)))

p_draws <- fit$draws("p_election", format = "draws_matrix")
sim_mat  <- 100 * as.matrix(p_draws)
colnames(sim_mat) <- K_cols

pred_baseline <- tibble(
  candidato = K_cols,
  mu_pct    = colMeans(sim_mat),
  sd_pct    = apply(sim_mat, 2, sd),
  lo80      = apply(sim_mat, 2, quantile, 0.10),
  hi80      = apply(sim_mat, 2, quantile, 0.90),
  lo95      = apply(sim_mat, 2, quantile, 0.025),
  hi95      = apply(sim_mat, 2, quantile, 0.975)
) %>% arrange(desc(mu_pct))

# -----------------------------------------------------------------------
# PASO 3: Modo C — momentum Trends con bloques ideológicos
# -----------------------------------------------------------------------
# El delta de Trends se centra DENTRO de cada bloque ideológico.
# Cepeda (solo en IZQUIERDA) tiene delta = 0: su base no se mueve por Trends.
# Soft clamp: dentro del IC80 pasa todo; fuera solo EXTRAPOLATION_SCALE del exceso.
aplicar_modo_C <- function(pred, trends_crudo, fecha_corte, factor,
                            ventana = 14, bloques = NULL,
                            extrapolation_scale = 0.20) {
  fecha_corte <- as.Date(fecha_corte)
  rec <- trends_crudo %>%
    filter(date > fecha_corte - ventana, date <= fecha_corte) %>%
    group_by(candidato) %>% summarise(hits_rec = mean(hits, na.rm = TRUE), .groups = "drop")
  ant <- trends_crudo %>%
    filter(date > fecha_corte - 2*ventana, date <= fecha_corte - ventana) %>%
    group_by(candidato) %>% summarise(hits_ant = mean(hits, na.rm = TRUE), .groups = "drop")

  mom <- rec %>% full_join(ant, by = "candidato") %>%
    mutate(hits_rec = coalesce(hits_rec, 0), hits_ant = coalesce(hits_ant, 0),
           delta    = log(hits_rec + 1) - log(hits_ant + 1)) %>%
    filter(hits_rec > 0 | hits_ant > 0)  # excluir 0->0 puro ruido
  mom$delta_c <- 0

  if (!is.null(bloques)) {
    cands_en_bloques <- c()
    for (bn in names(bloques)) {
      cb <- intersect(bloques[[bn]], mom$candidato)
      cands_en_bloques <- c(cands_en_bloques, cb)
      if (length(cb) < 2) next  # bloque unitario: delta_c = 0 (sin competencia)
      idx <- mom$candidato %in% cb
      mom$delta_c[idx] <- mom$delta[idx] - mean(mom$delta[idx], na.rm = TRUE)
    }
    mom <- mom[mom$candidato %in% cands_en_bloques, ]
  } else {
    mom$delta_c <- mom$delta - mean(mom$delta, na.rm = TRUE)
  }

  pred_new <- pred
  for (cc in intersect(pred$candidato, mom$candidato)) {
    i <- which(pred_new$candidato == cc)
    d <- mom$delta_c[mom$candidato == cc][1]
    if (is.na(d) || d == 0) next
    mu   <- pred_new$mu_pct[i]
    lo80 <- pred_new$lo80[i]
    hi80 <- pred_new$hi80[i]
    p_raw <- plogis(qlogis(min(max(mu/100, 1e-4), 1-1e-4)) + factor * d) * 100
    pred_new$mu_pct[i] <- if (p_raw > hi80) hi80 + (p_raw - hi80) * extrapolation_scale
                          else if (p_raw < lo80) lo80 - (lo80 - p_raw) * extrapolation_scale
                          else p_raw
  }

  # Renormalizar solo los candidatos en bloques; preservar masa de residuales
  if (!is.null(bloques)) {
    aj <- intersect(unique(unlist(bloques)), pred_new$candidato)
    idx_aj <- pred_new$candidato %in% aj
    masa_antes   <- sum(pred$mu_pct[pred$candidato %in% aj])
    masa_despues <- sum(pred_new$mu_pct[idx_aj])
    if (masa_despues > 0)
      pred_new$mu_pct[idx_aj] <- pred_new$mu_pct[idx_aj] * masa_antes / masa_despues
  } else {
    pred_new$mu_pct <- pred_new$mu_pct * 100 / sum(pred_new$mu_pct)
  }
  pred_new %>% arrange(desc(mu_pct))
}

TRENDS_RDS <- file.path(OUTPUT_DIR, "trends_presidencial.rds")
if (file.exists(TRENDS_RDS)) {
  trends_crudo <- readRDS(TRENDS_RDS)
  fecha_corte  <- max(trends_crudo$date)
  pred_final   <- aplicar_modo_C(pred_baseline, trends_crudo, fecha_corte,
                                  factor      = FACTOR_C,
                                  ventana     = VENTANA_DIAS,
                                  bloques     = BLOQUES_2026,
                                  extrapolation_scale = EXTRAPOLATION_SCALE)
  cat(sprintf("Modo C aplicado: factor=%.2f, ventana=%dd, datos hasta %s\n",
              FACTOR_C, VENTANA_DIAS, as.character(fecha_corte)))
} else {
  cat("⚠️  trends_presidencial.rds no encontrado — usando baseline puro\n")
  pred_final <- pred_baseline
}

# -----------------------------------------------------------------------
# PASO 4: Pronóstico final
# -----------------------------------------------------------------------
cands_eval <- intersect(CANDS_CONCURSO, pred_final$candidato)
pred_concurso <- pred_final %>%
  filter(candidato %in% cands_eval) %>%
  mutate(pronostico_pct = mu_pct) %>%   # pct bruto sobre todos los candidatos
  arrange(desc(pronostico_pct))

cat("\n################################################################\n")
cat("##  LECHONA DE HIPOPOTAMO — Pronostico Concurso             ##\n")
cat(sprintf("##  Dia E: %-44s ##\n",
            format(FECHA_PRIMERA_VUELTA, "%d de %B de %Y")))
cat("################################################################\n")
for (i in seq_len(nrow(pred_concurso))) {
  cat(sprintf("  %-22s  %5.2f%%   IC80%% [%.1f, %.1f]\n",
              pred_concurso$candidato[i],
              pred_concurso$pronostico_pct[i],
              pred_concurso$lo80[i],
              pred_concurso$hi80[i]))
}
cat(sprintf("\n  Suma 5 candidatos = %.2f%%  (resto: BlancoNulo/Otro/NSNR)\n",
            sum(pred_concurso$pronostico_pct)))
cat("################################################################\n\n")

# -----------------------------------------------------------------------
# PASO 5: Exportar CSV
# -----------------------------------------------------------------------
csv_path <- file.path(OUTPUT_DIR, "prediccion_lechona_concurso.csv")
pred_concurso %>%
  select(candidato, pronostico_pct, lo80, hi80) %>%
  write_csv(csv_path)
cat(sprintf("CSV: %s\n", csv_path))
cat(sprintf("=== Completado en %.1f min ===\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
