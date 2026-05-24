# =========================================================
# CONFIG.R - Hiperparámetros centralizados del modelo electoral 2026
# =========================================================
# Este archivo es la ÚNICA fuente de verdad para los hiperparámetros.
# Cualquier cambio aquí se propaga automáticamente a:
#   01_consultas.R, 02_primera_vuelta.R, 03_segunda_vuelta.R
#
# Los valores óptimos (w_hist, w_consulta, etc.) provienen del grid
# search ejecutado al final de 01_consultas.R. Si re-entrenas, copia
# los valores aquí.
# =========================================================

# ---------------------------------------------------------
# A) Hiperparámetros del Filtro de Kalman v7
# ---------------------------------------------------------
m_house_min      <- 1
n_pseudo         <- 2
alpha_residual   <- 0.5
error_base_neutral <- 1.5

R_MAX            <- 1e3
Q_bounds         <- c(1e-8, 2.0)   # valor óptimo del grid search
tau_bounds       <- c(1e-6, 2.0)
Q_house_bounds   <- c(1e-8, 1e-2)

var_last_cap     <- 0.5
min_obs_full_var <- 3
P_inicial_logit  <- 1.5

# Cap de varianza en escala logit para prediccion_dinamica.
# Sin cap, candidatos con pocas observaciones dan IC imposibles ([0%, 98%]).
# VAR_LOGIT_CAP = 1.5 → IC [~0.5%, ~38%] para un candidato al 3%.
# Reducir si los ICs siguen siendo demasiado amplios.
VAR_LOGIT_CAP    <- 1.5

# Cap de semi-amplitud máxima en espacio de probabilidad (pp del censo).
# El cap logit plano (1.5) crea artifacts para candidatos en el rango medio:
#   Cepeda al 27% con var=1.5 → IC_hi = 80%, P(>50%) = 21% — absurdo.
# Con este cap en pp: Cepeda al 27%, max_hi = 42%, P(>50%) < 0.2%.
# Se aplica a candidatos ≥ UMBRAL_PEQUENO_PCT. Los near-zero siguen con K×mu.
MAX_IC_HALFWIDTH_PP <- 15.0   # pp: IC superior ≤ mu_hat + 15pp

# Cap adaptativo para candidatos near-zero.
# Un cap logit uniforme es demasiado ancho en términos relativos:
#   Barreras al 0.55% con cap=1.5 → IC superior ≈ 5.7% = 10× su estimado.
# La restricción relativa dice: IC superior ≤ K_HI_PEQUENO × mu_hat_pct.
#   Para K_HI_PEQUENO=5 y Barreras 0.55% → IC superior ≤ 2.75%.
# Se aplica solo a candidatos bajo UMBRAL_PEQUENO_PCT.
UMBRAL_PEQUENO_PCT <- 2.0   # pp: candidatos "pequeños" (< 2pp del censo)
K_HI_PEQUENO      <- 5.0   # máximo IC superior = 5× el estimado puntual

# ---------------------------------------------------------
# B) Detección de herding (rho_house)
# ---------------------------------------------------------
rho_house_init       <- 0.30
rho_house_prior_mean <- 0.15
rho_house_prior_sd   <- 0.30

# ---------------------------------------------------------
# B2) Devaluación inteligente de encuestas (Kalman robusto)
# ---------------------------------------------------------
# Si una encuesta tiene innovación normalizada v²/S > chi2_threshold,
# su R se infla por el factor v²/(S × chi2_threshold).
# Intuición: polls que contradicen el random walk se creen menos.
# chi2_threshold = 3.84 ≡ percentil 95 del χ²(1).
#
# innovation_decay = FALSE → comportamiento original (sin devaluación)
# innovation_decay = TRUE  → activar para ver si suaviza polls viejos
#                            que siguen "altos" sin justificación reciente.
#
# RECOMENDACIÓN: dejar FALSE hasta verificar con datos nuevos.
# Activar con TRUE y comparar trayectorias antes/después.
innovation_decay <- TRUE
chi2_threshold   <- 3.84

# Devaluación temporal de encuestas antiguas.
# Infla el R de cada observación según su antigüedad:
#   R_efectivo = R × exp(TIME_DECAY_RATE × dias_antes_de_la_ultima_encuesta)
#
# La encuesta más reciente nunca se devalúa (factor = 1).
# Las encuestas antiguas tienen R más grande → el Kalman les cree menos.
#
# Esto también afecta el MLE que estima Q_hat: con encuestas viejas
# down-ponderadas, el Q estimado es mayor si hay caídas recientes,
# permitiendo que el RW "acepte" cambios estructurales sin pelearlos.
#
# Semivida = ln(2) / TIME_DECAY_RATE ≈ 0.693 / rate
#   rate = 0.000  → sin devaluación (equivalente a antes)
#   rate = 0.012  → semivida ~58 días  (recomendado)
#   rate = 0.023  → semivida ~30 días  (agresivo)
#
# Con rate=0.012: encuesta de hace 60 días → 2× menos peso
#                 encuesta de hace 120 días → 4× menos peso
TIME_DECAY_RATE <- 0.024   # semivida ~58 días

# ---------------------------------------------------------
# C) Penalización por concentración de mercado (Hill-1)
# ---------------------------------------------------------
tau_market_share <- 30   # días: escala temporal del kernel
gamma_share      <- 1.0  # intensidad: 1.0 = lineal

# ---------------------------------------------------------
# D) Pesos del error (entrenados en 01_consultas.R grid search)
# ---------------------------------------------------------
w_hist                 <- 0.40
w_consulta             <- 0.60
w_market_shrink        <- 0.30
sigma_regime_change_pp <- 0.40

# ---------------------------------------------------------
# E) Simulación Monte Carlo + Proyección dinámica
# ---------------------------------------------------------
N_sim                  <- 10000
kappa_OU               <- 0.04
umbral_competitivo_pct <- 1.5

# E2) Zona caliente (últimos días antes de la elección)
# hot_zone_mult escala Q en esa ventana. 2-3x = conservador, 4-5x = agresivo.
hot_zone_days <- 7     # días antes de la elección en zona caliente
hot_zone_mult <- 3.0   # Q × hot_zone_mult dentro de la zona caliente

# E3) Tendencia pre-electoral (señal sobre el ruido)
# La pendiente de los últimos puntos Kalman se extrapola solo si supera
# umbral_SNR × sigma_pendiente_RW. Con encuestas espaciadas el SNR tiende
# a ser bajo, así que solo candidatos con movimiento fuerte lo activan.
n_trend_obs       <- 5     # puntos Kalman usados para estimar pendiente
umbral_SNR        <- 1.5   # SNR mínimo para extrapolar la pendiente
trend_uncertainty <- 0.50  # fracción de la tendencia proyectada (0-1)

# ---------------------------------------------------------
# E4) Google Trends como señal de alta frecuencia
# ---------------------------------------------------------
# USAR_TRENDS = FALSE no descarga ni usa Trends en 02/03.
# MODO_TRENDS = "full"    → señal en TODO el período de encuestas
#             = "hotzone" → solo en los últimos hot_zone_days días
#             = "ambos"   → corre los dos y compara MAE (modo diagnóstico)
USAR_TRENDS  <- TRUE
MODO_TRENDS  <- "hotzone"   # cambiar a "full" o "hotzone" en producción

# Participación esperada en primera vuelta (histórico Colombia ~55 %).
# Se usa para escalar Trends a % del censo cuando aún no hay resultado real.
PARTICIPACION_ESPERADA_PV <- 55.0   # pp del censo

# Keywords de Google Trends para candidatos presidenciales.
# Verificar y actualizar según el nombre más buscado en Google Colombia.
# Formato: lista con nombre del código interno → string para la query.
# IMPORTANTE: estos son los nombres que TÚ verificaste manualmente.
# Si necesitas cambiarlos, hazlo SOLO aquí (este archivo es la única fuente de verdad).
TRENDS_KW_PRESIDENCIAL <- list(
  Cepeda        = "Ivan Cepeda",
  DeLaEspriella = "Abelardo De La Espriella",
  Fajardo       = "Sergio Fajardo",
  ClaudiaLopez  = "Claudia Lopez",
  Valencia      = "Paloma Valencia",
  Uribe         = "Miguel Uribe",
  Lizcano       = "Diego Lizcano",
  Murillo       = "Luis Gilberto Murillo"
)

# Keywords para candidatos de las consultas (calibración).
# Valencia = ancla entre grupos de PorColombia.
TRENDS_KW_PORCOLOMBIA <- list(
  Valencia  = "Paloma Valencia",
  Oviedo    = "Juan Daniel Oviedo",
  Gaviria   = "Aníbal Gaviria",
  Penalosa  = "Enrique Penalosa",
  Cardenas  = "Mauricio Cardenas",
  Galan     = "Juan Manuel Galan",
  Davila    = "Vicky Davila",
  Pinzon    = "Juan Carlos Pinzon",
  Luna      = "David Luna"
)
TRENDS_ANCLA_PORCOLOMBIA <- "Valencia"

# Categoría temática de Google Trends para filtrar resultados.
# Esto evita que el ranking se contamine con búsquedas no políticas
# (futbolistas homónimos, cantantes, etc.). Categorías relevantes:
#   0   = Todas las categorías (default de gtrendsR)
#   16  = News
#   396 = Politics            <- RECOMENDADO para este pipeline
#   184 = Government
# Para verlas todas: gtrendsR::categories
TRENDS_CATEGORY <- 396



# ---------------------------------------------------------
# E4.4) Transferencia del Momentum desde consultas a primera vuelta
# ---------------------------------------------------------
# Al aplicar los parámetros LOO de consultas en la primera vuelta,
# no hay garantía de generalización (contextos distintos, n=9 muy pequeño).
# El shrinkage interpola entre los params LOO y el prior neutro (sin ajuste):
#   alpha_used = S * alpha_loo + (1 - S) * 1.0   ← hacia "confiar en el Kalman"
#   beta_used  = S * beta_loo  + (1 - S) * 0.0   ← hacia "sin impulso"
#   delta_used = S * delta_loo + (1 - S) * 0.0   ← toward "sin saturacion"
#
# S = 0.0 → ignorar completamente las consultas (solo Kalman)
# S = 0.5 → mezcla conservadora (RECOMENDADO para predicción out-of-sample)
# S = 1.0 → confiar completamente en los params de consultas
#
# Para el concurso: usar S = 0.5 y reportar sensibilidad.
MOMENTUM_SHRINKAGE <- 0.5


# ---------------------------------------------------------
# E4.5) Alcance: qué descargar
# ---------------------------------------------------------
# Para optimizar y testear, puedes saltarte las presidenciales por ahora.
# Solo descargará PorColombia (consultas).
DESCARGAR_PRESIDENCIALES <- TRUE   # TRUE para incluir presidenciales


# ---------------------------------------------------------
# E5) Pausas para Google Trends (anti rate-limit 429)
# ---------------------------------------------------------
# Google Trends bloquea con HTTP 429 si pides muchas queries seguidas.
# Estas variables son las palancas para evitar el bloqueo:
#   - PAUSA_ENTRE_LLAMADAS_SEG: entre dos descargas seguidas
#   - PAUSA_ENTRE_BLOQUES_SEG : entre bloques (PorColombia → Presidenciales)
# Si sigues viendo 429 a pesar del cache, sube estos valores.
# Si tu IP ya está bloqueada, espera 1h y reintenta — el cache antiguo
# se usará hasta que la API responda de nuevo.
PAUSA_ENTRE_LLAMADAS_SEG <- 300   # 5 min entre descargas
PAUSA_ENTRE_BLOQUES_SEG  <- 900   # 15 min entre PorColombia y Presidenciales

# Si una descarga ya está en cache (key idéntica y < MAX_CACHE_AGE_HOURS),
# las pausas anteriores SE OMITEN. Cache hit = no llamada = no espera.

# Edad máxima del cache de Trends antes de re-descargar (horas)
MAX_CACHE_AGE_HOURS <- 24   # 24h normal; periodos historicos (fin < ayer) usan 30 dias

# Backoff exponencial cuando Google devuelve 429 (rate limit).
# El total de espera (sumar todos los elementos) es el tiempo máximo que el
# script puede quedarse colgado en una sola descarga.
# Default abajo: 5m + 15m + 30m + 1h + 2h + 4h = ~8 horas.
# Pensado para correr toda la noche con nohup: si Google bloquea, el script
# espera hasta que libere; no hace falta supervisión.
BACKOFF_DELAYS_SEG <- c(300, 900, 1800, 3600, 7200, 14400)
                     # 5min, 15min, 30min, 1h,   2h,   4h

# ---------------------------------------------------------
# F) Fechas clave
# ---------------------------------------------------------
FECHA_PRIMERA_VUELTA <- as.Date("2026-05-31")
FECHA_SEGUNDA_VUELTA <- as.Date("2026-06-21")
FECHA_CORTE_B2       <- as.Date("2026-02-15")  # filtro FrenteVida (post-retiros)

# ---------------------------------------------------------
# G) Participación REAL ex-post (consultas Mar 2026)
# ---------------------------------------------------------
PARTICIPACION_REAL <- c(
  PorColombia = 28.02,
  FrenteVida  = 2.85,
  Soluciones  = 2.96
)

# ---------------------------------------------------------
# H) Boletas de consultas
# ---------------------------------------------------------
BOLETA_PorColombia <- c("Cardenas","Davila","Galan","Gaviria","Luna",
                        "Oviedo","Penalosa","Pinzon","Valencia")
BOLETA_2           <- c("Barreras","Bernal","Cepeda","Cristo","Murillo",
                        "Pineda","Quintero","Romero","Torres")
BOLETA_3           <- c("Huerta","Lopez")

# ---------------------------------------------------------
# I) MOE de fallback por casa (cuando no se reporta)
# ---------------------------------------------------------
MOE_FALLBACK <- tibble::tibble(
  pollster     = c("Invamer","AtlasIntel","GAD3","Guarumo",
                   "CNC / Cambio","YanHass","W.A.A","CELAG"),
  moe_fallback = c(2.79, 3.00, 3.00, 2.00, 3.00, 2.40, 2.60, 2.80)
)

# ---------------------------------------------------------
# J) Errores históricos pre-2026 (Presidenciales 2022)
# ---------------------------------------------------------
# error_historico = MAE de la última encuesta de cada casa
#                   en la 1ra vuelta presidencial 2022.
# error_consulta  = SE GENERA dinámicamente al correr 01_consultas.R
#                   y se guarda en outputs/error_consulta.rds
ERRORES_HISTORICOS_PRE <- tibble::tibble(
  pollster        = c("Guarumo", "Invamer", "CNC / Cambio",
                      "YanHass", "AtlasIntel", "GAD3", "W.A.A"),
  error_historico = c(3.75, 1.96, 5.02, 7.79, 2.92, 1.48, 3.50)
)

# Fallback de error_consulta si 01_consultas.R aún no se ha corrido
ERRORES_CONSULTA_FALLBACK <- tibble::tibble(
  pollster       = c("Guarumo", "Invamer", "CNC / Cambio",
                     "YanHass", "AtlasIntel", "GAD3", "W.A.A"),
  error_consulta = c(2.40, 2.70, 2.50, 4.80, 4.40, 2.80, 3.50)
)

# ---------------------------------------------------------
# K) MAE intolerable (escala 1-10 de penalización de casas)
# ---------------------------------------------------------
mae_intolerable <- 10.0

# ---------------------------------------------------------
# L) Paleta de colores friendly (Okabe-Ito) y formas
# ---------------------------------------------------------
ok_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7", "#999999", "#882255")
ok_shapes  <- c(16, 17, 15, 18, 8, 4, 11, 3, 13)
ok_lines   <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

# ---------------------------------------------------------
# L2) Candidatos retirados de la carrera
# ---------------------------------------------------------
# Para candidatos retirados:
#   - Sus observaciones POSTERIORES a la fecha de retiro se excluyen
#     del Kalman (para no sesgar el modelo con datos fantasma).
#   - Sus predicciones se fuerzan a 0 en prediccion_dinamica.
#   - En las gráficas reciben un label "Retirado/a (fecha)" en vez de porcentaje.
# Formato: named list(candidato = fecha_retiro)
CANDIDATOS_RETIRADOS <- list(
  ClaraLopez = as.Date("2026-04-06"),
  Murillo    = as.Date("2026-05-06")
)

# ---------------------------------------------------------
# M) Directorios de output
# ---------------------------------------------------------
OUTPUT_DIR <- "outputs"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ---------------------------------------------------------
# N) Semilla global
# ---------------------------------------------------------
set.seed(42)

cat("[config.R] Hiperparámetros cargados.\n")
