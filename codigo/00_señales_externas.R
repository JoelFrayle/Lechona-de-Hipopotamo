# =====================================================================
# 00_señales_externas.R
# =====================================================================
# Post-procesa los CSVs descargados por fetch_trends.py y los normaliza
# usando el ancla de cada periodo. Deja todo listo para 07_analisis_trends.R
# y para futuros backtests.
#
# INPUTS (de fetch_trends.py):
#   outputs/trends_pv_2026_g{1,2,3}.csv          presidenciales 2026
#   outputs/trends_hist_2018_g{1,2}.csv          histórico 2018
#   outputs/trends_hist_2022_g{1,2}.csv          histórico 2022
#   outputs/trends_consulta_2026_g{1,2}.csv      consulta PorColombia
#
# OUTPUTS:
#   outputs/trends_presidencial.rds              ← 07_analisis_trends.R
#   outputs/trends_consultas.rds                 ← 07_analisis_trends.R
#   outputs/trends_hist_2018.rds                 ← futuros backtests
#   outputs/trends_hist_2022.rds                 ← futuros backtests
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble)
})

OUTPUT_DIR <- "outputs"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# =====================================================================
# MAPAS DE KEYWORDS (DEBEN COINCIDIR EXACTAMENTE CON fetch_trends.py)
# =====================================================================
# Mapeo: "Keyword Trends" → "Código en R"
#
# NOTA: las ANCLAS son ENTIDADES POLÍTICAS ESPECÍFICAS que NO compiten
# en el periodo (presidentes en funciones o salientes). Esto evita dos
# problemas: saturación por la propia carrera (que pasaría si la ancla
# fuera un candidato del periodo) y bot detection de Google (que activan
# queries genéricas tipo "elecciones presidenciales Colombia").
#
# Las anclas NO están en los mapas — al hacer el lookup `mapa_kw[keyword]`,
# quedan como NA y se descartan con filter(!is.na(candidato)).

MAPA_PV_2026 <- c(
  "Ivan Cepeda"               = "Cepeda",
  "Claudia Lopez"             = "ClaudiaLopez",
  "Abelardo De La Espriella"  = "DeLaEspriella",
  "Roy Barreras"              = "Barreras",
  "Santiago Botero"           = "Botero",
  "Carlos Caicedo"            = "Caicedo",
  "Sergio Fajardo"            = "Fajardo",
  "Sondra Macollins"          = "Macollins",
  "Mauricio Lizcano"          = "Lizcano",
  "Gustavo Matamoros"         = "Matamoros",
  "Miguel Uribe"              = "Uribe",
  "Paloma Valencia"           = "Valencia"
)
ANCLA_PV_2026 <- "Gustavo Petro"   # presidente actual, NO candidato

MAPA_HIST_2018 <- c(
  "Gustavo Petro"             = "Petro",
  "Ivan Duque"                = "Duque",
  "Humberto de la Calle"      = "DeLaCalle",
  "Jorge Trujillo Sarmiento"  = "TrujilloSarmiento",
  "Sergio Fajardo"            = "Fajardo",
  "Viviane Morales"           = "VivianeMorales",
  "Piedad Cordoba"            = "PiedadCordoba",
  "German Vargas Lleras"      = "VargasLleras"
)
ANCLA_HIST_2018 <- "Juan Manuel Santos"   # presidente saliente en 2018

MAPA_HIST_2022 <- c(
  "Gustavo Petro"             = "Petro",
  "Rodolfo Hernandez"         = "Hernandez",
  "John Milton Rodriguez"     = "JohnMiltonRodriguez",
  "Federico Gutierrez"        = "FedericoGutierrez",
  "Sergio Fajardo"            = "Fajardo",
  "Enrique Gomez"             = "EnriqueGomez",
  "Luis Perez"                = "LuisPerez",
  "Ingrid Betancourt"         = "IngridBetancourt"
)
ANCLA_HIST_2022 <- "Ivan Duque"           # presidente saliente en 2022

MAPA_CONSULTA_2026 <- c(
  "Paloma Valencia"           = "Valencia",
  "Oviedo PorColombia"        = "Oviedo",
  "Juan Manuel Galan"         = "Galan",
  "Pinzon PorColombia"        = "Pinzon",
  "Vicky Davila"              = "Davila",
  "Enrique Penalosa"          = "Penalosa",
  "Gaviria PorColombia"       = "Gaviria",
  "Luna PorColombia"          = "Luna",
  "Mauricio Cardenas"         = "Cardenas"
)
ANCLA_CONSULTA_2026 <- "Gustavo Petro"    # no participa en la consulta

# =====================================================================
# FUNCIÓN DE NORMALIZACIÓN POR ANCLA
# =====================================================================
# Cuando Google Trends devuelve un grupo de keywords, normaliza
# internamente a [0, 100] dentro de ese grupo. Si la ancla (kw común
# entre grupos) sale con max=80 en g1 y max=20 en g2, hay que multiplicar
# g2 por (80/20) = 4 para alinearlo a la escala de g1.

normalizar_con_ancla <- function(csvs, ancla, mapa_kw, periodo_label) {
  if (length(csvs) == 0) {
    cat(sprintf("  [%s] No se encontraron CSVs\n", periodo_label))
    return(NULL)
  }

  # Cargar todos los grupos (ordenados por nombre de archivo)
  csvs <- sort(csvs)
  cat(sprintf("  [%s] Cargando %d grupo(s): %s\n",
              periodo_label, length(csvs), paste(basename(csvs), collapse = ", ")))

  grupos <- lapply(csvs, function(f) {
    suppressMessages(read_csv(f, show_col_types = FALSE)) %>%
      mutate(date = as.Date(date), hits = as.numeric(hits))
  })

  # Promedio del ancla en cada grupo (solo valores positivos)
  ancla_mean <- map_dbl(grupos, function(g) {
    vals <- g$hits[g$keyword == ancla & g$hits > 0]
    if (length(vals) == 0) NA_real_ else mean(vals, na.rm = TRUE)
  })

  if (any(is.na(ancla_mean))) {
    warning(sprintf("[%s] Ancla '%s' no aparece (o es 0) en algunos grupos. ",
                    periodo_label, ancla),
            "Revisar que el nombre del ancla coincida entre R y Python.")
  }

  # Factor de escala: multiplicar cada grupo por (ancla_g1 / ancla_gN)
  factores <- ancla_mean[1] / ancla_mean
  factores[is.na(factores) | !is.finite(factores)] <- 1.0

  cat(sprintf("  [%s] Factores: %s\n", periodo_label,
              paste(sprintf("g%d=%.2fx", seq_along(factores), factores),
                    collapse = ", ")))

  # Escalar cada grupo
  grupos_escalados <- map2(grupos, factores, function(g, f) {
    g %>% mutate(hits = hits * f)
  })

  # Eliminar la ancla de grupos g2, g3, ... (mantener solo del g1)
  if (length(grupos_escalados) > 1) {
    for (i in 2:length(grupos_escalados)) {
      grupos_escalados[[i]] <- grupos_escalados[[i]] %>%
        filter(keyword != ancla)
    }
  }

  # Combinar, mapear a códigos R, ordenar
  kws_esperados <- names(mapa_kw)
  combinado <- bind_rows(grupos_escalados) %>%
    mutate(candidato = mapa_kw[keyword])

  # Diagnóstico: keywords que vinieron pero no están en el mapa
  kws_no_mapeados <- combinado %>%
    filter(is.na(candidato)) %>%
    pull(keyword) %>% unique()
  if (length(kws_no_mapeados) > 0) {
    warning(sprintf("[%s] Keywords no mapeados (revisar): %s",
                    periodo_label, paste(kws_no_mapeados, collapse = ", ")))
  }

  # Diagnóstico: candidatos esperados que no llegaron
  kws_no_recibidos <- setdiff(kws_esperados, unique(combinado$keyword))
  if (length(kws_no_recibidos) > 0) {
    cat(sprintf("  [%s] ⚠️  Keywords esperados sin datos: %s\n",
                periodo_label, paste(kws_no_recibidos, collapse = ", ")))
  }

  resultado <- combinado %>%
    filter(!is.na(candidato)) %>%
    select(date, candidato, hits) %>%
    arrange(candidato, date)

  cat(sprintf("  [%s] ✓ Resultado: %d filas, %d candidatos, %s a %s\n",
              periodo_label, nrow(resultado),
              length(unique(resultado$candidato)),
              min(resultado$date), max(resultado$date)))

  resultado
}

# =====================================================================
# PROCESAR CADA PERIODO
# =====================================================================
cat("[00] === Iniciando post-procesamiento de Google Trends ===\n\n")

# ---- PRESIDENCIALES 2026 ----
csvs_pv_2026 <- list.files(OUTPUT_DIR,
                            pattern = "^trends_pv_2026_g\\d+\\.csv$",
                            full.names = TRUE)
trends_pv <- normalizar_con_ancla(csvs_pv_2026, ANCLA_PV_2026,
                                    MAPA_PV_2026, "pv_2026")
if (!is.null(trends_pv)) {
  saveRDS(trends_pv, file.path(OUTPUT_DIR, "trends_presidencial.rds"))
  cat(sprintf("  → Guardado: outputs/trends_presidencial.rds\n\n"))
} else {
  cat("\n")
}

# ---- HISTÓRICO 2018 ----
csvs_2018 <- list.files(OUTPUT_DIR,
                         pattern = "^trends_hist_2018_g\\d+\\.csv$",
                         full.names = TRUE)
trends_2018 <- normalizar_con_ancla(csvs_2018, ANCLA_HIST_2018,
                                      MAPA_HIST_2018, "hist_2018")
if (!is.null(trends_2018)) {
  saveRDS(trends_2018, file.path(OUTPUT_DIR, "trends_hist_2018.rds"))
  cat(sprintf("  → Guardado: outputs/trends_hist_2018.rds\n\n"))
} else {
  cat("\n")
}

# ---- HISTÓRICO 2022 ----
csvs_2022 <- list.files(OUTPUT_DIR,
                         pattern = "^trends_hist_2022_g\\d+\\.csv$",
                         full.names = TRUE)
trends_2022 <- normalizar_con_ancla(csvs_2022, ANCLA_HIST_2022,
                                      MAPA_HIST_2022, "hist_2022")
if (!is.null(trends_2022)) {
  saveRDS(trends_2022, file.path(OUTPUT_DIR, "trends_hist_2022.rds"))
  cat(sprintf("  → Guardado: outputs/trends_hist_2022.rds\n\n"))
} else {
  cat("\n")
}

# ---- CONSULTA PORCOLOMBIA 2026 ----
csvs_consulta <- list.files(OUTPUT_DIR,
                              pattern = "^trends_consulta_2026_g\\d+\\.csv$",
                              full.names = TRUE)
trends_consulta <- normalizar_con_ancla(csvs_consulta, ANCLA_CONSULTA_2026,
                                          MAPA_CONSULTA_2026, "consulta_2026")
if (!is.null(trends_consulta)) {
  saveRDS(trends_consulta, file.path(OUTPUT_DIR, "trends_consultas.rds"))
  cat(sprintf("  → Guardado: outputs/trends_consultas.rds\n\n"))
} else {
  cat("\n")
}

# =====================================================================
# RESUMEN
# =====================================================================
cat("=================================================================\n")
cat("[00] RESUMEN\n")
cat("=================================================================\n")

resumen <- tibble(
  periodo = c("pv_2026", "hist_2018", "hist_2022", "consulta_2026"),
  rds_path = c("trends_presidencial.rds", "trends_hist_2018.rds",
                "trends_hist_2022.rds", "trends_consultas.rds"),
  resultado = c(
    if (!is.null(trends_pv)) "✓" else "✗",
    if (!is.null(trends_2018)) "✓" else "✗",
    if (!is.null(trends_2022)) "✓" else "✗",
    if (!is.null(trends_consulta)) "✓" else "✗"
  ),
  n_filas = c(
    if (!is.null(trends_pv)) nrow(trends_pv) else 0L,
    if (!is.null(trends_2018)) nrow(trends_2018) else 0L,
    if (!is.null(trends_2022)) nrow(trends_2022) else 0L,
    if (!is.null(trends_consulta)) nrow(trends_consulta) else 0L
  ),
  n_cands = c(
    if (!is.null(trends_pv)) length(unique(trends_pv$candidato)) else 0L,
    if (!is.null(trends_2018)) length(unique(trends_2018$candidato)) else 0L,
    if (!is.null(trends_2022)) length(unique(trends_2022$candidato)) else 0L,
    if (!is.null(trends_consulta)) length(unique(trends_consulta$candidato)) else 0L
  )
)
print(resumen)

cat("\n[00] ✅ Listo. Siguiente paso:\n")
cat("    Rscript 07_analisis_trends.R\n")
