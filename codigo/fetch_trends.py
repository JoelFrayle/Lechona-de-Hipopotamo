#!/usr/bin/env python3
"""
fetch_trends.py — Descarga TODOS los datos de Google Trends necesarios
para el análisis Colombia 2026.

Cubre 4 periodos con anclas compartidas para normalizar escala entre grupos:
  • pv_2026        — Presidenciales 2026 (12 candidatos, ancla: Iván Cepeda)
  • hist_2018      — Históricos 2018    ( 8 candidatos, ancla: Gustavo Petro)
  • hist_2022      — Históricos 2022    ( 8 candidatos, ancla: Gustavo Petro)
  • consulta_2026  — Consulta PorColombia 2026 (9 candidatos, ancla: Paloma Valencia)

Instalación:
    pip install pytrends pandas

Uso:
    python fetch_trends.py                      # todo
    python fetch_trends.py --periodo pv_2026    # solo un periodo
    python fetch_trends.py --force              # re-descarga incluso si CSV existe
    python fetch_trends.py --skip-existing      # default; salta CSVs ya descargados

Output (en outputs/):
    trends_pv_2026_g1.csv ... gN.csv
    trends_hist_2018_g1.csv ... gN.csv
    trends_hist_2022_g1.csv ... gN.csv
    trends_consulta_2026_g1.csv ... gN.csv

Procesamiento posterior:
    Rscript 00_señales_externas.R   # normaliza con ancla y guarda RDS
    Rscript 07_analisis_trends.R    # análisis de estrategias
"""

import argparse
import sys
import time
from pathlib import Path

import pandas as pd
from pytrends.request import TrendReq

# ============================================================
# CONFIGURACIÓN GLOBAL
# ============================================================
SCRIPT_DIR = Path(__file__).parent
OUTPUT_DIR = SCRIPT_DIR / "outputs"
OUTPUT_DIR.mkdir(exist_ok=True)

GEO            = "CO"
CAT            = 396     # Politics. Si da problemas: cambia a 0 (sin filtro)
MAX_KW_GRUPO   = 5       # límite de Google Trends por consulta
PAUSA_ENTRE_GRUPOS    = 45   # segundos entre grupos del mismo periodo
PAUSA_ENTRE_PERIODOS  = 90   # segundos entre periodos distintos

# Delays de reintento exponencial al recibir 429 (rate limit)
RETRY_DELAYS = [30, 60, 120, 300, 600, 900]

# ============================================================
# CONFIGURACIÓN DE PERIODOS
# ------------------------------------------------------------
# IMPORTANTE: las anclas son ENTIDADES POLÍTICAS ESPECÍFICAS que NO
# compiten en el periodo. Esto evita dos problemas:
#   1. Saturación por la propia carrera (que pasaría si la ancla fuera
#      un candidato del periodo, ej. Cepeda saturando si Valencia sube)
#   2. Bot detection de Google (que activan queries genéricas tipo
#      "elecciones presidenciales Colombia")
#
# Las anclas elegidas son presidentes en funciones o salientes:
#   • 2026 PV     → Petro (presidente actual, NO candidato)
#   • 2018        → Juan Manuel Santos (presidente saliente en 2018)
#   • 2022        → Iván Duque (presidente saliente en 2022)
#   • Consulta    → Petro (no participa en consulta PorColombia)
#
# Los nombres de keywords aquí deben coincidir EXACTAMENTE con los
# del 00_señales_externas.R (mapa keyword → código).
# ============================================================
PERIODOS_CONFIG = {
    # ---------------------------------------------------------------
    # PRESIDENCIALES 2026 — 12 candidatos
    # ---------------------------------------------------------------
    "pv_2026": {
        "timeframe": "2026-01-01 2026-05-24",
        "ancla_kw":  "Gustavo Petro",   # presidente actual, no compite
        "candidatos": {
            # codigo_R         → keyword Trends
            "Cepeda"        : "Ivan Cepeda",
            "ClaudiaLopez"  : "Claudia Lopez",
            "DeLaEspriella" : "Abelardo De La Espriella",
            "Barreras"      : "Roy Barreras",
            "Botero"        : "Santiago Botero",
            "Caicedo"       : "Carlos Caicedo",
            "Fajardo"       : "Sergio Fajardo",
            "Macollins"     : "Sondra Macollins",    # AJUSTAR si Trends da basura
            "Lizcano"       : "Mauricio Lizcano",
            "Matamoros"     : "Gustavo Matamoros",
            "Uribe"         : "Miguel Uribe",
            "Valencia"      : "Paloma Valencia",
        },
    },

    # ---------------------------------------------------------------
    # HISTÓRICO 2018 — 8 candidatos (1ra vuelta: 27 may 2018)
    # ---------------------------------------------------------------
    "hist_2018": {
        "timeframe": "2018-01-01 2018-05-27",
        "ancla_kw":  "Juan Manuel Santos",   # presidente saliente, no candidato
        "candidatos": {
            "Petro"             : "Gustavo Petro",
            "Duque"             : "Ivan Duque",
            "DeLaCalle"         : "Humberto de la Calle",
            "TrujilloSarmiento" : "Jorge Trujillo Sarmiento",  # corregí 'Jurge'
            "Fajardo"           : "Sergio Fajardo",
            "VivianeMorales"    : "Viviane Morales",
            "PiedadCordoba"     : "Piedad Cordoba",
            "VargasLleras"      : "German Vargas Lleras",
        },
    },

    # ---------------------------------------------------------------
    # HISTÓRICO 2022 — 8 candidatos (1ra vuelta: 29 may 2022)
    # ---------------------------------------------------------------
    "hist_2022": {
        "timeframe": "2022-01-01 2022-05-29",
        "ancla_kw":  "Ivan Duque",           # presidente saliente, no candidato
        "candidatos": {
            "Petro"               : "Gustavo Petro",
            "Hernandez"           : "Rodolfo Hernandez",
            "JohnMiltonRodriguez" : "John Milton Rodriguez",
            "FedericoGutierrez"   : "Federico Gutierrez",
            "Fajardo"             : "Sergio Fajardo",
            "EnriqueGomez"        : "Enrique Gomez",
            "LuisPerez"           : "Luis Perez",
            "IngridBetancourt"    : "Ingrid Betancourt",
        },
    },

    # ---------------------------------------------------------------
    # CONSULTA PORCOLOMBIA 2026 — 9 candidatos (consulta: 8 mar 2026)
    # NOTA: si el usuario sabe los nombres completos exactos, AJUSTAR
    # las queries para mejorar la precisión de Trends.
    # ---------------------------------------------------------------
    "consulta_2026": {
        "timeframe": "2026-01-01 2026-03-08",
        "ancla_kw":  "Gustavo Petro",        # no participa en la consulta
        "candidatos": {
            "Valencia" : "Paloma Valencia",
            "Oviedo"   : "Oviedo PorColombia",        # AJUSTAR nombre completo
            "Galan"    : "Juan Manuel Galan",
            "Pinzon"   : "Pinzon PorColombia",        # AJUSTAR
            "Davila"   : "Vicky Davila",
            "Penalosa" : "Enrique Penalosa",
            "Gaviria"  : "Gaviria PorColombia",       # AJUSTAR
            "Luna"     : "Luna PorColombia",          # AJUSTAR
            "Cardenas" : "Mauricio Cardenas",
        },
    },
}


# ============================================================
# FORMACIÓN DE GRUPOS (≤5 keywords cada uno, todos con ancla)
# ============================================================
def formar_grupos(candidatos_dict, ancla_kw, max_per_group=MAX_KW_GRUPO):
    """
    Divide candidatos en grupos de ≤max_per_group keywords. Cada grupo
    incluye el ancla (externa) al inicio para poder re-escalar después.
    El ancla NO está en candidatos_dict — se prepende aquí.
    """
    all_kws = list(candidatos_dict.values())
    grupos = []
    step = max_per_group - 1   # 4 candidatos + 1 ancla = 5
    for i in range(0, len(all_kws), step):
        grupo = [ancla_kw] + all_kws[i : i + step]
        grupos.append(grupo)
    return grupos


# Headers de navegador real para reducir detección de bot
HEADERS_NAVEGADOR = {
    "User-Agent": (
        "Mozilla/5.0 (X11; Linux x86_64; rv:120.0) "
        "Gecko/20100101 Firefox/120.0"
    ),
    "Accept": "application/json, text/plain, */*",
    "Accept-Language": "es-CO,es;q=0.9,en;q=0.5",
    "Accept-Encoding": "gzip, deflate, br",
    "DNT": "1",
    "Connection": "keep-alive",
}


def crear_pytrends():
    """
    Crea sesión TrendReq con headers de navegador real.
    NOTA: NO pasamos `retries`/`backoff_factor` porque pytrends usa
    internamente `method_whitelist` (param removido en urllib3 v2) y
    rompe. El retry lo hacemos nosotros en el for loop de descargar_grupo.
    """
    return TrendReq(
        hl="es-CO",
        tz=300,
        timeout=(10, 30),
        requests_args={"headers": HEADERS_NAVEGADOR, "verify": True},
    )


# ============================================================
# DESCARGA DE UN GRUPO CON REINTENTOS
# ============================================================
def descargar_grupo(keywords, timeframe, max_intentos=6):
    """
    Descarga un grupo. Retorna DataFrame [date, keyword, hits] o None.
    Reutiliza la sesión TrendReq entre intentos para mantener cookies
    (recrear sesión cada intento es un trigger clásico de 429).
    """
    pytrends = crear_pytrends()  # UNA sesión, reusada en todos los intentos

    for intento in range(1, max_intentos + 1):
        try:
            print(f"    Intento {intento}/{max_intentos}...", flush=True)
            pytrends.build_payload(
                kw_list=keywords,
                cat=CAT,
                timeframe=timeframe,
                geo=GEO,
                gprop="",
            )
            df = pytrends.interest_over_time()

            if df is None or df.empty:
                print("    Respuesta vacía — Google no tiene datos para ese periodo/cat.")
                return None

            df = df.drop(columns=["isPartial"], errors="ignore").reset_index()
            df_long = df.melt(id_vars="date", var_name="keyword", value_name="hits")
            df_long["hits"] = pd.to_numeric(df_long["hits"], errors="coerce").fillna(0)

            print(
                f"    OK: {len(df_long)} filas | {df_long['keyword'].nunique()} kw | "
                f"{df_long['date'].min().date()} → {df_long['date'].max().date()}"
            )
            return df_long

        except Exception as e:
            msg = str(e)
            es_429 = "429" in msg or "too many" in msg.lower()
            tipo = "(rate limit 429)" if es_429 else "(otro error)"
            print(f"    Intento {intento} fallido {tipo}: {msg[:140]}")

            # Después de 3 fallos seguidos, resetear sesión por si las cookies
            # quedaron en mal estado
            if intento == 3:
                print("    Reseteando sesión pytrends...")
                pytrends = crear_pytrends()

            if intento < max_intentos:
                espera = RETRY_DELAYS[min(intento - 1, len(RETRY_DELAYS) - 1)]
                print(f"    Esperando {espera}s antes de reintentar...", flush=True)
                time.sleep(espera)

    return None


# ============================================================
# DESCARGAR UN PERIODO COMPLETO
# ============================================================
def descargar_periodo(periodo_name, config, force=False):
    """
    Descarga todos los grupos de un periodo. Devuelve True si al menos
    un grupo se descargó exitosamente.
    """
    grupos = formar_grupos(config["candidatos"], config["ancla_kw"])
    print(f"  Total grupos: {len(grupos)} (ancla: '{config['ancla_kw']}')")

    n_exitosos = 0

    for i, grupo in enumerate(grupos, 1):
        salida = OUTPUT_DIR / f"trends_{periodo_name}_g{i}.csv"

        if salida.exists() and not force:
            print(f"\n  ⏭  Grupo {i}: {salida.name} ya existe — saltando "
                  f"(usa --force para re-descargar)")
            n_exitosos += 1
            continue

        print(f"\n  Grupo {i} ({len(grupo)} kw): {' | '.join(grupo)}")
        df = descargar_grupo(grupo, config["timeframe"])

        if df is not None:
            df.to_csv(salida, index=False)
            print(f"  ✓ Guardado: {salida.name}")
            n_exitosos += 1
        else:
            print(f"  ✗ Falló grupo {i} tras todos los reintentos")

        # Pausa entre grupos (salvo el último)
        if i < len(grupos):
            print(f"  --- Pausa {PAUSA_ENTRE_GRUPOS}s ---", flush=True)
            time.sleep(PAUSA_ENTRE_GRUPOS)

    return n_exitosos > 0


# ============================================================
# MAIN
# ============================================================
def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument(
        "--periodo",
        choices=list(PERIODOS_CONFIG.keys()) + ["todos"],
        default="todos",
        help="Cuál periodo descargar (default: todos)",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Re-descarga incluso si el CSV ya existe",
    )
    args = parser.parse_args()

    periodos_a_correr = (
        list(PERIODOS_CONFIG.keys()) if args.periodo == "todos" else [args.periodo]
    )

    print("=" * 70)
    print("fetch_trends.py — Trends Colombia 2026")
    print("=" * 70)
    print(f"Periodos    : {', '.join(periodos_a_correr)}")
    print(f"Geo         : {GEO}")
    print(f"Categoría   : {CAT} (396=Politics, 0=All)")
    print(f"Output      : {OUTPUT_DIR}")
    print(f"Force re-dl : {args.force}")
    print()

    resumen = {}

    for i, periodo in enumerate(periodos_a_correr):
        print(f"\n{'=' * 70}")
        print(f"PERIODO: {periodo}")
        print(f"  Timeframe: {PERIODOS_CONFIG[periodo]['timeframe']}")
        print(f"  Ancla:     {PERIODOS_CONFIG[periodo]['ancla_kw']}")
        print(f"{'=' * 70}")

        ok = descargar_periodo(periodo, PERIODOS_CONFIG[periodo], force=args.force)
        resumen[periodo] = "✓" if ok else "✗"

        if i < len(periodos_a_correr) - 1:
            print(f"\n--- Pausa entre periodos: {PAUSA_ENTRE_PERIODOS}s ---", flush=True)
            time.sleep(PAUSA_ENTRE_PERIODOS)

    # ---- Resumen final ----
    print(f"\n{'=' * 70}")
    print("RESUMEN")
    print("=" * 70)
    for p, status in resumen.items():
        print(f"  {status} {p}")
    print()
    print("Procesamiento en R:")
    print("    Rscript 00_señales_externas.R   # normaliza con ancla → RDS")
    print("    Rscript 07_analisis_trends.R    # análisis de estrategias")
    print("=" * 70)


if __name__ == "__main__":
    main()
