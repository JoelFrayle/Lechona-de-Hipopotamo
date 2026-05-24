# Lechona de Hipopotamo
## Concurso Recetas Electorales 2026 — Primera Vuelta

**Cocinero:** Joel Frayle Moreno  
**Contacto:** joel.frayle13@gmail.com  
**Fecha pronóstico:** 24 de mayo de 2026  

---

## Método

Modelo bayesiano de intención de voto estimado con MCMC (CmdStan/Stan),
cuyo núcleo es un **filtro de Kalman en espacio de estados** que controla
el ruido de medición y separa la señal latente del voto de las fluctuaciones
encuesta a encuesta.

**Ingredientes:**
- Encuestas presidenciales 2026 (`datos/encuestas_presidenciales_2026.csv`)
- Señal de Google Trends (`codigo/fetch_trends.py`)

**Cocina — tres capas:**

**1. Filtro de Kalman bayesiano sobre encuestas**  
El estado latente φ[t] (intención de voto en escala log-ratio) evoluciona
como una caminata aleatoria con velocidad estocástica. El filtro Kalman
separa la señal real del ruido de muestreo, la sobre-representación de
casas encuestadoras y el efecto de diseño muestral. Las observaciones son
conteos Dirichlet-Multinomial que acomodan la sobredispersión natural de
las encuestas. La inferencia es completamente bayesiana via MCMC (HMC-NUTS).

**2. Filtro de Kalman multi-escala sobre Google Trends**  
La señal de búsquedas en Google se modela con un estado latente de doble
drift AR(1): uno de vida media ~2 días (ruido rápido) y uno de vida media
~7 días (tendencia lenta). Esto permite que el modelo capture el momentum
real de interés en cada candidato sin confundirlo con picos de un día.
Calibrado por máxima verosimilitud aproximada (LFO-CV).

**3. Integración por momentum**  
El delta log entre la ventana reciente y la anterior de Trends (centrado
para suma cero entre candidatos) se aplica al logit del estado latente
con un factor de integración calibrado via **backtest contra 2018 y 2022**,
minimizando el error cuadrático medio sobre ambas elecciones históricas.

## Reproducibilidad

```bash
python3 codigo/fetch_trends.py          # descargar Trends (opcional)
Rscript codigo/00_senales_externas.R    # normalizar señal
Rscript codigo/concurso_lechona.R       # modelo + pronóstico
```

**Requisitos:** R ≥ 4.3, CmdStan ≥ 2.33, cmdstanr, dplyr, tidyr, ggplot2, posterior

Para detalles del proceso de calibración y backtest: **joel.frayle13@gmail.com**

## Pronóstico

Ver `outputs/prediccion_lechona_concurso.csv`

## Declaración de uso de IA

Ver `DECLARACION_IA.md`
