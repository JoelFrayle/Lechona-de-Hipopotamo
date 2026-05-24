# Declaración de Uso de Inteligencia Artificial

**Herramienta:** Claude Sonnet 4.6 (Anthropic)

**Rol en el proyecto:**
Asistencia en desarrollo e implementación del código en R y Stan.
La herramienta ayudó a escribir, depurar y optimizar los scripts.

**Lo que NO hizo la IA:**
- El pronóstico **no fue generado directamente por el LLM**.
- Las decisiones metodológicas (arquitectura del modelo MCMC, elección de priors,
  integración de Google Trends, diseño del backtest histórico 2018+2022)
  fueron tomadas por el autor.
- La evaluación de los resultados y la selección del modelo final
  fueron realizadas por el autor.

**Lo que SÍ hizo la IA:**
- Traducir decisiones metodológicas a código R/Stan funcional
- Identificar y corregir bugs durante el desarrollo
- Sugerir implementaciones de funciones auxiliares

El modelo estadístico subyacente (Stan, MCMC, Dirichlet-Multinomial)
es ejecutado localmente por el autor, no por el LLM.
