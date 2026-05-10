# RegFDC-Cauca v1.0.0

**Regionalización de Curvas de Duración de Caudales mediante el Método del Índice de Caudal con Memoria de Larga Dependencia**

Sistema hidrológico del río Cauca — Andes colombianos

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R ≥ 4.2](https://img.shields.io/badge/R-%E2%89%A54.2-blue.svg)](https://www.r-project.org/)
[![Version](https://img.shields.io/badge/version-1.0.0-green.svg)](https://github.com/MauricioVictoriaN/RegFDC-Cauca/releases)
[![DOI](https://img.shields.io/badge/preprint-EngrXiv-orange.svg)](https://engrxiv.org)

---

## Descripción

RegFDC-Cauca v1.0.0 es un marco computacional de código abierto en R para estimar curvas de duración de caudales (FDC) en cuencas sin datos del sistema hidrológico del río Cauca (departamentos de Valle del Cauca, Cauca, Risaralda y Quindío, Colombia). Implementa el **método del índice de caudal** con tres avances metodológicos complementarios:

1. **Clustering Ward D2** sobre atributos físicos de cuenca ponderados con metadatos cualitativos (régimen hidrológico y subregión climática), con restricción mínima WMO de ≥ 5 cuencas por región.
2. **Exponente de Hurst (R/S)** para detección de memoria de larga dependencia y selección automática entre **ARFIMA(0,d,0)** y AR(1) para la generación de series sintéticas.
3. **Modelo log-log de caudal medio** con selección de predictores por BIC (`MASS::stepAIC`), diagnóstico de influencia (distancia de Cook) e intervalos de predicción con corrección de Jensen.

La validación mediante **validación cruzada leave-one-out (LOO)** sobre 20 cuencas instrumentadas produce NSE = 0.97, KGE' = 0.96 y PBIAS = 0.3 % para la FDC adimensional, con MAPE < 5 % en el rango 0.05 ≤ F ≤ 0.85.

---


## Instalación rápida

### Prerrequisitos

- **R ≥ 4.2.0** — [Descargar R](https://cloud.r-project.org/)
- **RStudio** (recomendado) — [Descargar RStudio](https://posit.co/download/rstudio-desktop/)
- Los paquetes se instalan automáticamente al ejecutar el script.

### Paquetes R requeridos

| Paquete | Versión mín. | Uso |
|---|---|---|
| `tidyverse` | 1.3 | Manipulación de datos y visualización |
| `readxl` | 1.4 | Lectura del archivo Excel de entrada |
| `writexl` | 1.4 | Exportación de resultados a Excel |
| `cluster` | 2.1 | Clustering Ward D2 y silhouette |
| `viridis` | 0.6 | Paletas de color accesibles |
| `lubridate` | 1.9 | Manejo de fechas |
| `zoo` | 1.8 | Series temporales |
| `splines` | — | Spline cúbico monótono (Hyman) para FDC |
| `boot` | 1.3 | Bootstrap para IC de métricas LOO |
| `patchwork` | 1.1 | Composición de gráficos ggplot2 |
| `fracdiff` | 1.5 | ARFIMA — parámetro d fraccionario |
| `kSamples` | 1.2 | Test Anderson-Darling k-muestras |
| `MASS` | 7.3 | `stepAIC` (BIC) y `ginv` — se carga por `requireNamespace` |

### Pasos de ejecución

```r
# 1. Clonar el repositorio
#    git clone https://github.com/MauricioVictoriaN/RegFDC-Cauca.git

# 2. Abrir RegFDC_Cauca_1.0.0.R en RStudio

# 3. Ajustar la ruta al archivo de datos en la Sección 0 (línea ~75):
RUTA_EXCEL <- "RegFDC_Cauca_1.0.0_datos.xlsx"   # ruta relativa al directorio de trabajo
# o ruta absoluta, ej.: "C:/Proyectos/RegFDC-Cauca/RegFDC_Cauca_1.0.0_datos.xlsx"

# 4. Ejecutar todo el script (Ctrl+Shift+Enter en RStudio)
#    Los paquetes faltantes se instalan automáticamente.
```

El script detecta e instala los paquetes faltantes en la primera ejecución (requiere conexión a internet). Las ejecuciones posteriores no reinstalan paquetes existentes.

---

## Archivo de datos de entrada

**`RegFDC_Cauca_1.0.0_datos.xlsx`** contiene 5 hojas:

| Hoja | Contenido | Filas |
|---|---|---|
| `caudales_diarios` | Series de caudal diario (m³/s) para las 20 cuencas instrumentadas (C001–C020), período 2015-01-01 a 2019-12-31 | 1 828 (1 826 datos + 2 encabezados) |
| `atributos_cuencas` | Atributos físicos y derivados de las 28 cuencas (20 observadas + 8 a estimar) | 29 |
| `metadatos_estaciones` | Coordenadas, subregión climática y tipo de régimen de las 20 estaciones aforadas | 21 |
| `configuracion` | Parámetros configurables del análisis (25 parámetros) | 26 |
| `notas_metodologicas` | Documentación de fuentes de datos y metodología | — |

### Atributos de cuenca (`atributos_cuencas`)

| Variable | Unidad | Fuente |
|---|---|---|
| `area_km2` | km² | SIG / DEM SRTM 30 m |
| `pendiente_media_pct` | % | DEM SRTM 30 m |
| `precipitacion_anual_mm` | mm | CHIRPS v2.0 |
| `evapotranspiracion_anual_mm` | mm | MODIS MOD16A2 |
| `cn_promedio` | adim. | FAO Digital Soil Map + MODIS MCD12Q1 + tablas NRCS |
| `precipitacion_efectiva_mm` | mm | E − 0.4 × ETP |
| `retencion_maxima_mm` | mm | SCS: (25 400/CN) − 254 |
| `escorrentia_anual_mm` | mm | SCS: (P − 0.2S)² / (P + 0.8S) |
| `coeficiente_escorrentia` | adim. | escorrentía / precipitación |
| `indice_humedad` | adim. | precipitación efectiva / ETP |

---

## Parámetros de configuración clave

Todos los parámetros se leen desde la hoja `configuracion` del archivo Excel. Los más importantes para adaptar el análisis a otra región son:

| Parámetro | Valor por defecto | Descripción |
|---|---|---|
| `num_regiones` | 3 | Número de regiones hidrológicas (Ward D2) |
| `umbral_hurst_arfima` | 0.60 | H mínimo para usar ARFIMA en lugar de AR(1) |
| `umbral_r2_mu` | 0.50 | R²adj mínimo aceptable del modelo de caudal medio |
| `umbral_min_region` | 5 | Cuencas mínimas por región (criterio OMM) |
| `n_bootstrap` | 500 | Réplicas bootstrap para IC de métricas LOO |
| `semilla_aleatoria` | 12 345 | Semilla global para reproducibilidad completa |
| `usar_metadatos_clustering` | TRUE | Integra tipo de régimen y subregión al clustering |
| `peso_metadatos_clustering` | 0.5 | Peso relativo de metadatos en Ward D2 (0–1) |

---

## Salidas del análisis

El script genera automáticamente el directorio `resultados/` con:

### Libro Excel de resultados (`RegFDC_Cauca_resultados.xlsx`, 14 hojas)

| Hoja | Contenido |
|---|---|
| `FDC_regional` | FDC adimensional promedio por región (q, sd, p10, p90) |
| `FDC_sinteticas` | FDC adimensional estimada por cuenca sin datos |
| `FDC_dimensional` | FDC en m³/s con IC 90% para cuencas sin datos |
| `Asignacion` | Región asignada, μ estimado e IC 90%, H y d regional |
| `Calidad_registros` | n datos, φ AR(1), H, d, μ y q específica por cuenca |
| `Clustering` | Región Ward D2 y atributos por cuenca instrumentada |
| `AD_homogeneidad` | p-valor Anderson-Darling por región |
| `Extrapolacion` | Cuencas fuera del espacio de interpolación (si las hay) |
| `Diag_corr_mu` | Correlación r(log predictor, log μ) por atributo |
| `Modelo_mu` | Coeficientes BIC del modelo de caudal medio |
| `Metricas_LOO` | NSE, KGE', PBIAS, RMSE, MAE, MAPE, r Spearman globales |
| `Metricas_segmento` | Métricas por segmento (caudales altos, medios, bajos) |
| `Error_por_F` | MAE, MAPE y NSE por punto de frecuencia de excedencia |
| `Bootstrap_IC95` | IC 95% bootstrap de NSE, KGE' y PBIAS |

### Gráficos (directorio `resultados/graficos/`)

| Archivo | Contenido |
|---|---|
| `G1_FDC_dimensionales.png` | FDC en m³/s con IC 90% para las 8 cuencas sin datos |
| `G2_FDC_regionales.png` | FDC adimensionales regionales con banda p10–p90 |
| `G3_Error_LOO_frecuencia.png` | MAPE y NSE a lo largo de la FDC (validación LOO) |
| `G4_QQ_regional.png` | Diagramas Q-Q adimensionales LOO por región |
| `G5_Dendrograma_Ward.png` | Dendrograma Ward D2 y curva de silhouette |
| `G6_Serie_C021.png` | Serie sintética diaria de ejemplo (C021) |
| `G7_Diagnostico_mu.png` | Residuos vs fitted y distancia de Cook del modelo μ |
| `G8_Escorrentia_especifica.png` | Escorrentía específica observada (L/s/km²) |
| `G9_Hurst_cuencas.png` | Exponente de Hurst por cuenca y umbral ARFIMA |

### Informes de texto

- **`A_diagnostico_tecnico.txt`** — Correlaciones μ~atributos, extrapolación, clustering, modelo μ y Hurst por cuenca.
- **`B_resultados_y_aplicabilidad.txt`** — Caudales estimados, series sintéticas, métricas LOO, calificación global y guía de uso por segmento de FDC.

### Series sintéticas CSV

- **`series_sinteticas_todas.csv`** — Todas las cuencas sin datos en un único archivo largo.
- **`series_sinteticas/C0XX.csv`** — Un CSV por cuenca (columnas: `fecha`, `caudal_m3s`).

---

## Región de estudio y cuencas

### Cuencas instrumentadas (calibración y validación LOO)

| ID | Río / Estación | Subregión | Área (km²) | P (mm/año) |
|---|---|---|---|---|
| C001 | Otún en Dosquebradas | Andina-Norte | 480 | 2 450 |
| C002 | San Juan en Bolívar | Andina-Norte | 1 150 | 3 820 |
| C003 | Risaralda en Arauca | Andina-Norte | 890 | 2 980 |
| C004 | La Vieja en Cartago | Andina-Norte | 2 870 | 2 260 |
| C005 | Frío en Belalcázar | Andina-Norte | 620 | 3 140 |
| C006 | Quinchía en Irra | Andina-Norte | 340 | 2 680 |
| C007 | Amaime en Miranda | Andina-Centro | 760 | 1 840 |
| C008 | Tuluá en Monteloro | Andina-Centro | 1 280 | 1 950 |
| C009 | Nima en La Tulia | Andina-Centro | 390 | 1 760 |
| C010 | Bolo en Pradera | Andina-Centro | 540 | 1 820 |
| C011 | Morales en Zarzal | Andina-Centro | 580 | 2 120 |
| C012 | Palo en Caloto | Andina-Centro | 1 640 | 1 680 |
| C013 | Guachal en Buga | Andina-Centro | 720 | 1 920 |
| C014 | Ovejas en Santander Q. | Andina-Centro | 2 110 | 1 590 |
| C015 | Timba en Suárez | Andina-Sur | 1 870 | 2 940 |
| C016 | Frayle en Florida | Andina-Centro | 470 | 1 780 |
| C017 | Desbaratado en Corinto | Andina-Centro | 830 | 1 640 |
| C018 | Paila en La Paila | Andina-Centro | 420 | 2 050 |
| C019 | Cauca en La Bolsa | Andina-Sur | 18 900 | 2 180 |
| C020 | San Jorge (trib. Cauca) | Andina-Sur | 1 360 | 3 280 |

### Cuencas sin datos (estimación)

| ID | Río | Subregión | Área (km²) | μ̂ (m³/s) | IC 90% |
|---|---|---|---|---|---|
| C021 | Pescador en Roldanillo | Andina-Centro | 680 | 37.5 | [28.1, 50.0] |
| C022 | Guengüé en Ginebra | Andina-Centro | 310 | 13.2 | [9.9, 17.6] |
| C023 | Piedras en Popayán | Andina-Sur | 940 | 68.4 | [47.3, 98.8] |
| C024 | Bugalagrande | Andina-Centro | 1 120 | 49.1 | [36.8, 65.5] |
| C025 | Riofrío en Riofrío | Andina-Centro | 480 | 18.9 | [14.2, 25.2] |
| C026 | Dovio en El Dovio | Andina-Norte | 290 | 27.6 | [18.9, 40.3] |
| C027 | Dagua alto en Loboguerrero | Andina-Centro | 620 | 42.1 | [29.9, 59.3] |
| C028 | Anchicayá alto | Andina-Centro | 480 | 71.3 | [48.8, 104.2] |

---

## Metodología

```
Datos de entrada (Excel)
        │
        ▼
[S0] Configuración ──────────────────────────────────────────────────────────┐
        │                                                                     │
        ▼                                                                     │
[S1] Carga y validación                                                      │
        │                                                                     │
        ▼                                                                     │
[S2] FDC adimensional + Hurst (R/S) por cuenca                               │
        │   • Spline Hyman sobre posiciones de Weibull                        │
        │   • H estimado por regresión log-log de R/S                         │
        │   • d = max(0, H − 0.5)                                             │
        ▼                                                                     │
[S3] Diagnóstico r(μ, atributos)                                              │
        │   • Alerta si r_max < umbral (0.40)                                 │
        ▼                                                                     │
[S4] Clustering Ward D2                                                       │
        │   • Atributos físicos + metadatos ponderados (peso = 0.5)           │
        │   • k óptimo por silhouette; restricción ≥ 5 cuencas/región (OMM)  │
        │   • Test homogeneidad Anderson-Darling k-muestras                   │
        ▼                                                                     │
[S5] Detección de extrapolación (espacio de interpolación observado)          │
        ▼                                                                     │
[S6] Asignación cuencas sin datos → región (distancia Mahalanobis)           │
        ▼                                                                     │
[S7] Modelo log-log μ                                                         │
        │   • Selección predictores por BIC (MASS::stepAIC)                   │
        │   • Diagnóstico: residuos + distancia de Cook                        │
        │   • IC 90% con corrección de Jensen                                  │
        ▼                                                                     │
[S8] Generación series sintéticas                                              │
        │   • ARFIMA(0,d,0) si H > umbral, AR(1) si no                        │
        │   • Transformación normal-cuantil preserva FDC                       │
        ▼                                                                     │
[S9] Validación cruzada LOO + bootstrap                                        │
        │   • NSE, KGE', PBIAS, RMSE, MAE, MAPE, r Spearman                  │
        │   • Por segmento: caudales altos / medios / bajos                   │
        ▼                                                                     │
[S10] Gráficos (G1–G9) + Excel 14 hojas + 2 informes TXT + CSV ─────────────┘
```

---

## Resultados de validación

| Métrica | Global | Altos (F < 0.20) | Medios (0.20–0.80) | Bajos (F > 0.80) |
|---|---|---|---|---|
| **NSE** | **0.97** | 0.95 | 0.98 | 0.93 |
| **KGE'** | **0.96** | 0.94 | 0.97 | 0.91 |
| **PBIAS (%)** | **0.3** | 1.2 | 0.1 | −1.8 |
| MAPE (%) | 3.8 | 4.9 | 2.6 | 6.7 |
| r Spearman | 0.993 | 0.989 | 0.996 | 0.981 |

IC 95 % bootstrap (B = 500): NSE [0.95, 0.98] · KGE' [0.94, 0.97] · PBIAS [−2.1 %, 2.7 %]

Todas las cuencas presentan H ∈ [0.86, 0.91] → **100 % usan ARFIMA(0,d,0)**.

---

## Rango de aplicabilidad

El marco produce estimaciones con MAPE esperado **< 7 %** en el rango F ∈ [0.05, 0.85] para cuencas del sistema Cauca con:

- Área de drenaje: **300–3 000 km²**
- Precipitación media anual: **1 600–4 100 mm**
- Régimen hidrológico: **bimodal** (1°N–6°N, interior andino colombiano)

Para cuencas fuera de estos rangos, el script activa la advertencia de extrapolación (Sección 5) y reporta el atributo que excede el espacio observado.

---

## Citar este trabajo

**Cita recomendada (formato APA):**

> Victoria Niño, M. J. (2026). *RegFDC-Cauca v1.0.0: Regionalización de curvas de duración de caudales mediante el método del índice de caudal con memoria de larga dependencia en el sistema hidrológico del río Cauca, Colombia*. EngrXiv. https://github.com/MauricioVictoriaN/RegFDC-Cauca

**Entrada BibTeX:**

```bibtex
@misc{victoria2026regfdc,
  author    = {Victoria Niño, Mauricio Javier},
  title     = {{RegFDC-Cauca v1.0.0}: Regionalización de Curvas de Duración
               de Caudales mediante el Método del Índice de Caudal con Memoria
               de Larga Dependencia en el Sistema Hidrológico del Río Cauca,
               Colombia},
  year      = {2026},
  publisher = {EngrXiv},
  note      = {Preprint. \url{https://github.com/MauricioVictoriaN/RegFDC-Cauca}},
  url       = {https://github.com/MauricioVictoriaN/RegFDC-Cauca}
}
```

---

## Referencias clave

- Castellarin A. et al. (2004). Regional flow-duration curves: reliability for ungauged basins. *Advances in Water Resources*, 27(10), 953–965. https://doi.org/10.1016/j.advwatres.2004.08.005
- Hurst H.E. (1951). Long-term storage capacity of reservoirs. *Trans. Am. Soc. Civil Eng.*, 116, 770–799.
- Hosking J.R.M. (1981). Fractional differencing. *Biometrika*, 68(1), 165–176. https://doi.org/10.1093/biomet/68.1.165
- Gupta H.V., Kling H., Yilmaz K.K. & Martinez G.F. (2009). Decomposition of the mean squared error and NSE: Implications for improving hydrological modelling. *Journal of Hydrology*, 377(1–2), 80–91. https://doi.org/10.1016/j.jhydrol.2009.08.003
- Kling H., Fuchs M. & Paulin M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. *Journal of Hydrology*, 424–425, 264–277. https://doi.org/10.1016/j.jhydrol.2012.01.011
- Poveda G., Jaramillo L. & Vallejo L.F. (2014). Seasonal precipitation patterns along pathways of the South American Low-Level Jet and aerial rivers. *Water Resources Research*, 50(1), 98–118. https://doi.org/10.1002/2013WR014087
- IDEAM (2019). *Estudio Nacional del Agua 2018*. Bogotá, Colombia.

---

## Licencia

Este proyecto está bajo la licencia **MIT** — ver el archivo [LICENSE](LICENSE) para más detalles.

---

## Contacto

**Mauricio Javier Victoria Niño**
Investigador Independiente — Cali, Colombia
📧 hidratecsa@gmail.com
🔗 ORCID: [0009-0003-4328-5691](https://orcid.org/0009-0003-4328-5691)

---

<p align="center">
  <em>Sistema hidrológico del río Cauca · Andes colombianos · RegFDC-Cauca v1.0.0</em>
</p>
