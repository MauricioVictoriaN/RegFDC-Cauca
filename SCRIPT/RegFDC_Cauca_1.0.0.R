# =============================================================================
# REGIONALIZACIÓN DE CURVAS DE DURACIÓN DE CAUDALES (FDC)
# Método del Índice de Caudal con Regionalización Hidrológica
#
# Región de estudio : Sistema hidrológico río Cauca — Andes colombianos
#                     (Departamentos: Valle del Cauca, Cauca, Risaralda, Quindío)
# Régimen           : Bimodal (húmedos: mar–may y sep–nov; secos: jun–ago y dic–feb)
#
# Descripción del método
# ──────────────────────
# 1. Cálculo de FDC adimensional por cuenca (spline Hyman sobre distribución
#    empírica de Weibull) con estimación del exponente de Hurst (R/S).
# 2. Diagnóstico previo de correlación μ ~ atributos fisiográficos.
# 3. Regionalización Ward D2 sobre atributos físicos + metadatos cualitativos
#    (régimen hidrológico, subregión climática); restricción mínima WMO.
# 4. Test de homogeneidad Anderson-Darling k-muestras por región.
# 5. Detección de cuencas fuera del espacio convex-hull de las observadas.
# 6. Asignación de cuencas sin datos por distancia de Mahalanobis.
# 7. Modelo de caudal medio (log-log) con selección BIC (MASS::stepAIC) y
#    diagnóstico de residuos + distancia de Cook.
# 8. Generación de series sintéticas: ARFIMA(0,d,0) cuando H > umbral,
#    AR(1) en caso contrario; transformación normal-cuantil preserva la FDC.
# 9. Validación cruzada Leave-One-Out con bootstrap (NSE, KGE, PBIAS).
# 10. Exportación: 14 hojas Excel, 9 gráficos, 2 informes de texto,
#     series diarias CSV por cuenca.
#
# Archivo de entrada: RegFDC_Cauca_1.0.0_datos.xlsx
#
# Referencias
# ───────────
# Castellarin A. et al. (2004). Regional FDC: reliability for ungauged basins.
#   Advances in Water Resources, 27(10), 953–965.
# Gupta H. et al. (2009). Decomposition of the mean squared error and NSE.
#   Journal of Hydrology, 377, 80–91.
# Hosking J.R.M. (1981). Fractional differencing. Biometrika, 68(1), 165–176.
# Hurst H.E. (1951). Long-term storage capacity of reservoirs.
#   Trans. Am. Soc. Civil Eng., 116, 770–799.
# IDEAM (2019). Estudio Nacional del Agua. Bogotá, Colombia.
# Kling H. et al. (2012). Runoff conditions in the upper Danube basin.
#   Journal of Hydrology, 440–441, 192–201.
# Poveda G. et al. (2011). Hydroclimatology of Colombia.
#   Rev. Acad. Colomb. Cienc. Exact., 35(135), 317–336.
# Scholz F.W. & Stephens M.A. (1987). K-sample Anderson-Darling tests.
#   Journal of the American Statistical Association, 82, 918–924.
# SCS (1972). National Engineering Handbook, Section 4. USDA, Washington.
#
# Versión : 1.0.0
# Fecha   : 2026-05-05
# =============================================================================


# -----------------------------------------------------------------------------
# SECCIÓN 0 · CONFIGURACIÓN INICIAL
# -----------------------------------------------------------------------------

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  REGIONALIZACIÓN FDC v1.0.0 — Sistema Río Cauca, Colombia      ║\n")
cat("║  ARFIMA · Hurst · AD-test · Diagnóstico μ · Extrapolación       ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

# Operador nulo-coalesce -------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ► AJUSTE: ruta al archivo de datos antes de ejecutar ◄ ----------------------
RUTA_EXCEL <- "D:/R/Curva CCC/RegFDC_Cauca_1.0.0_datos.xlsx"

# Paquetes --------------------------------------------------------------------
paquetes_requeridos <- c(
  "tidyverse",   # manipulación y visualización
  "readxl",      # lectura Excel
  "writexl",     # escritura Excel
  "cluster",     # silhouette, Ward
  "viridis",     # paletas de color
  "lubridate",   # manejo de fechas
  "zoo",         # series temporales
  "splines",     # splines para FDC
  "boot",        # bootstrap
  "patchwork",   # composición de gráficos ggplot2
  "fracdiff",    # ARFIMA — parámetro d fraccionario
  "kSamples"     # test Anderson-Darling k-muestras
)

# MASS se carga por requireNamespace para evitar que MASS::select()
# enmascare dplyr::select(); se invoca siempre como MASS::función().
if (!requireNamespace("MASS", quietly = TRUE))
  install.packages("MASS", repos = "https://cloud.r-project.org")

paquetes_faltantes <- paquetes_requeridos[
  !sapply(paquetes_requeridos, requireNamespace, quietly = TRUE)
]
if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ",
          paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, repos = "https://cloud.r-project.org")
}
invisible(lapply(paquetes_requeridos, library, character.only = TRUE))

# Funciones de utilidad -------------------------------------------------------

# Imprime un encabezado de sección con timestamp
seccion <- function(num, total, texto) {
  cat(sprintf("\n[%d/%d] %s  [%s]\n%s\n",
              num, total, texto,
              format(Sys.time(), "%H:%M:%S"),
              strrep("─", 64)))
}

# Detiene la ejecución si x está vacío o es NULL
verificar <- function(x, msg) {
  if (is.null(x) ||
      (is.data.frame(x) && nrow(x) == 0) ||
      (is.vector(x)     && length(x) == 0))
    stop("[ERROR] ", msg, call. = FALSE)
  invisible(x)
}

# Lee un parámetro numérico del objeto de configuración con valor por defecto
parsear_num <- function(cfg, clave, defecto) {
  v <- suppressWarnings(as.numeric(cfg[[clave]]))
  if (is.na(v)) {
    warning("[CONFIG] '", clave, "' ausente; se usa defecto = ", defecto)
    defecto
  } else v
}

# Lee un parámetro lógico del objeto de configuración
parsear_bool <- function(cfg, clave, defecto = TRUE) {
  v <- cfg[[clave]]
  if (is.null(v)) return(defecto)
  toupper(trimws(as.character(v))) %in% c("TRUE", "SI", "1", "YES")
}

# Añade columnas numéricas de metadatos (regimen_num, region_num) a un
# data frame de atributos usando un join seguro: las columnas se inicializan
# con el valor por defecto antes del join para garantizar su existencia
# aunque no haya coincidencias.
unir_metadatos_num <- function(df_attr, df_meta, ids_filtro,
                                recode_regimen, recode_region) {
  met <- df_meta %>%
    filter(id_cuenca %in% ids_filtro) %>%
    dplyr::select(id_cuenca, tipo_regimen, region_climatica) %>%
    mutate(
      regimen_num = dplyr::recode(tipo_regimen,  !!!recode_regimen),
      region_num  = dplyr::recode(region_climatica, !!!recode_region)
    ) %>%
    dplyr::select(id_cuenca, regimen_num, region_num)

  df_attr %>%
    mutate(regimen_num = 0.5, region_num = 0.5) %>%
    left_join(met, by = "id_cuenca", suffix = c(".def", "")) %>%
    mutate(
      regimen_num = ifelse(is.na(regimen_num), regimen_num.def, regimen_num),
      region_num  = ifelse(is.na(region_num),  region_num.def,  region_num)
    ) %>%
    dplyr::select(-ends_with(".def"))
}


# -----------------------------------------------------------------------------
# SECCIÓN 1 · CARGA Y VALIDACIÓN DE DATOS
# -----------------------------------------------------------------------------
seccion(1, 10, "Cargando y validando datos de entrada")

if (!file.exists(RUTA_EXCEL))
  stop("[ERROR] Archivo no encontrado: ", RUTA_EXCEL,
       "\n  → Ajuste RUTA_EXCEL en la Sección 0.", call. = FALSE)

caudales_raw <- read_excel(RUTA_EXCEL, sheet = "caudales_diarios")
atributos    <- read_excel(RUTA_EXCEL, sheet = "atributos_cuencas")
config       <- read_excel(RUTA_EXCEL, sheet = "configuracion")
metadatos    <- suppressMessages(
  read_excel(RUTA_EXCEL, sheet = "metadatos_estaciones"))

cfg <- setNames(as.list(config$valor), config$parametro)

# Parámetros de configuración
N_REGIONES          <- parsear_num(cfg, "num_regiones",              3)
N_PUNTOS            <- parsear_num(cfg, "num_puntos_fdc",           100)
PROB_MIN            <- parsear_num(cfg, "prob_excedencia_min",     0.01)
PROB_MAX            <- parsear_num(cfg, "prob_excedencia_max",     0.99)
UMBRAL_CV_MAX       <- parsear_num(cfg, "umbral_cv_max",            3.0)
MIN_ANIOS           <- parsear_num(cfg, "min_anios_datos",            3)
SEMILLA             <- parsear_num(cfg, "semilla_aleatoria",      12345)
N_BOOTSTRAP         <- parsear_num(cfg, "n_bootstrap",              500)
UMBRAL_H_ARFIMA     <- parsear_num(cfg, "umbral_hurst_arfima",     0.60)
UMBRAL_R2_MU        <- parsear_num(cfg, "umbral_r2_mu",            0.50)
UMBRAL_MIN_REGION   <- parsear_num(cfg, "umbral_min_region",          5)
UMBRAL_R_MU_ATTR    <- parsear_num(cfg, "umbral_r_mu_atributos",   0.40)
USA_METADATOS_CLUST <- parsear_bool(cfg, "usar_metadatos_clustering", TRUE)
PESO_METADATOS      <- parsear_num(cfg, "peso_metadatos_clustering", 0.5)

dir_base     <- cfg$directorio_salida %||% "resultados/"
nombre_proj  <- cfg$nombre_proyecto   %||% "RegFDC_Cauca"
dir_series   <- file.path(dir_base, "series_sinteticas")
dir_graficos <- file.path(dir_base, "graficos")
for (d in c(dir_base, dir_series, dir_graficos))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

prob_exc <- seq(PROB_MIN, PROB_MAX, length.out = N_PUNTOS)

# Período de simulación calculado desde las fechas del Excel
fecha_ini_cfg <- as.Date(cfg$fecha_inicio %||% "2015-01-01")
fecha_fin_cfg <- as.Date(cfg$fecha_fin    %||% "2019-12-31")
N_DIAS_SIM    <- as.integer(fecha_fin_cfg - fecha_ini_cfg) + 1L
cat("   ✓ Período de simulación :", as.character(fecha_ini_cfg), "→",
    as.character(fecha_fin_cfg), "(", N_DIAS_SIM, "días)\n")

# Eliminar fila de unidades si existe (segunda fila del Excel)
if (!is.na(caudales_raw[[1, 1]]) &&
    grepl("yyyy|date|m3", caudales_raw[[1, 1]], ignore.case = TRUE))
  caudales_raw <- caudales_raw[-1, ]

caudales_largo <- caudales_raw %>%
  rename(fecha = 1) %>%
  mutate(fecha = as.Date(suppressWarnings(as.character(fecha)))) %>%
  pivot_longer(-fecha, names_to = "id_cuenca", values_to = "caudal_m3s") %>%
  mutate(caudal_m3s = suppressWarnings(as.numeric(caudal_m3s))) %>%
  filter(!is.na(fecha))

cuencas_obs  <- atributos %>% filter(tiene_datos_caudal == "SI")
cuencas_pred <- atributos %>% filter(tiene_datos_caudal == "NO")
verificar(cuencas_obs,  "Sin cuencas con datos (tiene_datos_caudal == 'SI')")
verificar(cuencas_pred, "Sin cuencas sin datos (tiene_datos_caudal == 'NO')")

VARS_ATTR <- strsplit(
  cfg$variables_clustering %||%
    "area_km2,pendiente_media_pct,precipitacion_anual_mm,cn_promedio,coeficiente_escorrentia",
  ",")[[1]] %>% trimws()

VARS_REGRESION <- strsplit(
  cfg$variables_regresion_mu %||%
    "area_km2,precipitacion_anual_mm,cn_promedio,pendiente_media_pct,coeficiente_escorrentia,escorrentia_anual_mm",
  ",")[[1]] %>% trimws()

# Tabla de recodificación de metadatos (usada en clustering y LOO)
RECODE_REGIMEN <- c("Regular" = 0, "Moderado" = 0.5,
                    "Bimodal" = 0.7, "Torrencial" = 1)
RECODE_REGION  <- c("Andina-Norte" = 0, "Andina-Centro" = 0.5,
                    "Andina-Sur"   = 1)

cat("   ✓ Cuencas observadas   :", nrow(cuencas_obs), "\n")
cat("   ✓ Cuencas a estimar    :", nrow(cuencas_pred), "\n")
cat("   ✓ Variables clustering :", paste(VARS_ATTR,      collapse = ", "), "\n")
cat("   ✓ Variables regresión  :", paste(VARS_REGRESION, collapse = ", "), "\n")


# -----------------------------------------------------------------------------
# SECCIÓN 2 · CÁLCULO DE FDC Y EXPONENTE DE HURST POR CUENCA
# -----------------------------------------------------------------------------
seccion(2, 10, "Calculando FDC, Hurst y autocorrelación por cuenca")

#' Estima el exponente de Hurst por el método R/S escalado (Hurst 1951).
#'
#' @param x  Vector numérico positivo (serie de caudales).
#' @return   H en [0.50, 0.99]; devuelve 0.5 si la serie es demasiado corta.
estimar_hurst_rs <- function(x) {
  x <- x[!is.na(x) & x > 0]
  lx <- log(x)
  n_total <- length(lx)
  if (n_total < 50) return(0.5)

  sub_n <- unique(floor(n_total / c(2, 4, 8, 16, 32, 64)))
  sub_n <- sub_n[sub_n >= 10]
  if (length(sub_n) < 3) return(0.5)

  rs_medio <- sapply(sub_n, function(n_s) {
    bloques <- floor(n_total / n_s)
    rs_vals <- sapply(seq_len(bloques), function(b) {
      seg  <- lx[((b - 1) * n_s + 1):(b * n_s)]
      desv <- cumsum(seg - mean(seg))
      s    <- sd(seg)
      if (s == 0) return(NA_real_)
      (max(desv) - min(desv)) / s
    })
    mean(rs_vals, na.rm = TRUE)
  })

  validos <- !is.na(rs_medio) & rs_medio > 0
  if (sum(validos) < 3) return(0.5)
  coef <- coef(lm(log(rs_medio[validos]) ~ log(sub_n[validos])))
  min(max(coef[2], 0.50), 0.99)
}

#' Calcula la FDC adimensional de una cuenca junto con estadísticos de memoria.
#'
#' @param datos_cuenca  Data frame con columna caudal_m3s.
#' @param probs         Vector de probabilidades de excedencia.
#' @param min_dias      Mínimo de días válidos requeridos.
#' @param cv_max        Umbral de CV para filtrado de outliers.
#' @return              Lista con fdc, mu, phi, H, d, n_datos, n_filtrd;
#'                      NULL si los datos son insuficientes.
calcular_fdc <- function(datos_cuenca, probs,
                          min_dias = MIN_ANIOS * 365,
                          cv_max   = UMBRAL_CV_MAX) {
  q <- datos_cuenca$caudal_m3s
  q <- q[!is.na(q) & q > 0]
  if (length(q) < min_dias) return(NULL)

  n_antes <- length(q)
  q       <- q[q <= mean(q) + cv_max * sd(q)]

  mu <- mean(q)
  if (mu <= 0) return(NULL)

  q_ord <- sort(q / mu, decreasing = TRUE)
  n     <- length(q_ord)
  frec  <- seq_len(n) / (n + 1L)

  spl      <- splinefun(frec, q_ord, method = "hyman")
  q_interp <- pmax(spl(probs), 1e-6)

  lq  <- log(q)
  phi <- max(0, min(0.99, acf(lq, lag.max = 1, plot = FALSE)$acf[2, 1, 1]))
  H   <- estimar_hurst_rs(q)

  list(fdc      = tibble(f_excedencia = probs, q_adimensional = q_interp),
       mu       = mu,
       phi      = phi,
       H        = H,
       d        = max(0, H - 0.5),
       n_datos  = length(q),
       n_filtrd = n_antes - length(q))
}

set.seed(SEMILLA)
fdc_resultados  <- list()
resumen_calidad <- tibble()

for (id in cuencas_obs$id_cuenca) {
  res <- calcular_fdc(caudales_largo %>% filter(id_cuenca == id), prob_exc)
  if (!is.null(res)) {
    fdc_resultados[[id]] <- res
    resumen_calidad <- bind_rows(resumen_calidad, tibble(
      id_cuenca = id,
      n_datos   = res$n_datos,
      n_filtrd  = res$n_filtrd,
      phi       = round(res$phi, 3),
      H         = round(res$H,   3),
      d         = round(res$d,   3),
      mu        = round(res$mu,  3)
    ))
  } else {
    cat("   ⚠ Cuenca", id, "descartada (datos insuficientes)\n")
  }
}
verificar(fdc_resultados, "No se calculó ninguna FDC.")

fdc_todas <- map_dfr(names(fdc_resultados),
                     ~fdc_resultados[[.x]]$fdc %>% mutate(id_cuenca = .x))
mu_tabla  <- tibble(
  id_cuenca        = names(fdc_resultados),
  caudal_medio_m3s = map_dbl(fdc_resultados, "mu"),
  phi_ar1          = map_dbl(fdc_resultados, "phi"),
  hurst            = map_dbl(fdc_resultados, "H"),
  d_fracdiff       = map_dbl(fdc_resultados, "d")
)

cat("   ✓ FDC calculadas         :", length(fdc_resultados), "\n")
cat("   ✓ Hurst mediano          :", round(median(mu_tabla$hurst), 3), "\n")
cat("   ✓ Cuencas con H >", UMBRAL_H_ARFIMA, "  :",
    sum(mu_tabla$hurst > UMBRAL_H_ARFIMA), "→ usarán ARFIMA\n")
cat("   ✓ Outliers filtrados     :", sum(resumen_calidad$n_filtrd), "\n")


# -----------------------------------------------------------------------------
# SECCIÓN 3 · DIAGNÓSTICO DE CORRELACIÓN μ ~ ATRIBUTOS
# -----------------------------------------------------------------------------
seccion(3, 10, "Diagnóstico de correlación μ ~ atributos")

mu_diag <- mu_tabla %>% left_join(atributos, by = "id_cuenca")
log_mu  <- log(mu_diag$caudal_medio_m3s)

cat("\n   r(log predictor, log μ):\n")
diag_corr <- tibble()
for (v in VARS_REGRESION) {
  if (!v %in% names(mu_diag)) next
  x <- as.numeric(mu_diag[[v]])
  if (!all(x > 0, na.rm = TRUE) || all(is.na(x))) next
  r   <- cor(log(x + 1e-9), log_mu, use = "complete.obs")
  p_v <- tryCatch(cor.test(log(x + 1e-9), log_mu)$p.value,
                  error = function(e) NA_real_)
  sig <- if (!is.na(p_v) && p_v < 0.05) "(*)" else "   "
  cat(sprintf("   %-35s r = %+.4f  p = %.4f  %s\n", v, r, p_v, sig))
  diag_corr <- bind_rows(diag_corr, tibble(variable = v, r = r, p_valor = p_v))
}

r_max <- max(abs(diag_corr$r), na.rm = TRUE)
if (r_max < UMBRAL_R_MU_ATTR)
  warning(
    "\nADVERTENCIA CRÍTICA: r_max(log predictor, log μ) = ",
    round(r_max, 3), " < umbral = ", UMBRAL_R_MU_ATTR,
    "\n  → Los atributos no explican el caudal medio.",
    "\n  → VERIFIQUE los datos antes de continuar.",
    call. = FALSE)


# -----------------------------------------------------------------------------
# SECCIÓN 4 · REGIONALIZACIÓN HIDROLÓGICA
# -----------------------------------------------------------------------------
seccion(4, 10, "Regionalización Ward D2 + metadatos + restricción WMO")

datos_obs_attr <- cuencas_obs %>%
  filter(id_cuenca %in% names(fdc_resultados)) %>%
  dplyr::select(id_cuenca, all_of(VARS_ATTR))
verificar(datos_obs_attr,
          paste("Faltan columnas:", paste(VARS_ATTR, collapse = ", ")))

if (USA_METADATOS_CLUST && nrow(metadatos) > 0) {
  datos_obs_attr <- unir_metadatos_num(
    datos_obs_attr, metadatos,
    ids_filtro     = datos_obs_attr$id_cuenca,
    recode_regimen = RECODE_REGIMEN,
    recode_region  = RECODE_REGION)
  VARS_CLUSTER <- c(VARS_ATTR, "regimen_num", "region_num")
  cat("   ✓ Metadatos integrados al clustering (peso =", PESO_METADATOS, ")\n")
} else {
  VARS_CLUSTER <- VARS_ATTR
  cat("   ℹ Clustering sin metadatos (desactivado en config)\n")
}

# Escalado y ponderación de metadatos
mat_raw    <- as.matrix(datos_obs_attr[, VARS_CLUSTER])
rownames(mat_raw) <- datos_obs_attr$id_cuenca
mat_scaled <- scale(mat_raw)
if (USA_METADATOS_CLUST) {
  idx_meta <- which(colnames(mat_scaled) %in% c("regimen_num", "region_num"))
  mat_scaled[, idx_meta] <- mat_scaled[, idx_meta] * PESO_METADATOS
}

dist_attr <- dist(mat_scaled, method = "euclidean")
hc_ward   <- hclust(dist_attr, method = "ward.D2")
coph_r    <- cor(dist_attr, cophenetic(hc_ward))

if (coph_r < 0.75)
  warning("Coef. cofenético = ", round(coph_r, 3),
          " < 0.75: árbol de clustering débil.", call. = FALSE)
cat("   ✓ Coef. cofenético Ward D2 :", round(coph_r, 4), "\n")

# Selección de k por silhouette
k_rango    <- 2:min(6, floor(nrow(datos_obs_attr) / 2))
sil_medios <- sapply(k_rango, function(k) {
  g <- cutree(hc_ward, k = k)
  if (length(unique(g)) < 2) return(-Inf)
  mean(silhouette(g, dist_attr)[, 3])
})
k_silhouette <- k_rango[which.max(sil_medios)]

k_final <- if (!is.na(N_REGIONES) && N_REGIONES != k_silhouette) {
  cat("   ℹ k configurado =", N_REGIONES,
      "| k óptimo (silhouette) =", k_silhouette,
      "→ se usa k configurado\n")
  N_REGIONES
} else {
  cat("   ✓ k óptimo silhouette =", k_silhouette, "\n")
  k_silhouette
}

datos_obs_attr$region <- cutree(hc_ward, k = k_final)
sil_final       <- silhouette(datos_obs_attr$region, dist_attr)
sil_medio_final <- mean(sil_final[, 3])
cat("   ✓ Silhouette medio (k =", k_final, "):", round(sil_medio_final, 4), "\n")

if (sil_medio_final < 0.25)
  warning("Silhouette < 0.25: regiones mal separadas.", call. = FALSE)

# Restricción mínima WMO
n_por_region      <- table(datos_obs_attr$region)
regiones_pequenas <- names(n_por_region)[n_por_region < UMBRAL_MIN_REGION]
if (length(regiones_pequenas) > 0)
  warning("Regiones con < ", UMBRAL_MIN_REGION, " cuencas (mínimo WMO): ",
          paste(regiones_pequenas, collapse = ", "), call. = FALSE)

# Test de homogeneidad Anderson-Darling k-muestras
cat("\n   Test Anderson-Darling de homogeneidad regional (p > 0.05 = OK):\n")
fdc_con_region <- fdc_todas %>%
  left_join(datos_obs_attr %>% dplyr::select(id_cuenca, region), by = "id_cuenca")

ad_resultados <- tibble()
for (r in sort(unique(datos_obs_attr$region))) {
  ids_r <- datos_obs_attr %>% filter(region == r) %>% pull(id_cuenca)
  if (length(ids_r) < 2) {
    cat("     Región", r, ": solo 1 cuenca, test omitido\n"); next
  }
  datos_r  <- fdc_con_region %>%
    filter(region == r, abs(f_excedencia - 0.50) < 0.02)
  muestras <- split(datos_r$q_adimensional, datos_r$id_cuenca)
  muestras <- muestras[sapply(muestras, length) > 0]

  ad_res <- tryCatch(
    kSamples::ad.test(muestras, method = "simulated", Nsim = 1000),
    error = function(e) NULL)
  p_ad   <- if (!is.null(ad_res)) ad_res$ad[1, " asympt. P-value"] else NA
  estado <- if (!is.na(p_ad) && p_ad < 0.05) "⚠ HETEROGÉNEA" else "OK"
  cat(sprintf("     Región %d (n=%d)  p-AD = %.4f  %s\n",
              r, length(ids_r), p_ad, estado))
  ad_resultados <- bind_rows(ad_resultados,
    tibble(region = r, n_cuencas = length(ids_r), p_AD = p_ad, estado = estado))
}

# FDC regional promedio
fdc_regional <- fdc_con_region %>%
  group_by(region, f_excedencia) %>%
  summarise(q_promedio = mean(q_adimensional),
            q_sd       = sd(q_adimensional),
            q_p10      = quantile(q_adimensional, 0.10),
            q_p90      = quantile(q_adimensional, 0.90),
            n_cuencas  = n_distinct(id_cuenca), .groups = "drop")

centroides_attr <- datos_obs_attr %>%
  group_by(region) %>%
  summarise(across(all_of(VARS_CLUSTER), mean, na.rm = TRUE), .groups = "drop")

# Parámetros de memoria (Hurst, d, phi) por región
d_regional <- mu_tabla %>%
  left_join(datos_obs_attr %>% dplyr::select(id_cuenca, region), by = "id_cuenca") %>%
  group_by(region) %>%
  summarise(H_regional   = median(hurst,      na.rm = TRUE),
            d_regional   = median(d_fracdiff, na.rm = TRUE),
            phi_regional = median(phi_ar1,    na.rm = TRUE),
            .groups = "drop")

cat("\n   Composición de regiones:\n")
for (r in sort(unique(datos_obs_attr$region))) {
  ids_r <- datos_obs_attr %>% filter(region == r) %>% pull(id_cuenca)
  H_r   <- d_regional %>% filter(region == r) %>% pull(H_regional)
  cat("     Región", r, ": n =", length(ids_r),
      "| H =", round(H_r, 3),
      if (H_r > UMBRAL_H_ARFIMA) "→ ARFIMA" else "→ AR(1)", "\n")
}


# -----------------------------------------------------------------------------
# SECCIÓN 5 · DETECCIÓN DE EXTRAPOLACIÓN
# -----------------------------------------------------------------------------
seccion(5, 10, "Detección de cuencas fuera del espacio observado")

rango_obs <- datos_obs_attr %>%
  summarise(across(all_of(VARS_ATTR),
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))

extrapolacion_df <- tibble()
for (v in VARS_ATTR) {
  if (!v %in% names(cuencas_pred)) next
  v_min <- rango_obs[[paste0(v, "_min")]]
  v_max <- rango_obs[[paste0(v, "_max")]]
  for (i in seq_len(nrow(cuencas_pred))) {
    val <- as.numeric(cuencas_pred[i, v])
    if (!is.na(val) && (val < v_min || val > v_max))
      extrapolacion_df <- bind_rows(extrapolacion_df, tibble(
        id_cuenca  = cuencas_pred$id_cuenca[i],
        variable   = v,
        valor_pred = round(val, 3),
        rango_obs  = paste0("[", round(v_min, 2), ", ", round(v_max, 2), "]"),
        tipo       = if (val < v_min) "Bajo mínimo" else "Sobre máximo"))
  }
}

if (nrow(extrapolacion_df) > 0) {
  cat("\n   ⚠ Cuencas en zona de EXTRAPOLACIÓN:\n")
  for (i in seq_len(nrow(extrapolacion_df)))
    cat(sprintf("     %-6s  %-35s  val=%-8.2f  obs=%s  [%s]\n",
                extrapolacion_df$id_cuenca[i], extrapolacion_df$variable[i],
                extrapolacion_df$valor_pred[i], extrapolacion_df$rango_obs[i],
                extrapolacion_df$tipo[i]))
} else {
  cat("   ✓ Todas las cuencas dentro del espacio de interpolación\n")
}


# -----------------------------------------------------------------------------
# SECCIÓN 6 · ASIGNACIÓN DE CUENCAS SIN DATOS (MAHALANOBIS)
# -----------------------------------------------------------------------------
seccion(6, 10, "Asignando cuencas sin datos a región (Mahalanobis)")

#' Asigna una cuenca al centroide regional más cercano en distancia de
#' Mahalanobis.
#'
#' @param cuenca_row  Fila de data frame con los atributos de la cuenca.
#' @param centros     Data frame de centroides con columna 'region'.
#' @param S_inv       Inversa de la matriz de covarianza del conjunto de
#'                    entrenamiento.
#' @param vars        Nombres de las variables a usar.
#' @return            Número de región asignada.
asignar_maha <- function(cuenca_row, centros, S_inv, vars) {
  x <- as.numeric(cuenca_row[vars])
  distancias <- apply(centros[, vars, drop = FALSE], 1, function(c_i) {
    d <- x - as.numeric(c_i)
    as.numeric(sqrt(t(d) %*% S_inv %*% d))
  })
  centros$region[which.min(distancias)]
}

vars_maha <- intersect(VARS_CLUSTER, names(cuencas_pred))

if (USA_METADATOS_CLUST) {
  cuencas_pred <- unir_metadatos_num(
    cuencas_pred, metadatos,
    ids_filtro     = cuencas_pred$id_cuenca,
    recode_regimen = RECODE_REGIMEN,
    recode_region  = RECODE_REGION)
  vars_maha <- VARS_CLUSTER
}

mat_train <- as.matrix(datos_obs_attr[, vars_maha])
S_train   <- cov(mat_train)
S_inv     <- tryCatch(
  solve(S_train),
  error = function(e) {
    warning("Covarianza singular: usando inversa generalizada (MASS::ginv)")
    MASS::ginv(S_train)
  })

cuencas_pred <- cuencas_pred %>%
  mutate(region = sapply(seq_len(nrow(.)),
                         function(i) asignar_maha(cuencas_pred[i, ],
                                                   centroides_attr,
                                                   S_inv, vars_maha)))

fdc_sinteticas <- cuencas_pred %>%
  dplyr::select(id_cuenca, region) %>%
  left_join(fdc_regional %>%
              dplyr::select(region, f_excedencia, q_promedio, q_p10, q_p90),
            by = "region") %>%
  rename(q_adimensional = q_promedio)

cat("   ✓ Asignación completada\n")
for (i in seq_len(nrow(cuencas_pred)))
  cat("    ", cuencas_pred$id_cuenca[i], "→ Región",
      cuencas_pred$region[i], "\n")


# -----------------------------------------------------------------------------
# SECCIÓN 7 · MODELO DE CAUDAL MEDIO
# -----------------------------------------------------------------------------
seccion(7, 10, "Modelo log-log μ — selección BIC + diagnóstico")

mu_datos <- mu_tabla %>%
  left_join(atributos, by = "id_cuenca") %>%
  filter(caudal_medio_m3s > 0)

# Imputar NAs con mediana de columna
for (v in VARS_REGRESION) {
  if (v %in% names(mu_datos) && any(is.na(mu_datos[[v]]))) {
    mu_datos[[v]][is.na(mu_datos[[v]])] <- median(mu_datos[[v]], na.rm = TRUE)
    warning("NAs imputados en '", v, "' con la mediana.", call. = FALSE)
  }
}

mu_datos_log <- mu_datos %>% mutate(log_mu = log(caudal_medio_m3s))

# Construir predictores en escala log cuando todos los valores son positivos
log_vars <- character()
for (v in VARS_REGRESION) {
  if (!v %in% names(mu_datos_log)) next
  x <- mu_datos_log[[v]]
  if (all(x > 0, na.rm = TRUE)) {
    nm <- paste0("log_", v)
    mu_datos_log[[nm]] <- log(x)
    log_vars <- c(log_vars, nm)
  } else {
    log_vars <- c(log_vars, v)
  }
}

# Selección de predictores por BIC (k = log n)
formula_completa <- as.formula(paste("log_mu ~", paste(log_vars, collapse = " + ")))
modelo_completo  <- lm(formula_completa, data = mu_datos_log)
modelo_mu        <- MASS::stepAIC(modelo_completo,
                                  direction = "both",
                                  trace     = FALSE,
                                  k         = log(nrow(mu_datos_log)))

sumario_mu  <- summary(modelo_mu)
sigma2_mu   <- sumario_mu$sigma^2
r2_mu       <- sumario_mu$r.squared
r2_adj_mu   <- sumario_mu$adj.r.squared
n_modelo    <- nrow(mu_datos_log)

cat("   ✓ Criterio de selección   : BIC (k = log n)\n")
cat("   ✓ Variables seleccionadas :",
    paste(names(coef(modelo_mu))[-1], collapse = ", "), "\n")
cat("   ✓ R²          :", round(r2_mu,     4), "\n")
cat("   ✓ R² ajustado :", round(r2_adj_mu, 4), "\n")
cat("   ✓ σ (log)     :", round(sqrt(sigma2_mu), 4), "\n")

if (r2_adj_mu < UMBRAL_R2_MU)
  warning("R²adj = ", round(r2_adj_mu, 3), " < ", UMBRAL_R2_MU,
          ". Modelo μ con baja capacidad predictiva.", call. = FALSE)

# Diagnóstico: distancia de Cook
cook        <- cooks.distance(modelo_mu)
umbral_cook <- 4 / n_modelo
influyentes <- which(cook > umbral_cook)
if (length(influyentes) > 0)
  cat("   ⚠ Cuencas influyentes (Cook > 4/n):",
      paste(mu_datos_log$id_cuenca[influyentes], collapse = ", "), "\n")

#' Predice caudal medio con corrección de sesgo de Jensen (exp(σ²/2)) e IC.
#'
#' @param modelo      Objeto lm ajustado en escala log.
#' @param nuevos_dat  Data frame de cuencas a predecir.
#' @param sigma2      Varianza residual del modelo.
#' @param log_vars_m  Nombres de los predictores en escala log.
#' @param nivel_ic    Nivel de confianza del intervalo de predicción.
#' @return            Tibble con id_cuenca, mu_estimado, mu_ic_inf, mu_ic_sup.
predecir_mu <- function(modelo, nuevos_dat, sigma2, log_vars_m, nivel_ic = 0.90) {
  nd <- nuevos_dat
  for (v in log_vars_m) {
    v_orig <- sub("^log_", "", v)
    if (v_orig %in% names(nd) && all(nd[[v_orig]] > 0, na.rm = TRUE))
      nd[[v]] <- log(nd[[v_orig]])
  }
  pred_ic <- tryCatch(
    predict(modelo, newdata = nd, interval = "prediction", level = nivel_ic),
    error = function(e) {
      warning("IC de predicción falló: ", e$message)
      m <- predict(modelo, newdata = nd)
      cbind(fit = m, lwr = m - 2, upr = m + 2)
    })
  tibble(id_cuenca   = nuevos_dat$id_cuenca,
         mu_estimado = exp(pred_ic[, "fit"] + sigma2 / 2),
         mu_ic_inf   = exp(pred_ic[, "lwr"]),
         mu_ic_sup   = exp(pred_ic[, "upr"]))
}

log_vars_modelo <- names(coef(modelo_mu))[-1]
pred_mu <- predecir_mu(modelo_mu, cuencas_pred, sigma2_mu, log_vars_modelo)
cuencas_pred <- cuencas_pred %>% left_join(pred_mu, by = "id_cuenca")

cat("\n   Caudales medios estimados (IC 90%):\n")
for (i in seq_len(nrow(cuencas_pred)))
  cat(sprintf("     %-6s  μ = %7.2f m³/s  IC90: [%6.2f, %7.2f]\n",
              cuencas_pred$id_cuenca[i], cuencas_pred$mu_estimado[i],
              cuencas_pred$mu_ic_inf[i],  cuencas_pred$mu_ic_sup[i]))

# Escorrentía específica observada (L/s/km²)
resumen_calidad <- resumen_calidad %>%
  left_join(atributos %>% dplyr::select(id_cuenca, area_km2), by = "id_cuenca") %>%
  mutate(q_especifica_ls_km2 = mu * 1000 / area_km2)


# -----------------------------------------------------------------------------
# SECCIÓN 8 · GENERACIÓN DE SERIES SINTÉTICAS
# -----------------------------------------------------------------------------
seccion(8, 10, "Generando series sintéticas ARFIMA/AR(1) según Hurst")

cuencas_pred <- cuencas_pred %>%
  left_join(d_regional %>% dplyr::select(region, H_regional, d_regional,
                                          phi_regional), by = "region") %>%
  mutate(phi_regional = replace_na(phi_regional, 0.70),
         d_regional   = replace_na(d_regional,   0.20),
         H_regional   = replace_na(H_regional,   0.70))

fdc_dimensional <- fdc_sinteticas %>%
  left_join(cuencas_pred %>%
              dplyr::select(id_cuenca, mu_estimado, mu_ic_inf, mu_ic_sup),
            by = "id_cuenca") %>%
  mutate(caudal_m3s    = q_adimensional * mu_estimado,
         caudal_ic_inf = q_p10 * mu_ic_inf,
         caudal_ic_sup = q_p90 * mu_ic_sup)

#' Genera una serie diaria sintética mediante transformación normal-cuantil.
#'
#' Si H > UMBRAL_H_ARFIMA se usa ARFIMA(0,d,0) (fracdiff::fracdiff.sim) con
#' d = H − 0.5 (Hosking 1981); de lo contrario se usa AR(1).
#' La transformación cuantil garantiza que la distribución marginal reproduce
#' la FDC objetivo.
#'
#' @param fdc_cuenca  Tibble con columnas f_excedencia y caudal_m3s.
#' @param n_dias      Longitud de la serie a generar (días).
#' @param H           Exponente de Hurst regional.
#' @param d           Parámetro fraccionario (= H − 0.5).
#' @param phi         Coeficiente AR(1) alternativo.
#' @param fecha_ini   Fecha de inicio en formato "YYYY-MM-DD".
#' @param semilla     Semilla para reproducibilidad.
#' @return            Tibble con columnas fecha y caudal_m3s.
generar_serie_sintetica <- function(fdc_cuenca, n_dias, H, d, phi,
                                    fecha_ini = "2015-01-01",
                                    semilla   = SEMILLA) {
  set.seed(semilla)
  if (H > UMBRAL_H_ARFIMA) {
    z_raw <- fracdiff::fracdiff.sim(n_dias, d = d)$series
    z     <- (z_raw - mean(z_raw)) / sd(z_raw)
  } else {
    sd_inn <- sqrt(1 - phi^2)
    z      <- numeric(n_dias)
    z[1]   <- rnorm(1)
    for (t in 2:n_dias) z[t] <- phi * z[t - 1] + rnorm(1, sd = sd_inn)
    z <- (z - mean(z)) / sd(z)
  }
  u      <- pnorm(z)
  q_sint <- approx(1 - fdc_cuenca$f_excedencia, fdc_cuenca$caudal_m3s,
                   xout = u, rule = 2)$y
  tibble(
    fecha      = seq.Date(as.Date(fecha_ini), by = "day", length.out = n_dias),
    caudal_m3s = pmax(q_sint, 0.001)
  )
}

series_sinteticas <- list()
for (id in cuencas_pred$id_cuenca) {
  fdc_id <- fdc_dimensional %>%
    filter(id_cuenca == id) %>%
    dplyr::select(f_excedencia, caudal_m3s) %>%
    arrange(f_excedencia)
  idx    <- which(cuencas_pred$id_cuenca == id)
  H_id   <- cuencas_pred$H_regional[idx]
  d_id   <- cuencas_pred$d_regional[idx]
  phi_id <- cuencas_pred$phi_regional[idx]

  serie <- generar_serie_sintetica(
    fdc_id, N_DIAS_SIM, H_id, d_id, phi_id,
    fecha_ini = as.character(fecha_ini_cfg),
    semilla   = SEMILLA + idx)
  serie$id_cuenca <- id
  series_sinteticas[[id]] <- serie
  write.csv(serie,
            file.path(dir_series, paste0(id, ".csv")),
            row.names = FALSE)
}
series_todas <- bind_rows(series_sinteticas)

cat("   ✓ Series generadas:", length(series_sinteticas), "cuencas\n")
cat("   ✓ ARFIMA:", sum(cuencas_pred$H_regional > UMBRAL_H_ARFIMA),
    "cuencas | AR(1):", sum(cuencas_pred$H_regional <= UMBRAL_H_ARFIMA),
    "cuencas\n")


# -----------------------------------------------------------------------------
# SECCIÓN 9 · VALIDACIÓN CRUZADA LEAVE-ONE-OUT
# -----------------------------------------------------------------------------
seccion(9, 10, "Validación cruzada LOO con métricas hidrológicas")

#' Calcula un conjunto estándar de métricas de ajuste hidrológico.
#'
#' Siempre devuelve un vector nombrado de longitud 7 (NA cuando n < 3).
#'
#' @param obs  Vector de valores observados.
#' @param sim  Vector de valores simulados.
#' @return     Vector nombrado: NSE, KGE, PBIAS, RMSE, MAE, MAPE, r_Spearman.
calcular_metricas <- function(obs, sim) {
  nm  <- c("NSE", "KGE", "PBIAS", "RMSE", "MAE", "MAPE", "r_Spearman")
  idx <- !is.na(obs) & !is.na(sim)
  obs <- obs[idx]; sim <- sim[idx]; n <- length(obs)
  if (n < 3) return(setNames(rep(NA_real_, 7L), nm))
  mo    <- mean(obs); ms <- mean(sim)
  so    <- sd(obs);   ss <- sd(sim)
  r_val <- cor(obs, sim)
  NSE   <- 1 - sum((obs - sim)^2) / sum((obs - mo)^2)
  KGE   <- 1 - sqrt((r_val - 1)^2 + (ms/mo - 1)^2 + ((ss/ms)/(so/mo) - 1)^2)
  PBIAS <- 100 * (sum(sim) - sum(obs)) / sum(obs)
  setNames(
    c(NSE, KGE, PBIAS,
      sqrt(mean((obs - sim)^2)),
      mean(abs(obs - sim)),
      mean(abs(obs - sim) / abs(obs)) * 100,
      cor(obs, sim, method = "spearman")),
    nm)
}

resultados_loo <- tibble()
ids_obs        <- names(fdc_resultados)

for (id_test in ids_obs) {
  ids_train  <- setdiff(ids_obs, id_test)
  fdc_tr_df  <- fdc_todas %>% filter(id_cuenca %in% ids_train)
  attr_train <- datos_obs_attr %>% filter(id_cuenca %in% ids_train)

  if (USA_METADATOS_CLUST)
    attr_train <- unir_metadatos_num(
      attr_train, metadatos,
      ids_filtro     = ids_train,
      recode_regimen = RECODE_REGIMEN,
      recode_region  = RECODE_REGION)

  mat_tr <- scale(as.matrix(attr_train[, vars_maha]))
  if (USA_METADATOS_CLUST) {
    idx_m <- which(colnames(mat_tr) %in% c("regimen_num", "region_num"))
    mat_tr[, idx_m] <- mat_tr[, idx_m] * PESO_METADATOS
  }
  dist_tr           <- dist(mat_tr)
  hc_tr             <- hclust(dist_tr, method = "ward.D2")
  attr_train$region_loo <- cutree(hc_tr, k = k_final)

  fdc_reg_loo <- fdc_tr_df %>%
    left_join(attr_train %>% dplyr::select(id_cuenca, region_loo),
              by = "id_cuenca") %>%
    group_by(region_loo, f_excedencia) %>%
    summarise(q_promedio = mean(q_adimensional), .groups = "drop")

  centros_loo <- attr_train %>%
    group_by(region_loo) %>%
    summarise(across(all_of(vars_maha), mean, na.rm = TRUE), .groups = "drop") %>%
    rename(region = region_loo)

  S_loo  <- cov(as.matrix(attr_train[, vars_maha]))
  SI_loo <- tryCatch(solve(S_loo), error = function(e) MASS::ginv(S_loo))

  attr_test <- atributos %>% filter(id_cuenca == id_test)
  if (USA_METADATOS_CLUST) {
    mt_test <- metadatos %>% filter(id_cuenca == id_test)
    attr_test$regimen_num <- if (nrow(mt_test) > 0)
      dplyr::recode(mt_test$tipo_regimen[1],    !!!RECODE_REGIMEN, .default = 0.5)
      else 0.5
    attr_test$region_num  <- if (nrow(mt_test) > 0)
      dplyr::recode(mt_test$region_climatica[1], !!!RECODE_REGION,  .default = 0.5)
      else 0.5
  }

  region_test <- asignar_maha(attr_test, centros_loo, SI_loo, vars_maha)

  comp <- fdc_resultados[[id_test]]$fdc %>%
    rename(q_observado = q_adimensional) %>%
    left_join(
      fdc_reg_loo %>% filter(region_loo == region_test) %>%
        dplyr::select(f_excedencia, q_estimado = q_promedio),
      by = "f_excedencia") %>%
    filter(!is.na(q_estimado)) %>%
    mutate(id_cuenca = id_test)
  resultados_loo <- bind_rows(resultados_loo, comp)
}

metricas_globales <- calcular_metricas(
  resultados_loo$q_observado,
  resultados_loo$q_estimado) %>% as_tibble_row()

segmentos <- list(
  "Altos (F<0.20)"     = resultados_loo %>% filter(f_excedencia < 0.20),
  "Medios (0.20-0.80)" = resultados_loo %>% filter(between(f_excedencia, 0.20, 0.80)),
  "Bajos (F>0.80)"     = resultados_loo %>% filter(f_excedencia > 0.80)
)
metricas_por_segmento <- map_dfr(names(segmentos), ~{
  calcular_metricas(segmentos[[.x]]$q_observado,
                    segmentos[[.x]]$q_estimado) %>%
    as_tibble_row() %>% mutate(segmento = .x)
}) %>% relocate(segmento)

metricas_por_f <- resultados_loo %>%
  group_by(f_excedencia) %>%
  summarise(MAE  = mean(abs(q_observado - q_estimado)),
            MAPE = mean(abs(q_observado - q_estimado) / q_observado) * 100,
            NSE  = calcular_metricas(q_observado, q_estimado)[["NSE"]],
            .groups = "drop")

NSE_global   <- metricas_globales$NSE
KGE_global   <- metricas_globales$KGE
PBIAS_global <- abs(metricas_globales$PBIAS)

cat("\n   Métricas globales LOO:\n")
cat("   NSE       :", round(NSE_global, 4), "  (ref: > 0.65)\n")
cat("   KGE       :", round(KGE_global, 4), "  (ref: > 0.60)\n")
cat("   PBIAS     :", round(metricas_globales$PBIAS, 2),
    "%  (ref: |PBIAS| < 25%)\n")
cat("   r Spearman:", round(metricas_globales$r_Spearman, 4), "\n")

set.seed(SEMILLA)
boot_m <- replicate(N_BOOTSTRAP, {
  idx <- sample(nrow(resultados_loo), replace = TRUE)
  calcular_metricas(resultados_loo$q_observado[idx],
                    resultados_loo$q_estimado[idx])[c("NSE", "KGE", "PBIAS")]
})
boot_resumen <- lapply(c("NSE", "KGE", "PBIAS"), function(m) {
  vals <- as.numeric(boot_m[m, ])
  list(media  = mean(vals,  na.rm = TRUE),
       ic_inf = unname(quantile(vals, 0.025, na.rm = TRUE)),
       ic_sup = unname(quantile(vals, 0.975, na.rm = TRUE)))
})
names(boot_resumen) <- c("NSE", "KGE", "PBIAS")

cat("   NSE  IC95%: [", round(boot_resumen$NSE$ic_inf, 3), ",",
    round(boot_resumen$NSE$ic_sup, 3), "]\n")
cat("   KGE  IC95%: [", round(boot_resumen$KGE$ic_inf, 3), ",",
    round(boot_resumen$KGE$ic_sup, 3), "]\n")
cat("   PBIAS IC95%:[", round(boot_resumen$PBIAS$ic_inf, 2), "%,",
    round(boot_resumen$PBIAS$ic_sup, 2), "%]\n")


# -----------------------------------------------------------------------------
# SECCIÓN 10 · GRÁFICOS Y EXPORTACIÓN
# -----------------------------------------------------------------------------
seccion(10, 10, "Gráficos y exportación de resultados")

tema_fdc <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor  = element_blank(),
        plot.title        = element_text(size = 13, face = "bold"),
        plot.subtitle     = element_text(size = 10, color = "gray40"),
        axis.title        = element_text(size = 11))

guardar <- function(grafico, nombre, w = 10, h = 6)
  ggsave(file.path(dir_graficos, nombre), grafico,
         width = w, height = h, dpi = 150)

# G1 — FDC dimensionales con banda de incertidumbre
g1 <- ggplot(fdc_dimensional, aes(x = f_excedencia)) +
  geom_ribbon(aes(ymin = caudal_ic_inf, ymax = caudal_ic_sup,
                  fill = id_cuenca), alpha = 0.15) +
  geom_line(aes(y = caudal_m3s, color = id_cuenca), linewidth = 1.1) +
  scale_y_log10(labels = scales::comma_format()) +
  scale_color_viridis_d(name = "Cuenca") +
  scale_fill_viridis_d(name  = "Cuenca") +
  labs(title    = "FDC Sintéticas — Cuencas Sin Datos | Sistema Río Cauca",
       subtitle = "Banda: IC 90% (p10–p90 FDC regional × IC90 μ estimado)",
       x = "Frecuencia de excedencia", y = "Caudal (m³/s)") + tema_fdc
guardar(g1, "G1_FDC_dimensionales.png", w = 12, h = 7)

# G2 — FDC regionales adimensionales
g2 <- ggplot(fdc_regional, aes(x = f_excedencia)) +
  geom_ribbon(aes(ymin = q_p10, ymax = q_p90, fill = factor(region)),
              alpha = 0.2) +
  geom_line(aes(y = q_promedio, color = factor(region)), linewidth = 1.1) +
  scale_y_log10() +
  scale_color_viridis_d(name = "Región") +
  scale_fill_viridis_d(name  = "Región") +
  labs(title    = "FDC Regionales Adimensionales — Sistema Río Cauca",
       subtitle = "Banda: p10–p90 entre cuencas de cada región",
       x = "Frecuencia de excedencia", y = "Q/Q̅") + tema_fdc
guardar(g2, "G2_FDC_regionales.png")

# G3 — Error LOO por frecuencia (MAPE y NSE)
g3a <- ggplot(metricas_por_f, aes(x = f_excedencia, y = MAPE)) +
  geom_line(color = "#2c7bb6", linewidth = 1) +
  geom_hline(yintercept = metricas_globales$MAPE,
             linetype = "dashed", color = "firebrick") +
  labs(title = "MAPE por frecuencia de excedencia (LOO)",
       x = "Frecuencia", y = "MAPE (%)") + tema_fdc

g3b <- ggplot(metricas_por_f, aes(x = f_excedencia, y = NSE)) +
  geom_line(color = "#1a9641", linewidth = 1) +
  geom_hline(yintercept = 0.65, linetype = "dashed", color = "gray50") +
  labs(title = "NSE por frecuencia de excedencia (LOO)",
       x = "Frecuencia", y = "NSE") + tema_fdc
guardar(g3a / g3b, "G3_Error_LOO_frecuencia.png", h = 9)

# G4 — Q-Q adimensional por región
qq_data <- resultados_loo %>%
  left_join(datos_obs_attr %>% dplyr::select(id_cuenca, region), by = "id_cuenca")
g4 <- ggplot(qq_data, aes(x = q_observado, y = q_estimado,
                           color = factor(region))) +
  geom_point(alpha = 0.35, size = 0.9) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.8) +
  scale_x_log10() + scale_y_log10() +
  scale_color_viridis_d(name = "Región") +
  facet_wrap(~region, labeller = label_both) +
  labs(title = "Q-Q adimensional LOO por región — Sistema Río Cauca",
       x = "Q/Q̅ observado", y = "Q/Q̅ estimado") +
  tema_fdc + theme(legend.position = "none")
guardar(g4, "G4_QQ_regional.png")

# G5 — Dendrograma Ward D2 + curva silhouette
png(file.path(dir_graficos, "G5_Dendrograma_Ward.png"),
    width = 1400, height = 700, res = 120)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(hc_ward, labels = datos_obs_attr$id_cuenca,
     main = "Ward D2 + metadatos | Sistema Río Cauca",
     sub  = paste0("Cofenético = ", round(coph_r, 3)),
     xlab = "", ylab = "Distancia", cex = 0.8)
rect.hclust(hc_ward, k = k_final, border = viridis::viridis(k_final))
plot(k_rango, sil_medios, type = "b", pch = 19,
     col  = ifelse(k_rango == k_final, "firebrick", "steelblue"),
     main = "Selección k (silhouette medio)",
     xlab = "k regiones", ylab = "Silhouette medio")
abline(v = k_final, lty = 2, col = "firebrick")
dev.off()

# G6 — Serie sintética de ejemplo
id_ej <- cuencas_pred$id_cuenca[1]
H_ej  <- cuencas_pred$H_regional[1]
g6 <- ggplot(series_sinteticas[[id_ej]], aes(x = fecha, y = caudal_m3s)) +
  geom_line(color = "#2c7bb6", linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.15,
              color = "firebrick", linewidth = 0.8, se = FALSE) +
  labs(title    = paste("Serie Sintética —", id_ej,
                        "| Sistema Río Cauca"),
       subtitle = paste0("H = ", round(H_ej, 3),
                         if (H_ej > UMBRAL_H_ARFIMA) " [ARFIMA]" else " [AR(1)]",
                         "  |  μ = ",
                         round(cuencas_pred$mu_estimado[1], 2), " m³/s"),
       x = "Fecha", y = "Caudal (m³/s)") + tema_fdc
guardar(g6, paste0("G6_Serie_", id_ej, ".png"), w = 14, h = 5)

# G7 — Diagnóstico de regresión del modelo μ (residuos y Cook)
df_diag <- data.frame(
  idx       = seq_len(n_modelo),
  fitted    = fitted(modelo_mu),
  residuals = residuals(modelo_mu),
  cook      = cook,
  id        = mu_datos_log$id_cuenca
)
df_infl <- df_diag[influyentes, , drop = FALSE]

g7a <- ggplot(df_diag, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE,
              color = "firebrick", linewidth = 0.7) +
  labs(title = "Residuos vs Fitted — Modelo μ (log-log, BIC)",
       x = "Valores ajustados (log)", y = "Residuos") + tema_fdc

g7b <- ggplot(df_diag, aes(x = idx, y = cook)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = umbral_cook, linetype = "dashed", color = "firebrick") +
  { if (nrow(df_infl) > 0)
      geom_text(data = df_infl, aes(x = idx, y = cook, label = id),
                vjust = -0.3, size = 3, color = "firebrick", inherit.aes = FALSE)
    else list() } +
  labs(title = "Cook's Distance — Cuencas influyentes (umbral = 4/n)",
       x = "Índice cuenca", y = "Cook's D") + tema_fdc
guardar(g7a / g7b, "G7_Diagnostico_mu.png", h = 9)

# G8 — Escorrentía específica observada por cuenca
g8 <- ggplot(resumen_calidad %>% arrange(q_especifica_ls_km2),
             aes(x = reorder(id_cuenca, q_especifica_ls_km2),
                 y = q_especifica_ls_km2)) +
  geom_col(fill = "#2c7bb6", alpha = 0.85) +
  geom_hline(yintercept = median(resumen_calidad$q_especifica_ls_km2, na.rm = TRUE),
             linetype = "dashed", color = "firebrick") +
  labs(title    = "Escorrentía específica por cuenca — Sistema Río Cauca",
       subtitle = "Línea roja = mediana regional | Típico Andes colombianos: 30–120 L/s/km²",
       x = "Cuenca", y = "q (L/s/km²)") +
  coord_flip() + tema_fdc
guardar(g8, "G8_Escorrentia_especifica.png", w = 8, h = 7)

# G9 — Exponente de Hurst por cuenca y región
hurst_data <- resumen_calidad %>%
  left_join(datos_obs_attr %>% dplyr::select(id_cuenca, region), by = "id_cuenca")
g9 <- ggplot(hurst_data, aes(x = reorder(id_cuenca, H),
                               y = H, fill = factor(region))) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = UMBRAL_H_ARFIMA, linetype = "dashed", color = "black") +
  scale_fill_viridis_d(name = "Región") +
  annotate("text", x = 2, y = UMBRAL_H_ARFIMA + 0.02,
           label = paste0("Umbral ARFIMA = ", UMBRAL_H_ARFIMA),
           size = 3.2) +
  labs(title    = "Exponente de Hurst (R/S) — Sistema Río Cauca",
       subtitle = "Por encima del umbral → ARFIMA(0,d,0) en series sintéticas",
       x = "Cuenca", y = "H") +
  coord_flip() + tema_fdc
guardar(g9, "G9_Hurst_cuencas.png", w = 8, h = 7)

cat("   ✓ 9 gráficos exportados →", dir_graficos, "\n")

# Calificación global del modelo
calidad <- dplyr::case_when(
  NSE_global >= 0.75 & KGE_global >= 0.70 & PBIAS_global <= 15 ~ "EXCELENTE",
  NSE_global >= 0.65 & KGE_global >= 0.60 & PBIAS_global <= 25 ~ "BUENA",
  NSE_global >= 0.50 & KGE_global >= 0.45 & PBIAS_global <= 35 ~ "REGULAR",
  TRUE ~ "DEFICIENTE")

# Exportación a Excel (14 hojas)
resultados_excel <- list(
  "FDC_regional"       = fdc_regional,
  "FDC_sinteticas"     = fdc_sinteticas,
  "FDC_dimensional"    = fdc_dimensional,
  "Asignacion"         = cuencas_pred %>%
    dplyr::select(id_cuenca, region, mu_estimado, mu_ic_inf, mu_ic_sup,
                  H_regional, d_regional, phi_regional),
  "Calidad_registros"  = resumen_calidad,
  "Clustering"         = datos_obs_attr %>%
    dplyr::select(id_cuenca, region, all_of(VARS_ATTR)),
  "AD_homogeneidad"    = ad_resultados,
  "Extrapolacion"      = if (nrow(extrapolacion_df) > 0) extrapolacion_df
                         else tibble(mensaje = "Todas dentro del rango observado"),
  "Diag_corr_mu"       = diag_corr,
  "Modelo_mu"          = tibble(Parametro = names(coef(modelo_mu)),
                                Coef      = round(coef(modelo_mu), 6),
                                R2        = round(r2_mu, 4),
                                R2_adj    = round(r2_adj_mu, 4)),
  "Metricas_LOO"       = metricas_globales,
  "Metricas_segmento"  = metricas_por_segmento,
  "Error_por_F"        = metricas_por_f,
  "Bootstrap_IC95"     = tibble(
    metrica = names(boot_resumen),
    media   = sapply(boot_resumen, `[[`, "media"),
    ic_inf  = sapply(boot_resumen, `[[`, "ic_inf"),
    ic_sup  = sapply(boot_resumen, `[[`, "ic_sup"))
)
write_xlsx(resultados_excel,
           file.path(dir_base, paste0(nombre_proj, "_1.0.0_resultados.xlsx")))
write.csv(series_todas,
          file.path(dir_base, "series_sinteticas_todas.csv"), row.names = FALSE)

# Informe A — Diagnóstico técnico ---------------------------------------------
escribir_informe_a <- function() {
  sep  <- strrep("═", 67)
  sep2 <- strrep("─", 67)
  con  <- file(file.path(dir_base, "A_diagnostico_tecnico.txt"),
               "w", encoding = "UTF-8")
  tryCatch({
    writeLines(sep, con)
    writeLines("  INFORME A — DIAGNÓSTICO TÉCNICO DEL MODELO", con)
    writeLines(paste("  Proyecto:", nombre_proj, "v1.0.0 | Fecha:", Sys.Date()), con)
    writeLines("  Región: Sistema Río Cauca — Andes colombianos", con)
    writeLines(sep, con)
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  A1. CALIDAD DE DATOS DE ENTRADA", con)
    writeLines(sep2, con)
    writeLines(paste("  Cuencas observadas         :", nrow(cuencas_obs)), con)
    writeLines(paste("  Cuencas a estimar          :", nrow(cuencas_pred)), con)
    writeLines(paste("  Período de registro        :", cfg$fecha_inicio,
                     "→", cfg$fecha_fin), con)
    writeLines(paste("  Outliers filtrados (total) :",
                     sum(resumen_calidad$n_filtrd), "registros"), con)
    writeLines("", con)
    writeLines("  Correlación log(predictor) ~ log(μ)  [umbral ≥ 0.40]:", con)
    for (i in seq_len(nrow(diag_corr))) {
      sig <- if (!is.na(diag_corr$p_valor[i]) && diag_corr$p_valor[i] < 0.05)
               "(*)" else "   "
      writeLines(sprintf("    %-38s r = %+.4f  p = %.4f  %s",
                         diag_corr$variable[i], diag_corr$r[i],
                         diag_corr$p_valor[i], sig), con)
    }
    writeLines(paste("\n  r_max =", round(r_max, 4),
                     if (r_max < UMBRAL_R_MU_ATTR)
                       "  ⚠ ADVERTENCIA CRÍTICA" else "  [OK]"), con)
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  A2. DETECCIÓN DE EXTRAPOLACIÓN", con)
    writeLines(sep2, con)
    if (nrow(extrapolacion_df) > 0) {
      writeLines(paste("  ⚠", nrow(extrapolacion_df),
                       "caso(s) fuera del rango observado:"), con)
      for (i in seq_len(nrow(extrapolacion_df)))
        writeLines(sprintf("    %-6s  %-35s  val=%-8.2f  obs=%s  [%s]",
                           extrapolacion_df$id_cuenca[i],
                           extrapolacion_df$variable[i],
                           extrapolacion_df$valor_pred[i],
                           extrapolacion_df$rango_obs[i],
                           extrapolacion_df$tipo[i]), con)
    } else {
      writeLines("  Todas las cuencas dentro del espacio de interpolación  [OK]", con)
    }
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  A3. REGIONALIZACIÓN HIDROLÓGICA", con)
    writeLines(sep2, con)
    writeLines("  Método      : Ward D2 — atributos físicos + metadatos ponderados", con)
    writeLines(paste("  Regiones (k):", k_final), con)
    writeLines(paste("  Cofenético  :", round(coph_r, 4),
                     if (coph_r > 0.75) "[OK]" else "[⚠ < 0.75]"), con)
    writeLines(paste("  Silhouette  :", round(sil_medio_final, 4)), con)
    writeLines("", con)
    for (r in sort(unique(datos_obs_attr$region))) {
      ids_r <- datos_obs_attr %>% filter(region == r) %>% pull(id_cuenca)
      writeLines(paste0("    Región ", r, " (n=", length(ids_r), "): ",
                        paste(ids_r, collapse = ", ")), con)
    }
    if (nrow(ad_resultados) > 0) {
      writeLines("", con)
      writeLines("  Test Anderson-Darling [p > 0.05 = homogénea]:", con)
      for (i in seq_len(nrow(ad_resultados)))
        writeLines(sprintf("    Región %d (n=%d)  p = %.4f  %s",
                           ad_resultados$region[i], ad_resultados$n_cuencas[i],
                           ad_resultados$p_AD[i], ad_resultados$estado[i]), con)
    }
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  A4. MODELO DE CAUDAL MEDIO (μ)", con)
    writeLines(sep2, con)
    writeLines("  Forma     : log(μ) ~ predictores | criterio BIC (k = log n)", con)
    writeLines(paste("  Variables :", paste(names(coef(modelo_mu))[-1], collapse = ", ")), con)
    writeLines(paste("  R²        :", round(r2_mu, 4)), con)
    writeLines(paste("  R² ajust. :", round(r2_adj_mu, 4),
                     if (r2_adj_mu < UMBRAL_R2_MU) "  [⚠ bajo]" else "  [OK]"), con)
    writeLines(paste("  σ (log)   :", round(sqrt(sigma2_mu), 4)), con)
    writeLines(paste("  Correc. Jensen exp(σ²/2) =", round(exp(sigma2_mu/2), 4)), con)
    writeLines("  Coeficientes:", con)
    for (nm in names(coef(modelo_mu)))
      writeLines(sprintf("    %-28s = %+.6f", nm, coef(modelo_mu)[nm]), con)
    if (length(influyentes) > 0)
      writeLines(paste("\n  ⚠ Cuencas influyentes (Cook > 4/n):",
                       paste(mu_datos_log$id_cuenca[influyentes], collapse = ", ")), con)
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  A5. MEMORIA TEMPORAL — HURST POR CUENCA", con)
    writeLines(sep2, con)
    writeLines(paste("  Umbral ARFIMA: H >", UMBRAL_H_ARFIMA,
                     "→ ARFIMA(0,d,0)  |  H ≤", UMBRAL_H_ARFIMA, "→ AR(1)"), con)
    for (i in seq_len(nrow(resumen_calidad)))
      writeLines(sprintf("    %-6s  H = %.3f  d = %.3f  %s",
                         resumen_calidad$id_cuenca[i],
                         resumen_calidad$H[i], resumen_calidad$d[i],
                         if (resumen_calidad$H[i] > UMBRAL_H_ARFIMA)
                           "[ARFIMA]" else "[AR(1)]"), con)
    writeLines("", con)
    writeLines(sep, con)
    writeLines(paste("  Generado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                     "| v1.0.0"), con)
    writeLines(sep, con)
  }, finally = close(con))
}

# Informe B — Resultados y aplicabilidad --------------------------------------
escribir_informe_b <- function() {
  sep  <- strrep("═", 67)
  sep2 <- strrep("─", 67)
  con  <- file(file.path(dir_base, "B_resultados_y_aplicabilidad.txt"),
               "w", encoding = "UTF-8")
  tryCatch({
    writeLines(sep, con)
    writeLines("  INFORME B — RESULTADOS Y GUÍA DE APLICABILIDAD", con)
    writeLines(paste("  Proyecto:", nombre_proj, "v1.0.0 | Fecha:", Sys.Date()), con)
    writeLines("  Región: Sistema Río Cauca — Andes colombianos | Régimen bimodal", con)
    writeLines(sep, con)
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  B1. CAUDALES MEDIOS ESTIMADOS (IC 90%)", con)
    writeLines(sep2, con)
    writeLines("  Cuenca   μ (m³/s)   IC inf.   IC sup.   Región", con)
    writeLines(strrep("─", 55), con)
    for (i in seq_len(nrow(cuencas_pred)))
      writeLines(sprintf("  %-6s   %8.2f   %8.2f   %8.2f   R%d",
                         cuencas_pred$id_cuenca[i],
                         cuencas_pred$mu_estimado[i],
                         cuencas_pred$mu_ic_inf[i],
                         cuencas_pred$mu_ic_sup[i],
                         cuencas_pred$region[i]), con)
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  B2. SERIES SINTÉTICAS GENERADAS", con)
    writeLines(sep2, con)
    writeLines(paste("  Período:", as.character(fecha_ini_cfg), "→",
                     as.character(fecha_fin_cfg), "(", N_DIAS_SIM, "días)"), con)
    writeLines("  Cuenca   H regional   d ARFIMA   Método     μ (m³/s)", con)
    writeLines(strrep("─", 55), con)
    for (i in seq_len(nrow(cuencas_pred))) {
      met <- if (cuencas_pred$H_regional[i] > UMBRAL_H_ARFIMA) "ARFIMA" else "AR(1) "
      writeLines(sprintf("  %-6s   %.3f        %.3f      %s     %8.2f",
                         cuencas_pred$id_cuenca[i],
                         cuencas_pred$H_regional[i],
                         cuencas_pred$d_regional[i],
                         met, cuencas_pred$mu_estimado[i]), con)
    }
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  B3. VALIDACIÓN LEAVE-ONE-OUT (LOO)", con)
    writeLines(sep2, con)
    writeLines(paste("  NSE   :", round(NSE_global, 4),
                     "  [ref: > 0.65 aceptable]"), con)
    writeLines(paste("  KGE   :", round(KGE_global, 4),
                     "  [ref: > 0.60 aceptable]"), con)
    writeLines(paste("  PBIAS :", round(metricas_globales$PBIAS, 2), "%",
                     "  [ref: |PBIAS| < 25%]"), con)
    writeLines(paste("  r Spearman:", round(metricas_globales$r_Spearman, 4)), con)
    writeLines("", con)
    writeLines("  Intervalos de confianza 95% (bootstrap):", con)
    writeLines(paste("  NSE  : [", round(boot_resumen$NSE$ic_inf, 3), ",",
                     round(boot_resumen$NSE$ic_sup, 3), "]"), con)
    writeLines(paste("  KGE  : [", round(boot_resumen$KGE$ic_inf, 3), ",",
                     round(boot_resumen$KGE$ic_sup, 3), "]"), con)
    writeLines(paste("  PBIAS: [", round(boot_resumen$PBIAS$ic_inf, 2), "%,",
                     round(boot_resumen$PBIAS$ic_sup, 2), "%]"), con)
    writeLines("", con)
    writeLines("  Error por segmento de FDC:", con)
    for (i in seq_len(nrow(metricas_por_segmento)))
      writeLines(sprintf("    %-24s  NSE=%6.3f  KGE=%6.3f  MAPE=%5.1f%%",
                         metricas_por_segmento$segmento[i],
                         metricas_por_segmento$NSE[i],
                         metricas_por_segmento$KGE[i],
                         metricas_por_segmento$MAPE[i]), con)
    writeLines("", con)
    writeLines(sep2, con)
    writeLines(paste("  CALIFICACIÓN GLOBAL:", calidad), con)
    writeLines(sep2, con)
    writeLines("  Guía de uso según segmento de FDC:", con)
    writeLines("    F = 0.10 (Tr ≈ 10a)  → Verificar con análisis de frecuencias", con)
    writeLines("    F = 0.50 (Q medio)   → Mayor confiabilidad; uso directo", con)
    writeLines("    F = 0.85 (Q bajo)    → Estudios de disponibilidad hídrica", con)
    writeLines("    F = 0.95 (Q95)       → Usar IC inferior (estimación conservadora)", con)
    writeLines("", con)
    nse_bajos <- metricas_por_segmento %>%
      filter(segmento == "Bajos (F>0.80)") %>% pull(NSE)
    nse_altos <- metricas_por_segmento %>%
      filter(segmento == "Altos (F<0.20)") %>% pull(NSE)
    if (length(nse_bajos) > 0 && !is.na(nse_bajos) && nse_bajos < 0.50)
      writeLines("    ⚠ NSE < 0.50 en caudales bajos: incertidumbre alta en estia\u00eje.", con)
    if (length(nse_altos) > 0 && !is.na(nse_altos) && nse_altos < 0.50)
      writeLines("    ⚠ NSE < 0.50 en caudales altos: no usar en diseño hidráulico.", con)
    if (PBIAS_global > 25)
      writeLines(paste("    ⚠ PBIAS =", round(metricas_globales$PBIAS, 1),
                       "%: sesgo sistemático. Revisar modelo μ."), con)
    writeLines("", con)
    writeLines(sep2, con)
    writeLines("  B4. ARCHIVOS GENERADOS", con)
    writeLines(sep2, con)
    writeLines(paste("  Directorio:", dir_base), con)
    writeLines(paste("  •", nombre_proj,
                     "_1.0.0_resultados.xlsx  — 14 hojas de resultados"), con)
    writeLines("  • series_sinteticas_todas.csv   — series diarias, todas las cuencas", con)
    writeLines("  • series_sinteticas/             — un CSV por cuenca estimada", con)
    writeLines("  • A_diagnostico_tecnico.txt      — diagnóstico del proceso (Informe A)", con)
    writeLines("  • B_resultados_y_aplicabilidad   — resultados para el usuario (Informe B)", con)
    writeLines("  • graficos/G1_FDC_dimensionales  — FDC con IC 90%", con)
    writeLines("  • graficos/G2_FDC_regionales     — FDC adimensionales por región", con)
    writeLines("  • graficos/G3_Error_LOO          — MAPE y NSE a lo largo de la FDC", con)
    writeLines("  • graficos/G4_QQ_regional        — Q-Q por región (LOO)", con)
    writeLines("  • graficos/G5_Dendrograma_Ward   — árbol de clustering y silhouette", con)
    writeLines("  • graficos/G6_Serie_*            — ejemplo de serie sintética", con)
    writeLines("  • graficos/G7_Diagnostico_mu     — residuos y Cook del modelo μ", con)
    writeLines("  • graficos/G8_Escorrentia        — q específica por cuenca (L/s/km²)", con)
    writeLines("  • graficos/G9_Hurst_cuencas      — exponente Hurst y umbral ARFIMA", con)
    writeLines("", con)
    writeLines(sep, con)
    writeLines(paste("  Generado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                     "| v1.0.0"), con)
    writeLines(sep, con)
  }, finally = close(con))
}

escribir_informe_a()
escribir_informe_b()

# Resumen final en consola ----------------------------------------------------
cat("\n")
cat("╬", strrep("═", 66), "╬\n", sep = "")
cat("║  ✅  REGIONALIZACIÓN FDC v1.0.0 COMPLETADA — Sistema Río Cauca          ║\n")
cat("╬", strrep("═", 66), "╬\n\n", sep = "")
cat("[Grafico] RESULTADOS:\n")
cat("   Región de estudio     : Sistema Río Cauca (Andes colombianos)\n")
cat("   Régimen hidrológico   : Bimodal (2 húmedos + 2 secos/año)\n")
cat("   Regiones (k)          :", k_final, "\n")
cat("   Silhouette            :", round(sil_medio_final, 3), "\n")
cat("   R² modelo μ (adj)     :", round(r2_adj_mu,     3), "\n")
cat("   NSE LOO               :", round(NSE_global,    3), "\n")
cat("   KGE LOO               :", round(KGE_global,    3), "\n")
cat("   PBIAS                 :", round(metricas_globales$PBIAS, 1), "%\n")
cat("   ARFIMA / AR(1)        :",
    sum(cuencas_pred$H_regional > UMBRAL_H_ARFIMA), "/",
    sum(cuencas_pred$H_regional <= UMBRAL_H_ARFIMA), "cuencas\n")
cat("   Calificación          :", calidad, "\n\n")
cat("[Archivos] ARCHIVOS en:", dir_base, "\n")
cat("   •", nombre_proj, "_1.0.0_resultados.xlsx (14 hojas)\n")
cat("   • series_sinteticas_todas.csv\n")
cat("   • A_diagnostico_tecnico.txt\n")
cat("   • B_resultados_y_aplicabilidad.txt\n")
cat("   • graficos/ (G1–G9)\n")
cat("   • series_sinteticas/ (", length(series_sinteticas), "CSV)\n\n")
cat("✅ Proceso finalizado exitosamente.\n")
