# ============================================================
# MACROECONOMÍA - COLOMBIA
# EFECTO DE LA TASA CDT 90 DÍAS SOBRE LA INFLACIÓN
#
# MODELOS:
#   1. ARIMAX
#   2. VAR
#
# FRECUENCIA:
#   Mensual
#
# VARIABLE DEPENDIENTE:
#   Inflación anual = variación anual del IPC
#
# VARIABLE DE INTERÉS:
#   Tasa CDT a 90 días
#
# MUESTRA:
#   Abril 2008 - Julio 2026
#
# PRONÓSTICO:
#   Agosto - Diciembre 2026
#
# NOTA METODOLÓGICA:
#   La causalidad de Granger se interpreta como causalidad
#   predictiva y NO como causalidad estructural.
# ============================================================


# ============================================================
# 1. LIMPIAR ENTORNO
# ============================================================

rm(list = ls())

cat("\n")
cat("====================================================\n")
cat("MACROECONOMÍA - COLOMBIA\n")
cat("CDT 90 DÍAS E INFLACIÓN\n")
cat("====================================================\n")


# ============================================================
# 2. PAQUETES
# ============================================================

paquetes <- c(
  "tidyverse",
  "lubridate",
  "readxl",
  "forecast",
  "tseries",
  "urca",
  "vars",
  "lmtest",
  "readr",
  "zoo"
)

instalar <- paquetes[
  !(paquetes %in% installed.packages()[, "Package"])
]

if (length(instalar) > 0) {

  install.packages(
    instalar,
    dependencies = TRUE
  )

}

suppressPackageStartupMessages({

  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(forecast)
  library(tseries)
  library(urca)
  library(vars)
  library(lmtest)
  library(readr)
  library(zoo)

})


# ============================================================
# 3. CONFIGURACIÓN
# ============================================================

setwd(
  "/Users/sergiodeluquez/Downloads/Macroeconomia"
)

dir.create(
  "resultados",
  showWarnings = FALSE
)


# ============================================================
# 4. ARCHIVOS
# ============================================================

archivo_ipc <- "datos/anex-IPC-Indices-jul2026.xlsx"

archivo_cdt <- "datos/cdt_a_90_dias.csv"


# ============================================================
# 5. FUNCIÓN AUXILIAR PARA CONVERTIR NÚMEROS
# ============================================================

convertir_numero <- function(x) {

  x <- as.character(x)

  x <- trimws(x)

  x[x %in% c(
    "",
    "NA",
    "N/A",
    "-",
    "..",
    "..."
  )] <- NA

  x <- gsub(
    "\\s+",
    "",
    x
  )

  # Casos tipo 1.234,56
  x <- ifelse(
    grepl(",", x) & grepl("\\.", x),
    gsub("\\.", "", x),
    x
  )

  # Decimal con coma
  x <- gsub(
    ",",
    ".",
    x
  )

  suppressWarnings(
    as.numeric(x)
  )
}


# ============================================================
# 6. VERIFICAR ARCHIVOS
# ============================================================

if (!file.exists(archivo_ipc)) {

  stop(
    paste0(
      "No existe el archivo IPC: ",
      archivo_ipc
    )
  )

}

if (!file.exists(archivo_cdt)) {

  stop(
    paste0(
      "No existe el archivo CDT: ",
      archivo_cdt
    )
  )

}


# ============================================================
# 7. IMPORTAR IPC
# ============================================================

cat("\n")
cat("====================================================\n")
cat("IMPORTANDO IPC\n")
cat("====================================================\n")


ipc_excel <- readxl::read_excel(
  archivo_ipc,
  sheet = "IndicesIPC",
  col_names = FALSE
)


cat("\nDimensiones del Excel IPC:\n")

print(
  dim(ipc_excel)
)


# ============================================================
# 8. BUSCAR FILA DE MESES
# ============================================================

fila_mes <- which(
  trimws(
    as.character(
      ipc_excel[[1]]
    )
  ) == "Mes"
)


cat("\nFila de meses encontrada:\n")

print(
  fila_mes
)


if (length(fila_mes) != 1) {

  stop(
    "No fue posible identificar exactamente una fila 'Mes'."
  )

}


# ============================================================
# 9. LEER ENCABEZADOS
# ============================================================

encabezados <- as.character(
  unlist(
    ipc_excel[fila_mes, ],
    use.names = FALSE
  )
)

encabezados <- trimws(
  encabezados
)

encabezados[1] <- "Mes"


# ============================================================
# 10. IDENTIFICAR AÑOS
# ============================================================

anios <- suppressWarnings(
  as.numeric(
    encabezados
  )
)

columnas_anio <- which(
  !is.na(anios) &
    anios >= 1900 &
    anios <= 2100
)


cat("\nAños encontrados:\n")

print(
  anios[columnas_anio]
)


if (length(columnas_anio) < 10) {

  stop(
    "Se encontraron muy pocas columnas de años en el IPC."
  )

}


# ============================================================
# 11. EXTRAER TABLA IPC
# ============================================================

ipc_raw <- ipc_excel[
  (fila_mes + 1):nrow(ipc_excel),
  ,
  drop = FALSE
]


names(ipc_raw) <- paste0(
  "col_",
  seq_len(ncol(ipc_raw))
)

names(ipc_raw)[1] <- "Mes"


# ============================================================
# 12. CONSTRUIR IPC LIMPIO
# ============================================================

ipc_limpio <- ipc_raw[
  ,
  c(1, columnas_anio),
  drop = FALSE
]


names(ipc_limpio) <- c(
  "Mes",
  as.character(
    anios[columnas_anio]
  )
)


# ============================================================
# 13. CONSERVAR LOS 12 MESES
# ============================================================

meses <- c(
  "Enero",
  "Febrero",
  "Marzo",
  "Abril",
  "Mayo",
  "Junio",
  "Julio",
  "Agosto",
  "Septiembre",
  "Octubre",
  "Noviembre",
  "Diciembre"
)


ipc_limpio <- ipc_limpio %>%

  mutate(
    Mes = trimws(
      as.character(Mes)
    )
  ) %>%

  filter(
    Mes %in% meses
  )


# ============================================================
# 14. CONVERTIR IPC A NUMÉRICO
# ============================================================

columnas_anios_nombres <- as.character(
  anios[columnas_anio]
)


ipc_limpio <- ipc_limpio %>%

  mutate(
    across(
      all_of(
        columnas_anios_nombres
      ),
      convertir_numero
    )
  )


cat("\nTabla IPC limpia:\n")

print(
  head(
    ipc_limpio,
    15
  )
)


# ============================================================
# 15. VALIDAR IPC 2026
# ============================================================

if (!"2026" %in% names(ipc_limpio)) {

  stop(
    "No existe la columna 2026 en el IPC."
  )

}

if (!is.numeric(ipc_limpio[["2026"]])) {

  stop(
    "La columna 2026 no es numérica."
  )

}


# ============================================================
# 16. PASAR IPC A FORMATO LARGO
# ============================================================

ipc <- ipc_limpio %>%

  pivot_longer(

    cols = all_of(
      columnas_anios_nombres
    ),

    names_to = "anio",

    values_to = "ipc"

  ) %>%

  mutate(

    anio = as.numeric(anio),

    ipc = as.numeric(ipc)

  ) %>%

  filter(

    !is.na(anio),

    !is.na(ipc)

  )


# ============================================================
# 17. CREAR FECHA
# ============================================================

ipc <- ipc %>%

  mutate(

    mes_num = match(
      Mes,
      meses
    ),

    fecha = as.Date(
      sprintf(
        "%04d-%02d-01",
        anio,
        mes_num
      )
    )

  ) %>%

  dplyr::select(
    fecha,
    ipc
  ) %>%

  arrange(
    fecha
  )


# ============================================================
# 18. VALIDAR SERIE IPC
# ============================================================

cat("\n")
cat("====================================================\n")
cat("IPC PROCESADO\n")
cat("====================================================\n")


cat("\nPrimeras observaciones:\n")

print(
  head(
    ipc,
    15
  )
)


cat("\nÚltimas observaciones:\n")

print(
  tail(
    ipc,
    15
  )
)


cat("\nÚltima fecha disponible:\n")

print(
  max(
    ipc$fecha
  )
)


# ============================================================
# 19. CALCULAR INFLACIÓN ANUAL
# ============================================================

ipc <- ipc %>%

  arrange(
    fecha
  ) %>%

  mutate(

    inflacion =
      100 *
      (
        ipc /
          lag(
            ipc,
            12
          ) -
          1
      )

  )


# ============================================================
# 20. VALIDAR INFLACIÓN
# ============================================================

if (
  all(
    is.na(
      ipc$inflacion
    )
  )
) {

  stop(
    "No fue posible calcular la inflación anual."
  )

}


cat("\nInflación calculada:\n")

print(
  tail(
    ipc,
    15
  )
)


# ============================================================
# 21. IMPORTAR CDT
# ============================================================

cat("\n")
cat("====================================================\n")
cat("IMPORTANDO CDT 90 DÍAS\n")
cat("====================================================\n")


cdt_raw <- readr::read_csv(
  archivo_cdt,
  show_col_types = FALSE
)


cat("\nColumnas CDT:\n")

print(
  names(cdt_raw)
)


# ============================================================
# 22. VALIDAR ESTRUCTURA CDT
# ============================================================

if (ncol(cdt_raw) < 2) {

  stop(
    "El archivo CDT debe tener al menos dos columnas."
  )

}


nombre_original_cdt <- names(
  cdt_raw
)[2]


cat("\nVariable CDT encontrada:\n")

print(
  nombre_original_cdt
)


# ============================================================
# 23. EVITAR USAR MONTOS
# ============================================================

if (
  grepl(
    "monto",
    nombre_original_cdt,
    ignore.case = TRUE
  )
) {

  stop(
    paste0(
      "\nERROR: la variable seleccionada es MONTO y no TASA.\n\n",
      "Debe utilizarse la tasa CDT a 90 días."
    )
  )

}


# ============================================================
# 24. RENOMBRAR VARIABLES
# ============================================================

names(cdt_raw)[1:2] <- c(
  "fecha",
  "cdt90"
)


# ============================================================
# 25. CONVERTIR FECHA Y TASA
# ============================================================

cdt <- cdt_raw %>%

  mutate(

    fecha_original = fecha,

    fecha = as.Date(
      fecha,
      tryFormats = c(
        "%Y-%m-%d",
        "%d/%m/%Y",
        "%m/%d/%Y",
        "%Y/%m/%d"
      )
    ),

    cdt90 = convertir_numero(
      cdt90
    )

  ) %>%

  arrange(
    fecha
  )


# ============================================================
# 26. VALIDAR CDT
# ============================================================

if (
  all(
    is.na(
      cdt$fecha
    )
  )
) {

  stop(
    "No fue posible convertir las fechas del CDT."
  )

}

if (
  all(
    is.na(
      cdt$cdt90
    )
  )
) {

  stop(
    "No fue posible convertir la tasa CDT."
  )

}


cat("\nPrimera fecha CDT:\n")

print(
  min(
    cdt$fecha,
    na.rm = TRUE
  )
)


cat("\nÚltima fecha CDT:\n")

print(
  max(
    cdt$fecha,
    na.rm = TRUE
  )
)


cat("\nValores faltantes CDT:\n")

print(
  sum(
    is.na(
      cdt$cdt90
    )
  )
)


# ============================================================
# 27. CONTROL DE RANGO CDT
# ============================================================

mediana_cdt <- median(
  cdt$cdt90,
  na.rm = TRUE
)


cat("\nMediana CDT:\n")

print(
  mediana_cdt
)


if (
  is.finite(mediana_cdt) &&
  mediana_cdt > 100
) {

  stop(
    "Los valores de CDT parecen estar expresados incorrectamente."
  )

}


# ============================================================
# 28. CDT DIARIO → MENSUAL
# ============================================================

cdt_mensual <- cdt %>%

  mutate(

    fecha = floor_date(
      fecha,
      unit = "month"
    )

  ) %>%

  group_by(
    fecha
  ) %>%

  summarise(

    # n_obs debe calcularse ANTES de resumir cdt90, porque summarise()
    # reutiliza el valor ya resumido en las expresiones posteriores.
    n_obs = sum(
      !is.na(
        cdt90
      )
    ),

    cdt90 = ifelse(
      n_obs == 0,
      NA_real_,
      mean(
        cdt90,
        na.rm = TRUE
      )
    ),

    .groups = "drop"

  ) %>%

  dplyr::relocate(
    cdt90,
    .after = fecha
  )


# ============================================================
# 29. VALIDAR CDT MENSUAL
# ============================================================

cat("\nCDT mensual:\n")

print(
  tail(
    cdt_mensual,
    15
  )
)


if (
  any(
    is.na(
      cdt_mensual$cdt90
    )
  )
) {

  warning(
    "Existen meses del CDT sin observaciones."
  )

}


# ============================================================
# 30. UNIR IPC + CDT
# ============================================================

base <- ipc %>%

  left_join(
    cdt_mensual,
    by = "fecha"
  ) %>%

  arrange(
    fecha
  )


# ============================================================
# 31. BASE FINAL DEL MODELO
# ============================================================

base_modelo <- base %>%

  filter(

    fecha >= as.Date(
      "2008-04-01"
    ),

    fecha <= as.Date(
      "2026-07-01"
    )

  ) %>%

  drop_na(

    inflacion,

    cdt90

  )


# ============================================================
# 32. VALIDAR MUESTRA
# ============================================================

cat("\n")
cat("====================================================\n")
cat("BASE FINAL DEL MODELO\n")
cat("====================================================\n")


cat("\nNúmero de observaciones:\n")

print(
  nrow(
    base_modelo
  )
)


cat("\nPrimera fecha:\n")

print(
  min(
    base_modelo$fecha
  )
)


cat("\nÚltima fecha:\n")

print(
  max(
    base_modelo$fecha
  )
)


cat("\nValores faltantes:\n")

print(
  colSums(
    is.na(
      base_modelo
    )
  )
)


if (
  nrow(base_modelo) < 100
) {

  stop(
    "Número insuficiente de observaciones."
  )

}


# ============================================================
# 33. CONTROL DE FRECUENCIA MENSUAL
# ============================================================

fechas_esperadas <- seq(

  from =
    min(
      base_modelo$fecha
    ),

  to =
    max(
      base_modelo$fecha
    ),

  by =
    "month"

)


if (
  length(fechas_esperadas) !=
  nrow(base_modelo)
) {

  warning(
    "La base contiene meses faltantes o fechas duplicadas."
  )

}


# ============================================================
# 34. GUARDAR BASE FINAL
# ============================================================

write.csv(

  base_modelo,

  "resultados/base_modelo_final.csv",

  row.names = FALSE

)


# ============================================================
# 35. ESTADÍSTICOS DESCRIPTIVOS
# ============================================================

cat("\n")
cat("====================================================\n")
cat("ESTADÍSTICOS DESCRIPTIVOS\n")
cat("====================================================\n")


cat("\nInflación:\n")

print(
  summary(
    base_modelo$inflacion
  )
)


cat("\nCDT90:\n")

print(
  summary(
    base_modelo$cdt90
  )
)


cat("\nDesviación estándar inflación:\n")

print(
  sd(
    base_modelo$inflacion
  )
)


cat("\nDesviación estándar CDT:\n")

print(
  sd(
    base_modelo$cdt90
  )
)


# ============================================================
# 36. CORRELACIÓN CONTEMPORÁNEA
# ============================================================

correlacion <- cor(

  base_modelo$inflacion,

  base_modelo$cdt90,

  use = "complete.obs"

)


cat("\nCorrelación inflación-CDT90:\n")

print(
  correlacion
)


# ============================================================
# 37. GRÁFICO INFLACIÓN
# ============================================================

grafico_inflacion <- ggplot(

  base_modelo,

  aes(
    x = fecha,
    y = inflacion
  )

) +

  geom_line(
    color = "steelblue",
    linewidth = 0.8
  ) +

  labs(

    title =
      "Inflación anual en Colombia",

    subtitle =
      "Variación anual del IPC",

    x =
      "Fecha",

    y =
      "Inflación (%)"

  ) +

  theme_minimal()


ggsave(

  "resultados/inflacion.png",

  grafico_inflacion,

  width = 10,

  height = 6

)


# ============================================================
# 38. GRÁFICO CDT
# ============================================================

grafico_cdt <- ggplot(

  base_modelo,

  aes(
    x = fecha,
    y = cdt90
  )

) +

  geom_line(

    color = "darkred",

    linewidth = 0.8

  ) +

  labs(

    title =
      "Tasa CDT a 90 días",

    subtitle =
      "Promedio mensual de la tasa diaria",

    x =
      "Fecha",

    y =
      "Tasa (%)"

  ) +

  theme_minimal()


ggsave(

  "resultados/cdt90.png",

  grafico_cdt,

  width = 10,

  height = 6

)


# ============================================================
# 39. CONSTRUIR SERIES TEMPORALES
# ============================================================

fecha_inicio <- min(
  base_modelo$fecha
)


inflacion_ts <- ts(

  base_modelo$inflacion,

  start = c(
    year(fecha_inicio),
    month(fecha_inicio)
  ),

  frequency = 12

)


cdt_ts <- ts(

  base_modelo$cdt90,

  start = c(
    year(fecha_inicio),
    month(fecha_inicio)
  ),

  frequency = 12

)


# ============================================================
# 40. ADF - NIVEL
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PRUEBAS DE ESTACIONARIEDAD\n")
cat("====================================================\n")


adf_inflacion <- suppressWarnings(
  tseries::adf.test(
    inflacion_ts
  )
)


adf_cdt <- suppressWarnings(
  tseries::adf.test(
    cdt_ts
  )
)


cat("\nADF - Inflación:\n")

print(
  adf_inflacion
)


cat("\nADF - CDT90:\n")

print(
  adf_cdt
)


# ============================================================
# 41. KPSS - NIVEL
# ============================================================

kpss_inflacion <- suppressWarnings(
  tseries::kpss.test(
    inflacion_ts
  )
)


kpss_cdt <- suppressWarnings(
  tseries::kpss.test(
    cdt_ts
  )
)


cat("\nKPSS - Inflación:\n")

print(
  kpss_inflacion
)


cat("\nKPSS - CDT90:\n")

print(
  kpss_cdt
)


# ============================================================
# 42. PRIMERAS DIFERENCIAS
# ============================================================

inflacion_diff <- diff(
  inflacion_ts
)

cdt_diff <- diff(
  cdt_ts
)


# ============================================================
# 43. ADF PRIMERAS DIFERENCIAS
# ============================================================

adf_inflacion_diff <- suppressWarnings(
  tseries::adf.test(
    inflacion_diff
  )
)

adf_cdt_diff <- suppressWarnings(
  tseries::adf.test(
    cdt_diff
  )
)


cat("\nADF primera diferencia inflación:\n")

print(
  adf_inflacion_diff
)


cat("\nADF primera diferencia CDT:\n")

print(
  adf_cdt_diff
)


# ============================================================
# 44. ACF / PACF
# ============================================================

png(

  "resultados/acf_inflacion.png",

  width = 1000,

  height = 700

)

forecast::Acf(

  inflacion_ts,

  main =
    "ACF - Inflación"

)

dev.off()


png(

  "resultados/pacf_inflacion.png",

  width = 1000,

  height = 700

)

forecast::Pacf(

  inflacion_ts,

  main =
    "PACF - Inflación"

)

dev.off()


png(

  "resultados/acf_cdt90.png",

  width = 1000,

  height = 700

)

forecast::Acf(

  cdt_ts,

  main =
    "ACF - CDT90"

)

dev.off()


png(

  "resultados/pacf_cdt90.png",

  width = 1000,

  height = 700

)

forecast::Pacf(

  cdt_ts,

  main =
    "PACF - CDT90"

)

dev.off()


# ============================================================
# 45. ESTIMAR ARIMAX
# ============================================================

cat("\n")
cat("====================================================\n")
cat("ESTIMANDO ARIMAX\n")
cat("====================================================\n")


modelo_arimax <- forecast::auto.arima(

  inflacion_ts,

  xreg =
    matrix(
      cdt_ts,
      ncol = 1,
      dimnames = list(
        NULL,
        "cdt90"
      )
    ),

  seasonal = TRUE,

  stepwise = FALSE,

  approximation = FALSE,

  allowdrift = FALSE

)


cat("\nModelo ARIMAX:\n")

print(
  summary(
    modelo_arimax
  )
)


# ============================================================
# 46. DIAGNÓSTICO ARIMAX
# ============================================================

cat("\n")
cat("====================================================\n")
cat("DIAGNÓSTICO ARIMAX\n")
cat("====================================================\n")


residuos_arimax <- residuals(
  modelo_arimax
)


# ============================================================
# 47. LJUNG-BOX ARIMAX
# ============================================================

coeficientes_arimax <- coef(
  modelo_arimax
)

n_coef_arima <- length(
  coeficientes_arimax
)

if (
  "cdt90" %in%
  names(
    coeficientes_arimax
  )
) {

  n_coef_arima <-
    n_coef_arima - 1

}


# Evitar que fitdf sea igual o superior al rezago
ljung_lag <- max(
  12,
  2 * n_coef_arima + 1
)

if (
  ljung_lag >=
  length(
    residuos_arimax
  )
) {

  ljung_lag <-
    floor(
      length(
        residuos_arimax
      ) / 4
    )

}

if (
  ljung_lag <= n_coef_arima
) {

  ljung_lag <-
    n_coef_arima + 1

}


ljung_arimax <- Box.test(

  residuos_arimax,

  lag =
    ljung_lag,

  type =
    "Ljung-Box",

  fitdf =
    n_coef_arima

)


cat("\nLjung-Box ARIMAX:\n")

print(
  ljung_arimax
)


# Diagnóstico gráfico
png(

  "resultados/residuos_arimax.png",

  width = 1200,

  height = 800

)

forecast::checkresiduals(
  modelo_arimax
)

dev.off()


# ============================================================
# 48. MODELO PARA CDT
# ============================================================

cat("\n")
cat("====================================================\n")
cat("MODELO CDT90\n")
cat("====================================================\n")


modelo_cdt <- forecast::auto.arima(

  cdt_ts,

  seasonal = TRUE,

  stepwise = FALSE,

  approximation = FALSE

)


print(
  summary(
    modelo_cdt
  )
)


# ============================================================
# 49. PRONÓSTICO CDT
# ============================================================

pronostico_cdt <- forecast::forecast(

  modelo_cdt,

  h = 5

)


cdt_futuro <- as.numeric(
  pronostico_cdt$mean
)


cat("\nPronóstico CDT:\n")

print(
  pronostico_cdt
)


write.csv(

  data.frame(
    fecha = seq(
      as.Date("2026-08-01"),
      by = "month",
      length.out = 5
    ),
    cdt90_pronosticado = cdt_futuro
  ),

  "resultados/pronostico_cdt.csv",

  row.names = FALSE

)


# ============================================================
# 50. FECHAS FUTURAS
# ============================================================

fechas_futuras <- seq(

  from =
    as.Date(
      "2026-08-01"
    ),

  by =
    "month",

  length.out =
    5

)


# ============================================================
# 51. PRONÓSTICO ARIMAX
# ============================================================

pronostico_arimax <- forecast::forecast(

  modelo_arimax,

  xreg =
    matrix(
      cdt_futuro,
      ncol = 1,
      dimnames = list(
        NULL,
        "cdt90"
      )
    ),

  h = 5

)


pronosticos_arimax <- data.frame(

  fecha =
    fechas_futuras,

  mes =
    format(
      fechas_futuras,
      "%B"
    ),

  pronostico =
    as.numeric(
      pronostico_arimax$mean
    ),

  limite_inferior_95 =
    as.numeric(
      pronostico_arimax$lower[, 2]
    ),

  limite_superior_95 =
    as.numeric(
      pronostico_arimax$upper[, 2]
    )

)


cat("\nPronóstico ARIMAX:\n")

print(
  pronosticos_arimax
)


write.csv(

  pronosticos_arimax,

  "resultados/pronosticos_arimax.csv",

  row.names = FALSE

)


# ============================================================
# 52. PREPARAR VAR
# ============================================================

datos_var <- base_modelo %>%

  dplyr::select(

    fecha,

    inflacion,

    cdt90

  ) %>%

  drop_na()


datos_var_ts <- ts(

  datos_var[
    ,
    c(
      "inflacion",
      "cdt90"
    )
  ],

  start = c(

    year(
      min(
        datos_var$fecha
      )
    ),

    month(
      min(
        datos_var$fecha
      )
    )

  ),

  frequency = 12

)


# ============================================================
# 53. SELECCIÓN DE REZAGOS VAR
# ============================================================

cat("\n")
cat("====================================================\n")
cat("SELECCIÓN DE REZAGOS VAR\n")
cat("====================================================\n")


seleccion_lags <- vars::VARselect(

  datos_var_ts,

  lag.max = 12,

  type = "const"

)


print(
  seleccion_lags$selection
)


print(
  seleccion_lags$criteria
)


# ============================================================
# 54. REZAGOS BIC
# ============================================================

p_bic <- as.numeric(

  seleccion_lags$selection[
    "SC(n)"
  ]

)


cat("\nRezagos seleccionados por BIC:\n")

print(
  p_bic
)


if (
  is.na(p_bic) ||
  p_bic < 1
) {

  stop(
    "No fue posible determinar correctamente el número de rezagos del VAR."
  )

}


# ============================================================
# 55. ESTIMAR VAR
# ============================================================

modelo_var <- vars::VAR(

  datos_var_ts,

  p =
    p_bic,

  type =
    "const"

)


cat("\n")
cat("====================================================\n")
cat("MODELO VAR\n")
cat("====================================================\n")


print(
  summary(
    modelo_var
  )
)


# ============================================================
# 56. ESTABILIDAD VAR
# ============================================================

raices_var <- vars::roots(
  modelo_var
)


var_estable <- all(
  abs(
    raices_var
  ) < 1
)


cat("\nRaíces VAR:\n")

print(
  raices_var
)


cat("\n¿VAR estable?\n")

print(
  var_estable
)


# ============================================================
# 57. AUTOCORRELACIÓN VAR
# ============================================================

serial_var <- vars::serial.test(

  modelo_var,

  lags.pt = 12,

  type =
    "PT.asymptotic"

)


cat("\nAutocorrelación VAR:\n")

print(
  serial_var
)


# ============================================================
# 58. ARCH VAR
# ============================================================

arch_var <- vars::arch.test(

  modelo_var,

  lags.multi = 5

)


cat("\nARCH VAR:\n")

print(
  arch_var
)


# ============================================================
# 59. NORMALIDAD VAR
# ============================================================

normalidad_var <- vars::normality.test(

  modelo_var

)


cat("\nNormalidad VAR:\n")

print(
  normalidad_var
)


# ============================================================
# 60. CAUSALIDAD DE GRANGER
# ============================================================

cat("\n")
cat("====================================================\n")
cat("CAUSALIDAD DE GRANGER\n")
cat("====================================================\n")


granger_cdt <- vars::causality(

  modelo_var,

  cause =
    "cdt90"

)


granger_inflacion <- vars::causality(

  modelo_var,

  cause =
    "inflacion"

)


cat("\nCDT90 -> Inflación:\n")

print(
  granger_cdt
)


cat("\nInflación -> CDT90:\n")

print(
  granger_inflacion
)


# ============================================================
# 61. EXTRAER CORRECTAMENTE RESULTADOS DE GRANGER
# ============================================================

extraer_granger <- function(objeto) {

  resultado <- objeto$Granger

  # Convertir de manera robusta
  # dependiendo de la versión del paquete vars.

  estadistico <- NA_real_
  p_value <- NA_real_

  if (
    !is.null(
      resultado$statistic
    )
  ) {

    estadistico <-
      as.numeric(
        resultado$statistic[1]
      )

  }

  if (
    !is.null(
      resultado$p.value
    )
  ) {

    p_value <-
      as.numeric(
        resultado$p.value[1]
      )

  }

  # Algunas versiones pueden almacenar
  # el resultado dentro de una lista adicional.

  if (
    is.na(estadistico) &&
    !is.null(
      resultado$F
    )
  ) {

    estadistico <-
      as.numeric(
        resultado$F[1]
      )

  }

  if (
    is.na(p_value) &&
    !is.null(
      resultado$P
    )
  ) {

    p_value <-
      as.numeric(
        resultado$P[1]
      )

  }

  data.frame(

    estadistico_F =
      estadistico,

    p_value =
      p_value

  )

}


granger_cdt_resultado <-
  extraer_granger(
    granger_cdt
  )


granger_inflacion_resultado <-
  extraer_granger(
    granger_inflacion
  )


# ============================================================
# 62. IMPULSO RESPUESTA
# ============================================================

set.seed(12345)


irf_cdt <- vars::irf(

  modelo_var,

  impulse =
    "cdt90",

  response =
    "inflacion",

  n.ahead =
    24,

  boot =
    TRUE,

  runs =
    1000,

  ci =
    0.95

)


png(

  "resultados/impulso_respuesta_cdt_inflacion.png",

  width =
    1200,

  height =
    800

)


plot(

  irf_cdt,

  main =
    "Respuesta de la inflación ante un shock en CDT90"

)


dev.off()


# ============================================================
# 63. DESCOMPOSICIÓN DE VARIANZA
# ============================================================

fevd_var <- vars::fevd(

  modelo_var,

  n.ahead =
    24

)


png(

  "resultados/descomposicion_varianza.png",

  width =
    1200,

  height =
    800

)


plot(
  fevd_var
)


dev.off()


# ============================================================
# 64. VALIDACIÓN FUERA DE MUESTRA
# ============================================================

cat("\n")
cat("====================================================\n")
cat("VALIDACIÓN FUERA DE MUESTRA\n")
cat("====================================================\n")


h_validacion <- 12


n_total <- nrow(
  base_modelo
)


n_train <- n_total -
  h_validacion


base_train <- base_modelo[
  1:n_train,
  ]

base_test <- base_modelo[
  (n_train + 1):n_total,
  ]


cat("\nPeriodo entrenamiento:\n")

print(
  range(
    base_train$fecha
  )
)


cat("\nPeriodo validación:\n")

print(
  range(
    base_test$fecha
  )
)


# ============================================================
# 65. ARIMAX VALIDACIÓN
# ============================================================

inflacion_train_ts <- ts(

  base_train$inflacion,

  start = c(

    year(
      min(
        base_train$fecha
      )
    ),

    month(
      min(
        base_train$fecha
      )
    )

  ),

  frequency = 12

)


cdt_train_ts <- ts(

  base_train$cdt90,

  start = c(

    year(
      min(
        base_train$fecha
      )
    ),

    month(
      min(
        base_train$fecha
      )
    )

  ),

  frequency = 12

)


modelo_arimax_validacion <- forecast::auto.arima(

  inflacion_train_ts,

  xreg =
    matrix(
      cdt_train_ts,
      ncol = 1,
      dimnames = list(
        NULL,
        "cdt90"
      )
    ),

  seasonal = TRUE,

  stepwise = FALSE,

  approximation = FALSE,

  allowdrift = FALSE

)


pronostico_arimax_validacion <- forecast::forecast(

  modelo_arimax_validacion,

  xreg =
    matrix(
      base_test$cdt90,
      ncol = 1,
      dimnames = list(
        NULL,
        "cdt90"
      )
    ),

  h =
    h_validacion

)


# ============================================================
# 66. MÉTRICAS ARIMAX
# ============================================================

error_arimax <- base_test$inflacion -

  as.numeric(
    pronostico_arimax_validacion$mean
  )


rmse_arimax <- sqrt(
  mean(
    error_arimax^2,
    na.rm = TRUE
  )
)


mae_arimax <- mean(
  abs(
    error_arimax
  ),
  na.rm = TRUE
)


# ============================================================
# 67. VAR VALIDACIÓN
# ============================================================

datos_var_train <- base_train %>%

  dplyr::select(

    inflacion,

    cdt90

  )


datos_var_train_ts <- ts(

  datos_var_train,

  start = c(

    year(
      min(
        base_train$fecha
      )
    ),

    month(
      min(
        base_train$fecha
      )
    )

  ),

  frequency = 12

)


seleccion_lags_train <- vars::VARselect(

  datos_var_train_ts,

  lag.max = 12,

  type = "const"

)


p_bic_train <- as.numeric(

  seleccion_lags_train$selection[
    "SC(n)"
  ]

)


if (
  is.na(p_bic_train) ||
  p_bic_train < 1
) {

  stop(
    "No fue posible determinar los rezagos del VAR de validación."
  )

}


modelo_var_validacion <- vars::VAR(

  datos_var_train_ts,

  p =
    p_bic_train,

  type =
    "const"

)


# ============================================================
# 68. PRONÓSTICO VAR DE VALIDACIÓN
# ============================================================

pronostico_var_validacion <- predict(

  modelo_var_validacion,

  n.ahead =
    h_validacion,

  ci =
    0.95

)


pred_var_validacion <-

  pronostico_var_validacion$fcst$inflacion[
    ,
    "fcst"
  ]


# ============================================================
# 69. MÉTRICAS VAR
# ============================================================

error_var <- base_test$inflacion -

  as.numeric(
    pred_var_validacion
  )


rmse_var <- sqrt(
  mean(
    error_var^2,
    na.rm = TRUE
  )
)


mae_var <- mean(
  abs(
    error_var
  ),
  na.rm = TRUE
)


resultados_validacion <- data.frame(

  modelo =
    c(
      "ARIMAX",
      "VAR"
    ),

  RMSE =
    c(
      rmse_arimax,
      rmse_var
    ),

  MAE =
    c(
      mae_arimax,
      mae_var
    )

)


cat("\nResultados validación:\n")

print(
  resultados_validacion
)


write.csv(

  resultados_validacion,

  "resultados/validacion_modelos.csv",

  row.names = FALSE

)


# ============================================================
# 70. SELECCIONAR MEJOR MODELO
# ============================================================

mejor_modelo <- resultados_validacion$modelo[
  which.min(
    resultados_validacion$RMSE
  )
]


cat("\nMejor modelo según RMSE:\n")

print(
  mejor_modelo
)


# ============================================================
# 71. PRONÓSTICO VAR FINAL
# ============================================================

pronostico_var <- predict(

  modelo_var,

  n.ahead =
    5,

  ci =
    0.95

)


pronostico_var_inflacion <- data.frame(

  fecha =
    fechas_futuras,

  mes =
    format(
      fechas_futuras,
      "%B"
    ),

  pronostico =
    as.numeric(
      pronostico_var$fcst$inflacion[
        ,
        "fcst"
      ]
    ),

  limite_inferior_95 =
    as.numeric(
      pronostico_var$fcst$inflacion[
        ,
        "lower"
      ]
    ),

  limite_superior_95 =
    as.numeric(
      pronostico_var$fcst$inflacion[
        ,
        "upper"
      ]
    )

)


cat("\nPronóstico VAR:\n")

print(
  pronostico_var_inflacion
)


write.csv(

  pronostico_var_inflacion,

  "resultados/pronosticos_var.csv",

  row.names = FALSE

)


# ============================================================
# 72. COMPARACIÓN ARIMAX VS VAR
# ============================================================

comparacion <- data.frame(

  fecha =
    fechas_futuras,

  mes =
    format(
      fechas_futuras,
      "%B"
    ),

  arimax =
    pronosticos_arimax$pronostico,

  var =
    pronostico_var_inflacion$pronostico

)


cat("\n")
cat("====================================================\n")
cat("COMPARACIÓN ARIMAX VS VAR\n")
cat("====================================================\n")


print(
  comparacion
)


write.csv(

  comparacion,

  "resultados/comparacion_arimax_var.csv",

  row.names = FALSE

)


# ============================================================
# 73. PRONÓSTICO FINAL DEL MEJOR MODELO
# ============================================================

if (
  mejor_modelo ==
  "ARIMAX"
) {

  pronostico_final <- pronosticos_arimax %>%

    dplyr::select(

      fecha,

      mes,

      pronostico,

      limite_inferior_95,

      limite_superior_95

    )

} else {

  pronostico_final <-
    pronostico_var_inflacion

}


# ============================================================
# 74. PREDICCIÓN EXACTA DE AGOSTO 2026
# ============================================================

prediccion_agosto <- pronostico_final %>%

  filter(

    fecha ==
      as.Date(
        "2026-08-01"
      )

  )


cat("\n")
cat("====================================================\n")
cat("PREDICCIÓN EXACTA AGOSTO 2026\n")
cat("====================================================\n")


print(
  prediccion_agosto
)


write.csv(

  prediccion_agosto,

  "resultados/prediccion_agosto_2026.csv",

  row.names = FALSE

)


# ============================================================
# 75. AIC / BIC ARIMAX
# ============================================================

aic_arimax <- AIC(
  modelo_arimax
)


bic_arimax <- BIC(
  modelo_arimax
)


cat("\nAIC ARIMAX:\n")

print(
  aic_arimax
)


cat("\nBIC ARIMAX:\n")

print(
  bic_arimax
)


# ============================================================
# 76. GRÁFICO COMPARACIÓN
# ============================================================

comparacion_largo <- comparacion %>%

  pivot_longer(

    cols =
      c(
        arimax,
        var
      ),

    names_to =
      "modelo",

    values_to =
      "inflacion"

  )


grafico_comparacion <- ggplot(

  comparacion_largo,

  aes(

    x =
      fecha,

    y =
      inflacion,

    color =
      modelo

  )

) +

  geom_line(
    linewidth = 1
  ) +

  geom_point(
    size = 2
  ) +

  labs(

    title =
      "Pronóstico de inflación: ARIMAX vs VAR",

    subtitle =
      "Agosto - Diciembre de 2026",

    x =
      "Fecha",

    y =
      "Inflación anual (%)",

    color =
      "Modelo"

  ) +

  theme_minimal()


ggsave(

  "resultados/comparacion_arimax_var.png",

  grafico_comparacion,

  width = 10,

  height = 6

)


# ============================================================
# 77. GUARDAR PRONÓSTICO FINAL
# ============================================================

write.csv(

  pronostico_final,

  "resultados/pronostico_final_mejor_modelo.csv",

  row.names = FALSE

)


# ============================================================
# 78. ESTADÍSTICOS PRINCIPALES
# ============================================================

estadisticos <- data.frame(

  indicador = c(

    "Correlación inflación-CDT90",

    "ADF inflación nivel p-value",

    "ADF CDT nivel p-value",

    "KPSS inflación nivel p-value",

    "KPSS CDT nivel p-value",

    "ADF inflación diferencia p-value",

    "ADF CDT diferencia p-value",

    "AIC ARIMAX",

    "BIC ARIMAX",

    "Rezagos VAR BIC",

    "VAR estable",

    "RMSE ARIMAX",

    "MAE ARIMAX",

    "RMSE VAR",

    "MAE VAR",

    "Mejor modelo"

  ),

  valor = c(

    correlacion,

    adf_inflacion$p.value,

    adf_cdt$p.value,

    kpss_inflacion$p.value,

    kpss_cdt$p.value,

    adf_inflacion_diff$p.value,

    adf_cdt_diff$p.value,

    aic_arimax,

    bic_arimax,

    p_bic,

    var_estable,

    rmse_arimax,

    mae_arimax,

    rmse_var,

    mae_var,

    mejor_modelo

  )

)


write.csv(

  estadisticos,

  "resultados/estadisticos_modelo.csv",

  row.names = FALSE

)


# ============================================================
# 79. RESUMEN DE GRANGER
# ============================================================

granger_resumen <- data.frame(

  direccion = c(

    "CDT90 -> Inflación",

    "Inflación -> CDT90"

  ),

  estadistico_F = c(

    granger_cdt_resultado$estadistico_F,

    granger_inflacion_resultado$estadistico_F

  ),

  p_value = c(

    granger_cdt_resultado$p_value,

    granger_inflacion_resultado$p_value

  )

)


write.csv(

  granger_resumen,

  "resultados/granger.csv",

  row.names = FALSE

)


# ============================================================
# 80. RESUMEN GENERAL
# ============================================================

sink(

  "resultados/resumen_modelos.txt"

)


cat(
  "====================================================\n"
)

cat(
  "MACROECONOMÍA - COLOMBIA\n"
)

cat(
  "CDT 90 DÍAS E INFLACIÓN\n"
)

cat(
  "====================================================\n\n"
)


cat(
  "PERIODO DE ESTIMACIÓN:\n"
)

cat(
  as.character(
    min(
      base_modelo$fecha
    )
  )
)

cat(
  " a "
)

cat(
  as.character(
    max(
      base_modelo$fecha
    )
  )
)

cat(
  "\n\n"
)


cat(
  "OBSERVACIONES:\n"
)

print(
  nrow(
    base_modelo
  )
)


cat(
  "\n\n====================================================\n"
)

cat(
  "ESTACIONARIEDAD\n"
)

cat(
  "====================================================\n\n"
)


cat(
  "ADF inflación nivel:\n"
)

print(
  adf_inflacion
)


cat(
  "\nADF CDT nivel:\n"
)

print(
  adf_cdt
)


cat(
  "\nKPSS inflación nivel:\n"
)

print(
  kpss_inflacion
)


cat(
  "\nKPSS CDT nivel:\n"
)

print(
  kpss_cdt
)


cat(
  "\nADF inflación primera diferencia:\n"
)

print(
  adf_inflacion_diff
)


cat(
  "\nADF CDT primera diferencia:\n"
)

print(
  adf_cdt_diff
)


cat(
  "\n\n====================================================\n"
)

cat(
  "ARIMAX\n"
)

cat(
  "====================================================\n\n"
)


print(
  summary(
    modelo_arimax
  )
)


cat(
  "\n\nAIC:\n"
)

print(
  aic_arimax
)


cat(
  "\nBIC:\n"
)

print(
  bic_arimax
)


cat(
  "\nLjung-Box:\n"
)

print(
  ljung_arimax
)


cat(
  "\n\n====================================================\n"
)

cat(
  "VAR\n"
)

cat(
  "====================================================\n\n"
)


print(
  summary(
    modelo_var
  )
)


cat(
  "\n\nRaíces VAR:\n"
)

print(
  raices_var
)


cat(
  "\nVAR estable:\n"
)

print(
  var_estable
)


cat(
  "\n\nAutocorrelación:\n"
)

print(
  serial_var
)


cat(
  "\n\nARCH:\n"
)

print(
  arch_var
)


cat(
  "\n\nNormalidad:\n"
)

print(
  normalidad_var
)


cat(
  "\n\n====================================================\n"
)

cat(
  "CAUSALIDAD DE GRANGER\n"
)

cat(
  "====================================================\n\n"
)


cat(
  "CDT90 -> Inflación:\n"
)

print(
  granger_cdt
)


cat(
  "\nInflación -> CDT90:\n"
)

print(
  granger_inflacion
)


cat(
  "\n\n====================================================\n"
)

cat(
  "VALIDACIÓN FUERA DE MUESTRA\n"
)

cat(
  "====================================================\n\n"
)


print(
  resultados_validacion
)


cat(
  "\n\nMEJOR MODELO:\n"
)

print(
  mejor_modelo
)


cat(
  "\n\n====================================================\n"
)

cat(
  "PRONÓSTICO FINAL\n"
)

cat(
  "====================================================\n\n"
)


print(
  pronostico_final
)


cat(
  "\n\n====================================================\n"
)

cat(
  "PREDICCIÓN AGOSTO 2026\n"
)

cat(
  "====================================================\n\n"
)


print(
  prediccion_agosto
)


sink()


# ============================================================
# 81. TABLA FINAL PARA EL INFORME
# ============================================================

tabla_informe <- pronostico_final %>%

  mutate(

    prediccion =
      round(
        pronostico,
        6
      ),

    limite_inferior =
      round(
        limite_inferior_95,
        6
      ),

    limite_superior =
      round(
        limite_superior_95,
        6
      )

  ) %>%

  dplyr::select(

    fecha,

    mes,

    prediccion,

    limite_inferior,

    limite_superior

  )


write.csv(

  tabla_informe,

  "resultados/tabla_final_informe.csv",

  row.names = FALSE

)


# ============================================================
# 82. MENSAJE FINAL
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PROCESO TERMINADO CORRECTAMENTE\n")
cat("====================================================\n")


cat("\nÚltima observación utilizada:\n")

print(
  max(
    base_modelo$fecha
  )
)


cat("\nObservaciones utilizadas:\n")

print(
  nrow(
    base_modelo
  )
)


cat("\nMejor modelo según RMSE de validación:\n")

print(
  mejor_modelo
)


cat("\n")
cat("====================================================\n")
cat("PRONÓSTICO AGOSTO - DICIEMBRE 2026\n")
cat("====================================================\n")


print(
  pronostico_final
)


cat("\n")
cat("====================================================\n")
cat("PREDICCIÓN EXACTA PARA AGOSTO 2026\n")
cat("====================================================\n")


print(
  prediccion_agosto
)


cat("\nArchivos generados en:\n")

cat(
  "resultados/\n"
)


cat("\n")
cat("FIN DEL PROGRAMA\n")
cat("====================================================\n")

# esta version es funcional.
# por favor, no modificar,
# o modificar lo menos posible
