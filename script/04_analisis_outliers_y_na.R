# ==============================================================================
# Análisis de NA y Outliers
# ==============================================================================

library(tidyverse)
library(gt)

# Crear las carpetas de resultados si no existen
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

wdi_limpia <- read_csv(
  "input/wdi_limpia.csv",
  show_col_types = FALSE
)

# Verificar que cada país tenga una sola observación por año
stopifnot(
  !anyNA(wdi_limpia$codigo_pais),
  !anyNA(wdi_limpia$anio),
  !anyDuplicated(wdi_limpia[c("codigo_pais", "anio")])
)

# Análisis horizontal de outliers ####

# Variables relevantes para el análisis
variables_horizontal <- c(
  "poblacion_65_mas",
  "fecundidad",
  "esperanza_vida",
  "crecimiento_poblacional",
  "migracion_neta",
  "poblacion_total",
  "pib_per_capita_ppa",
  "gasto_bolsillo_salud",
  "gasto_publico_salud_pib",
  "gasto_salud_pib",
  "indice_cobertura_salud"
)

# Verificar que todas las variables estén en la base
stopifnot(
  length(variables_horizontal) == 11,
  all(variables_horizontal %in% names(wdi_limpia))
)

# Etiquetas que utilizamos en las tablas y los gráficos
etiquetas_variables <- c(
  poblacion_65_mas = "Población de 65 años o más",
  fecundidad = "Fecundidad",
  esperanza_vida = "Esperanza de vida",
  crecimiento_poblacional = "Crecimiento poblacional",
  migracion_neta = "Migración neta",
  poblacion_total = "Población total",
  pib_per_capita_ppa = "PIB per cápita PPA",
  gasto_bolsillo_salud = "Gasto de bolsillo en salud",
  gasto_publico_salud_pib = "Gasto público en salud",
  gasto_salud_pib = "Gasto total en salud",
  indice_cobertura_salud = "Índice de cobertura de salud"
)

stopifnot(setequal(names(etiquetas_variables), variables_horizontal))

# Pasar los datos a formato largo:
# cada fila representa un país, un año y una variable
datos_horizontal <- wdi_limpia |>
  select(
    pais,
    codigo_pais,
    anio,
    all_of(variables_horizontal)
  ) |>
  pivot_longer(
    cols = all_of(variables_horizontal),
    names_to = "variable",
    values_to = "valor"
  )

# Verificar que la transformación conserve todas las observaciones
stopifnot(
  nrow(datos_horizontal) ==
    nrow(wdi_limpia) * length(variables_horizontal)
)

# Calcular cuartiles y detectar valores extremos por año y variable
analisis_horizontal <- datos_horizontal |>
  group_by(anio, variable) |>
  mutate(
    # Cantidad de valores numéricos válidos
    n_validos = sum(is.finite(valor)),
    
    # Primer y tercer cuartil
    q1 = if (n_validos[1] >= 30) {
      quantile(valor[is.finite(valor)], 0.25)
    } else {
      NA_real_
    },
    
    q3 = if (n_validos[1] >= 30) {
      quantile(valor[is.finite(valor)], 0.75)
    } else {
      NA_real_
    },
    
    # Rango intercuartílico y límites
    iqr = q3 - q1,
    limite_inferior = q1 - 1.5 * iqr,
    limite_superior = q3 + 1.5 * iqr,
    
    # Identificación de candidatos a outlier
    outlier_horizontal = case_when(
      !is.finite(valor) ~ NA,
      n_validos < 30 ~ NA,
      valor < limite_inferior ~ TRUE,
      valor > limite_superior ~ TRUE,
      TRUE ~ FALSE
    ),
    
    tipo_outlier = case_when(
      is.na(outlier_horizontal) ~ NA_character_,
      valor < limite_inferior ~ "Inferior",
      valor > limite_superior ~ "Superior",
      TRUE ~ "No outlier"
    )
  ) |>
  ungroup()

# Resumen general de la detección
analisis_horizontal |>
  summarise(
    observaciones_evaluadas = sum(!is.na(outlier_horizontal)),
    candidatos_outlier = sum(outlier_horizontal, na.rm = TRUE),
    porcentaje_outliers = round(
      100 * candidatos_outlier / observaciones_evaluadas, 2
    )
  ) |>
  print()

# Exportar los outliers detectados en la base WDI
outliers_horizontales_wdi <- analisis_horizontal |>
  filter(outlier_horizontal == TRUE) |>
  select(
    codigo_pais,
    pais,
    anio,
    variable,
    valor,
    tipo_outlier,
    limite_inferior,
    limite_superior
  ) |>
  arrange(codigo_pais, anio, variable)

write_csv(
  outliers_horizontales_wdi,
  "output/tablas/outliers_horizontales_wdi.csv"
)

# Para ver cuántos outliers hay y qué peso tienen en cada variable-año
resumen_outliers_horizontal <- analisis_horizontal |>
  group_by(variable, anio) |>
  summarise(
    paises_con_dato = first(n_validos),
    cantidad_outliers = sum(outlier_horizontal, na.rm = TRUE),
    
    porcentaje_outliers = if_else(
      paises_con_dato >= 30,
      round(100 * cantidad_outliers / paises_con_dato, 1),
      NA_real_
    ),
    
    evaluable = paises_con_dato >= 30,
    .groups = "drop"
  ) |>
  arrange(
    variable,
    anio
  )

# Se repiten países muchas veces como outliers en las variables?
recurrencia_outliers <- analisis_horizontal |>
  filter(outlier_horizontal == TRUE) |>
  group_by(variable, pais) |>
  summarise(
    cantidad_anios_outlier = n(),
    primer_anio_outlier = min(anio),
    ultimo_anio_outlier = max(anio),
    .groups = "drop"
  ) |>
  arrange(
    variable,
    desc(cantidad_anios_outlier)
  )

# Con este análisis horizontal buscamos candidatos, es decir valores extremos en
# determinada variable de determinado país. Pero falta ver si esos valores son coherentes
# con las mismas dinámicas de esa variable en ese país.

# Análisis vertical de outliers ####

# La comparación temporal permite evaluar si los candidatos detectados
# horizontalmente también presentan desviaciones importantes respecto
# de la trayectoria del propio país.
# Una desviación temporal no demuestra que exista un error en los datos.

# Solo necesitamos fijarnos en los que son outlier horizontal reconocido previamente.
# Entonces, recortamos los candidatos.
candidatos_outliers <- analisis_horizontal |>
  filter(outlier_horizontal == TRUE) |>
  select(
    pais,
    codigo_pais,
    anio,
    variable,
    valor,
    tipo_outlier
  )

# Cuántos candidatos quedaron?
nrow(candidatos_outliers)

# Cuántos por variable?
candidatos_outliers |>
  count(variable, sort = TRUE)

# Las observaciones candidatas pueden pertenecer al mismo país y a la misma
# variable en distintos años. No equivalen a igual cantidad de países afectados.
# Nos preguntamos si son extremos genuinos o posibles errores de medición.
# Analizamos el mismo país y variable: cinco años antes y cinco después.

# Identificamos el año y valor de cada candidato
candidatos_vertical <- candidatos_outliers |>
  rename(
    anio_candidato = anio,
    valor_candidato = valor
  )

# Verificamos que cada candidato sea único
stopifnot(
  !anyDuplicated(
    candidatos_vertical[
      c("codigo_pais", "anio_candidato", "variable")
    ]
  )
)

# Generamos los diez años vecinos de cada candidato
vecinos_vertical <- candidatos_vertical |>
  tidyr::crossing(
    desfase = c(-5:-1, 1:5)
  ) |>
  mutate(
    anio_vecino = anio_candidato + desfase
  ) |>
  left_join(
    datos_horizontal |>
      select(codigo_pais, anio, variable, valor) |>
      rename(
        anio_vecino = anio,
        valor_vecino = valor
      ),
    by = c("codigo_pais", "variable", "anio_vecino"),
    relationship = "many-to-one"
  ) |>
  select(-desfase)

# Comprobamos que hay diez posiciones vecinas por candidato
stopifnot(
  nrow(vecinos_vertical) == nrow(candidatos_vertical) * 10
)

# Resumen de disponibilidad de los vecinos
vecinos_vertical |>
  summarise(
    candidatos = n_distinct(
      codigo_pais, anio_candidato, variable
    ),
    posiciones_vecinas = n(),
    vecinos_con_dato = sum(is.finite(valor_vecino)),
    vecinos_sin_dato = sum(!is.finite(valor_vecino))
  ) |>
  print()

# Diagnóstico vertical de los candidatos ####

# Resumir los valores de los años vecinos
diagnostico_vertical <- vecinos_vertical |>
  group_by(
    pais,
    codigo_pais,
    variable,
    anio_candidato,
    valor_candidato,
    tipo_outlier
  ) |>
  summarise(
    # Cantidad de vecinos con datos válidos
    n_vecinos = sum(is.finite(valor_vecino)),
    
    n_antes = sum(
      anio_vecino < anio_candidato & is.finite(valor_vecino)
    ),
    
    n_despues = sum(
      anio_vecino > anio_candidato & is.finite(valor_vecino)
    ),
    
    # Medidas de la trayectoria local
    mediana_vecinos = if (n_vecinos >= 5) {
      median(valor_vecino[is.finite(valor_vecino)])
    } else {
      NA_real_
    },
    
    minimo_vecinos = if (n_vecinos >= 5) {
      min(valor_vecino[is.finite(valor_vecino)])
    } else {
      NA_real_
    },
    
    maximo_vecinos = if (n_vecinos >= 5) {
      max(valor_vecino[is.finite(valor_vecino)])
    } else {
      NA_real_
    },
    
    mad_vecinos = if (n_vecinos >= 5) {
      mad(valor_vecino[is.finite(valor_vecino)])
    } else {
      NA_real_
    },
    
    .groups = "drop"
  ) |>
  mutate(
    # Comprobar si el candidato excede el rango observado
    fuera_rango_local = case_when(
      n_vecinos < 5 ~ NA,
      valor_candidato < minimo_vecinos ~ TRUE,
      valor_candidato > maximo_vecinos ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Desvío respecto de la mediana en unidades de MAD
    z_robusto = if_else(
      n_vecinos >= 5 & is.finite(mad_vecinos) & mad_vecinos > 0,
      abs(valor_candidato - mediana_vecinos) / mad_vecinos,
      NA_real_
    )
  )

# Comprobar que seguimos teniendo un registro por candidato
stopifnot(
  nrow(diagnostico_vertical) == nrow(candidatos_vertical)
)

# Evaluar la disponibilidad del diagnóstico
diagnostico_vertical |>
  summarise(
    total_candidatos = n(),
    suficientes_vecinos = sum(n_vecinos >= 5),
    pocos_vecinos = sum(n_vecinos < 5),
    mad_cero = sum(mad_vecinos == 0, na.rm = TRUE),
    z_disponible = sum(is.finite(z_robusto)),
    sin_ambos_lados = sum(n_antes == 0 | n_despues == 0)
  ) |>
  print()

# Clasificación de los candidatos según el diagnóstico vertical ####

diagnostico_vertical <- diagnostico_vertical |>
  mutate(
    # Registrar si la comparación tiene datos anteriores y posteriores
    cobertura_temporal = if_else(
      n_antes > 0 & n_despues > 0,
      "Ambos lados",
      "Un solo lado"
    ),
    
    # Clasificar según el rango local y el criterio de 3 MAD
    clasificacion_vertical = case_when(
      is.na(z_robusto) ~ "No evaluable con MAD",
      
      fuera_rango_local == TRUE &
        z_robusto > 3 ~ "Alerta fuerte: ambos criterios",
      
      fuera_rango_local == TRUE &
        z_robusto <= 3 ~ "Solo fuera de rango",
      
      fuera_rango_local == FALSE &
        z_robusto > 3 ~ "Solo supera 3 MAD",
      
      TRUE ~ "Coherente con la trayectoria"
    )
  )

# Cantidad de candidatos por clasificación
diagnostico_vertical |>
  count(clasificacion_vertical, sort = TRUE) |>
  print(n = Inf)

# Examinar si las alertas tienen cobertura temporal bilateral
diagnostico_vertical |>
  count(clasificacion_vertical, cobertura_temporal) |>
  print(n = Inf)

# Verificar que todos los candidatos hayan sido clasificados
stopifnot(
  !anyNA(diagnostico_vertical$clasificacion_vertical),
  nrow(diagnostico_vertical) == nrow(candidatos_vertical)
)

# ¿En qué variables aparecen las alertas fuertes, tanto horizontales como
# respecto de la trayectoria del propio país?
diagnostico_vertical |>
  filter(clasificacion_vertical == "Alerta fuerte: ambos criterios") |>
  count(variable, sort = TRUE)

# Miramos casos particulares de cada tipo de alerta.
# La función evita repetir el mismo procedimiento para las tres categorías.
mostrar_trayectoria <- function(caso) {
  
  if (nrow(caso) == 0) {
    message("No hay candidatos en esta categoría.")
    return(invisible(NULL))
  }
  
  datos_horizontal |>
    filter(
      codigo_pais == caso$codigo_pais[[1]],
      variable == caso$variable[[1]],
      anio >= caso$anio_candidato[[1]] - 5,
      anio <= caso$anio_candidato[[1]] + 5
    ) |>
    select(pais, variable, anio, valor) |>
    arrange(anio) |>
    print(n = Inf)
}

# Caso con alerta fuerte
caso_zoom <- diagnostico_vertical |>
  filter(clasificacion_vertical == "Alerta fuerte: ambos criterios") |>
  slice_max(z_robusto, n = 1, with_ties = FALSE)

print(caso_zoom)
mostrar_trayectoria(caso_zoom)

# Caso que solo está fuera del rango de los años vecinos
caso_fuera_rango <- diagnostico_vertical |>
  filter(clasificacion_vertical == "Solo fuera de rango") |>
  slice_max(z_robusto, n = 1, with_ties = FALSE)

print(caso_fuera_rango)
mostrar_trayectoria(caso_fuera_rango)

# Caso que solo supera 3 MAD
caso_mad <- diagnostico_vertical |>
  filter(clasificacion_vertical == "Solo supera 3 MAD") |>
  slice_max(z_robusto, n = 1, with_ties = FALSE)

print(caso_mad)
mostrar_trayectoria(caso_mad)

# Formamos tabla: reutilizamos las etiquetas definidas al inicio
variables_tabla <- unname(etiquetas_variables[variables_horizontal])

tabla_diagnostico_vertical <- diagnostico_vertical |>
  mutate(
    # Acortamos el nombre de la categoría
    clasificacion_vertical = recode(
      clasificacion_vertical,
      "Alerta fuerte: ambos criterios" = "Alerta fuerte"
    ),
    # Utilizar los mismos nombres descriptivos que en las tablas
    variable = recode(variable, !!!etiquetas_variables)
  ) |>
  
  filter(
    clasificacion_vertical != "Coherente con la trayectoria"
  ) |>
  
  # Contamos cuántos casos hay por variable y clasificación
  count(
    variable,
    clasificacion_vertical,
    name = "casos"
  ) |>
  
  # Forzamos a que aparezcan TODAS las variables,
  # aunque tengan 0 casos en todas las categorías
  complete(
    variable = variables_tabla,
    clasificacion_vertical = c(
      "Solo fuera de rango",
      "Solo supera 3 MAD",
      "Alerta fuerte",
      "No evaluable con MAD"
    ),
    fill = list(casos = 0)
  ) |>
  
  # Pasamos las categorías a columnas
  pivot_wider(
    names_from = clasificacion_vertical,
    values_from = casos
  ) |>
  
  # Sumamos el total de alertas por variable
  mutate(
    Total =
      `Solo fuera de rango` +
      `Solo supera 3 MAD` +
      `Alerta fuerte` +
      `No evaluable con MAD`
  ) |>
  
  arrange(desc(Total))

# Agregamos una fila TOTAL

tabla_diagnostico_vertical <- tabla_diagnostico_vertical |>
  bind_rows(
    tabla_diagnostico_vertical |>
      summarise(
        variable = "TOTAL",
        across(
          where(is.numeric),
          sum
        )
      )
  )

# Tabla con etiquetas, totales y formato básico
tabla_diagnostico_gt <- tabla_diagnostico_vertical |>
  gt() |>
  tab_header(
    title = "Diagnóstico vertical de outliers",
    subtitle = "Cantidad de observaciones según variable y tipo de señal"
  ) |>
  cols_label(
    variable = "Variable",
    `Solo fuera de rango` = "Solo fuera de rango",
    `Solo supera 3 MAD` = "Solo supera 3 MAD",
    `Alerta fuerte` = "Alerta fuerte",
    `No evaluable con MAD` = "No evaluable",
    Total = "Total"
  ) |>
  cols_align(align = "center", columns = -variable) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = variable == "TOTAL")
  )

gt::gtsave(
  tabla_diagnostico_gt,
  "output/tablas/tabla_diagnostico_outliers.png",
  zoom = 2
)

# Análisis NA ####

# Los mapas de calor por región y década están en 03_analisis_exploratorio.R.
# Aquí conservamos el seguimiento anual y el test del mecanismo de faltantes.

# Diagnóstico de NA ####

# Utilizamos los mismos indicadores que en el análisis de outliers
variables_relevantes <- variables_horizontal

# Calculamos los faltantes para cada variable y año
tabla_na_anual <- wdi_limpia |>
  group_by(anio) |>
  summarise(
    across(
      all_of(variables_relevantes),
      ~ sum(is.na(.))
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = all_of(variables_relevantes),
    names_to = "variable",
    values_to = "cantidad_na"
  )

# Mostramos la tabla
print(tabla_na_anual)

# Resumen para los años de los análisis principales
tabla_na_anios_elegidos <- tabla_na_anual |>
  filter(anio %in% c(1980, 1995, 2000, 2010, 2019, 2023, 2024)) |>
  pivot_wider(
    names_from = anio,
    values_from = cantidad_na
  )

print(tabla_na_anios_elegidos)

# Test de Little: diagnóstico del mecanismo de datos faltantes ####

# Años relevantes para los análisis
anios_mcar <- c(2000, 2010, 2019, 2023)

# Tabla para almacenar los resultados
resultados_little <- tibble()

# El test requiere el paquete naniar y variables con variación.
if (!requireNamespace("naniar", quietly = TRUE)) {
  stop("Falta el paquete naniar. Instalalo con install.packages('naniar').")
}

for (anio_elegido in anios_mcar) {
  
  # Seleccionar los datos del año correspondiente
  datos_mcar <- wdi_limpia |>
    filter(anio == anio_elegido) |>
    select(all_of(variables_relevantes)) |>
    # Excluir solo del test las columnas constantes o sin datos válidos.
    select(where(~ {
      desvio <- sd(., na.rm = TRUE)
      is.finite(desvio) && desvio > 0
    }))
  
  if (ncol(datos_mcar) < 2 || !anyNA(datos_mcar)) {
    # No informar un p-valor si el test no puede aplicarse.
    resultado_anio <- tibble(
      anio = anio_elegido,
      statistic = NA_real_,
      df = NA_real_,
      p.value = NA_real_,
      estado = "No aplicable"
    )
  } else {
    # Estandarizar las variables
    datos_mcar_std <- datos_mcar |>
      mutate(
        across(
          everything(),
          ~ as.numeric(scale(.))
        )
      )
    
    # Verificar que la estandarización no alteró los NA
    stopifnot(
      identical(is.na(datos_mcar), is.na(datos_mcar_std))
    )
    
    # Ejecutar el test de Little; si falla, dejar constancia.
    resultado_anio <- tryCatch(
      naniar::mcar_test(datos_mcar_std) |>
        mutate(anio = anio_elegido, estado = "Calculado", .before = 1),
      error = function(e) {
        message("Test de Little (", anio_elegido, "): ", conditionMessage(e))
        tibble(
          anio = anio_elegido,
          statistic = NA_real_,
          df = NA_real_,
          p.value = NA_real_,
          estado = "No calculado: revisar error"
        )
      }
    )
  }
  
  # Registrar las variables efectivamente incluidas en cada año
  resultado_anio <- resultado_anio |>
    mutate(
      cantidad_variables = ncol(datos_mcar),
      variables_utilizadas = paste(names(datos_mcar), collapse = ", ")
    )
  
  # Guardar el resultado de este año
  resultados_little <- bind_rows(
    resultados_little,
    resultado_anio
  )
}

# Mostrar todos los resultados
print(resultados_little, n = Inf)

# Exportar la tabla
write_csv(
  resultados_little,
  "output/tablas/test_little.csv"
)

# El resumen general de NA ya se guarda en 03_analisis_exploratorio.R.

# Exportación de resultados

# Diagnóstico horizontal: resumen por variable y año
write_csv(
  resumen_outliers_horizontal,
  "output/tablas/resumen_outliers_horizontal.csv"
)

# Recurrencia de outliers en cada país y variable
write_csv(
  recurrencia_outliers,
  "output/tablas/recurrencia_outliers.csv"
)

# Diagnóstico vertical: todos los candidatos y sus criterios
write_csv(
  diagnostico_vertical,
  "output/tablas/diagnostico_vertical_completo.csv"
)

# Datos faltantes: evolución anual
write_csv(
  tabla_na_anual,
  "output/tablas/na_por_anio.csv"
)

# Datos faltantes: años relevantes para los análisis
write_csv(
  tabla_na_anios_elegidos,
  "output/tablas/na_anios_elegidos.csv"
)

# Verificar que todos los archivos se guardaron
archivos_resultados <- c(
  "output/tablas/outliers_horizontales_wdi.csv",
  "output/tablas/recurrencia_outliers.csv",
  "output/tablas/tabla_diagnostico_outliers.png",
  "output/tablas/test_little.csv",
  "output/tablas/resumen_outliers_horizontal.csv",
  "output/tablas/diagnostico_vertical_completo.csv",
  "output/tablas/na_por_anio.csv",
  "output/tablas/na_anios_elegidos.csv"
)

stopifnot(all(file.exists(archivos_resultados)))

print("Todos los resultados se guardaron correctamente.")
