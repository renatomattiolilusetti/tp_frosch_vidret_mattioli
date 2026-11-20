# ==============================================================================
# Análisis de NA y Outliers
# ==============================================================================

library(tidyverse)
library(gt)

wdi_limpia <- read_csv("input/wdi_limpia.csv")


# Verificar que cada país tenga una sola observación por año
stopifnot(
  !anyNA(wdi_limpia$codigo_pais),
  !anyNA(wdi_limpia$anio),
  !anyDuplicated(wdi_limpia[c("codigo_pais", "anio")])
)

# 1. Detección horizontal de outliers

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
  "dependencia_mayor",
  "gasto_salud_pib",
  "indice_cobertura_salud"
)

# Verificar que todas las variables estén en la base
stopifnot(
  all(variables_horizontal %in% names(wdi_limpia))
)

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

print(file.exists(
  "output/tablas/outliers_horizontales_wdi.csv"
))

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
# con las mismas dinámicas de esa variable en ese país. Ej. no es lo mismo caso 
# Mónaco que caso país africano que no sigue una tendencia marcada.

# Análisis VERTICAL ####

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

# Tendriamos en principio 6093 observaciones marcadas como outliers. Pero pueden pertenecer
# algunas al mismo país y a la misma variable y repetirse en distintos años. Y ademas
# puede resultar siendo un solo caso (país-variable). 

# Es decir, nos preguntamos si esas 6093 observaciones candidatas son valores genuinos
# o posibles errores/ verdaderos valores atipicos.
# Analizamos mismo país, misma variable --> 5 años antes y 5 años después.

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

# 3. Diagnóstico vertical de los candidatos

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

# 4. Clasificación de los candidatos según el diagnóstico vertical

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

# En qué variables aparecen los 334 casos que son raros tanto horizontalmente como 
# con respecto a de la trayectoria del propio país?

diagnostico_vertical |>
  filter(clasificacion_vertical == "Alerta fuerte: ambos criterios") |>
  count(variable, sort = TRUE)

# Miramos un caso particular de alerta fuerte:

caso_zoom <- diagnostico_vertical |>
  filter(
    clasificacion_vertical == "Alerta fuerte: ambos criterios"
  ) |>
  arrange(z_robusto) |>
  slice(167)

caso_zoom

datos_horizontal |>
  filter(
    pais == caso_zoom$pais,
    variable == caso_zoom$variable,
    anio >= caso_zoom$anio_candidato - 5,
    anio <= caso_zoom$anio_candidato + 5
  ) |>
  select(pais, variable, anio, valor) |>
  arrange(anio)

# Otro zoom in en el grupo "Solo fuera de rango":

caso_fuera_rango <- diagnostico_vertical |>
  filter(
    clasificacion_vertical == "Solo fuera de rango"
  ) |>
  arrange(z_robusto) |>
  slice(271)

caso_fuera_rango

datos_horizontal |>
  filter(
    pais == caso_fuera_rango$pais,
    variable == caso_fuera_rango$variable,
    anio >= caso_fuera_rango$anio_candidato - 5,
    anio <= caso_fuera_rango$anio_candidato + 5
  ) |>
  select(pais, variable, anio, valor) |>
  arrange(anio)

# Otro en el grupo "Solo supera 3 MAD"

caso_mad <- diagnostico_vertical |>
  filter(
    clasificacion_vertical == "Solo supera 3 MAD"
  ) |>
  arrange(z_robusto) |>
  slice(86)

caso_mad

datos_horizontal |>
  filter(
    pais == caso_mad$pais,
    variable == caso_mad$variable,
    anio >= caso_mad$anio_candidato - 5,
    anio <= caso_mad$anio_candidato + 5
  ) |>
  select(pais, variable, anio, valor) |>
  arrange(anio)

# Formamos tabla para PPT:

variables_tabla <- c(
  "Población de 65 años o más",
  "Fecundidad",
  "Esperanza de vida",
  "Crecimiento poblacional",
  "Migración neta",
  "Población total",
  "PIB per cápita PPA",
  "Gasto de bolsillo en salud",
  "Gasto público en salud",
  "Dependencia de mayores",
  "Gasto total en salud",
  "Índice de cobertura de salud"
)

tabla_diagnostico_vertical <- diagnostico_vertical |>
  
  mutate(
    
    # Acortamos el nombre de la categoría
    clasificacion_vertical = recode(
      clasificacion_vertical,
      "Alerta fuerte: ambos criterios" = "Alerta fuerte"
    ),
    
    # Ponemos otros nombres a las variables
    variable = recode(
      variable,
      poblacion_65_mas = "Población de 65 años o más",
      fecundidad = "Fecundidad",
      esperanza_vida = "Esperanza de vida",
      crecimiento_poblacional = "Crecimiento poblacional",
      migracion_neta = "Migración neta",
      poblacion_total = "Población total",
      pib_per_capita_ppa = "PIB per cápita PPA",
      gasto_bolsillo_salud = "Gasto de bolsillo en salud",
      gasto_publico_salud_pib = "Gasto público en salud",
      dependencia_mayor = "Dependencia de mayores",
      gasto_salud_pib = "Gasto total en salud",
      indice_cobertura_salud = "Índice de cobertura de salud"
    )
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

# Tabla:

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
  
  cols_align(
    align = "left",
    columns = variable
  ) |>
  
  cols_align(
    align = "center",
    columns = -variable
  ) |>
  
  # Negrita en los nombres de columnas
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  
  # Negrita en la fila TOTAL
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = variable == "TOTAL"
    )
  ) |>
  
  tab_options(
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(15),
    column_labels.font.size = px(13),
    table.font.size = px(13),
    data_row.padding = px(6)
  )

gt::gtsave(
  tabla_diagnostico_gt,
  "output/tablas/tabla_diagnostico_outliers.png",
  zoom = 2
)

# NA ####

# Recordamos las variables relevantes:

variables_relevantes <- c(
  "poblacion_65_mas",
  "fecundidad",
  "esperanza_vida",
  "crecimiento_poblacional",
  "migracion_neta",
  "poblacion_total",
  "pib_per_capita_ppa",
  "gasto_bolsillo_salud",
  "gasto_publico_salud_pib",
  "dependencia_mayor",
  "gasto_salud_pib",
  "indice_cobertura_salud"
)

# Agregamos etiquetas (para los graficos posteriores):

etiquetas_variables <- c(
  poblacion_65_mas = "Población 65+",
  fecundidad = "Fecundidad",
  esperanza_vida = "Esperanza de vida",
  crecimiento_poblacional = "Crecimiento poblacional",
  migracion_neta = "Migración neta",
  poblacion_total = "Población total",
  pib_per_capita_ppa = "PIB per cápita PPA",
  gasto_bolsillo_salud = "Gasto de bolsillo en salud",
  gasto_publico_salud_pib = "Gasto público en salud",
  dependencia_mayor = "Dependencia de mayores",
  gasto_salud_pib = "Gasto total en salud",
  indice_cobertura_salud = "Índice de cobertura de salud"
)

# Evaluamos NA por décadas

na_decadas_relevantes <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  mutate(
    decada = cut(
      anio,
      breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2026),
      labels = c(
        "1960–1969", "1970–1979", "1980–1989",
        "1990–1999", "2000–2009", "2010–2019",
        "2020–2025"
      ),
      right = FALSE
    )
  ) |>
  group_by(decada) |>
  summarise(
    across(
      all_of(variables_relevantes),
      ~ mean(is.na(.)) * 100
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = all_of(variables_relevantes),
    names_to = "variable",
    values_to = "porcentaje_na"
  ) |>
  mutate(
    variable = recode(variable, !!!etiquetas_variables)
  )

# Grafico para agregar a diapo: Mapa de calor de concentración de NA por variable
# por década. 

grafico_na_decadas <- ggplot(
  na_decadas_relevantes,
  aes(
    x = decada,
    y = variable,
    fill = porcentaje_na
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  
  geom_text(
    aes(
      label = ifelse(
        porcentaje_na == 0,
        "",
        paste0(round(porcentaje_na, 1), "%")
      )
    ),
    size = 3,
    color = "#243746"
  ) +
  
  scale_fill_gradient(
    low = "#F4F7FB",
    high = "#174A7E",
    limits = c(0, 100),
    name = "% de NA"
  ) +
  
  labs(
    title = "Valores faltantes a lo largo del tiempo",
    subtitle = "Porcentaje de NA por variable y década",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      size = 10,
      color = "#3F494E"
    ),
    
    axis.text.y = element_text(
      size = 10,
      color = "#3F494E"
    ),
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#173A5E"
    ),
    
    plot.subtitle = element_text(
      size = 11,
      color = "#647480"
    ),
    
    legend.position = "right"
  )


# grafico_na_decadas

ggsave(
  "output/graficos/na_por_decada.png",
  plot = grafico_na_decadas,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Mapa de calor por región

na_region_relevantes <- wdi_limpia |>
  
  group_by(region) |>
  
  summarise(
    across(
      all_of(variables_relevantes),
      ~ mean(is.na(.)) * 100
    ),
    .groups = "drop"
  ) |>
  
  pivot_longer(
    cols = all_of(variables_relevantes),
    names_to = "variable",
    values_to = "porcentaje_na"
  ) |>
  
  mutate(
    variable = recode(
      variable,
      !!!etiquetas_variables
    )
  )


grafico_na_region <- ggplot(
  na_region_relevantes,
  aes(
    x = region,
    y = variable,
    fill = porcentaje_na
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  
  geom_text(
    aes(
      label = ifelse(
        porcentaje_na == 0,
        "",
        paste0(round(porcentaje_na, 1), "%")
      )
    ),
    size = 3,
    color = "#243746"
  ) +
  
  scale_fill_gradient(
    low = "#F4F7FB",
    high = "#174A7E",
    limits = c(0, 100),
    name = "% de NA"
  ) +
  
  labs(
    title = "Valores faltantes según región",
    subtitle = "Porcentaje de NA por variable y región",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 9,
      color = "#3F494E"
    ),
    
    axis.text.y = element_text(
      size = 10,
      color = "#3F494E"
    ),
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#173A5E"
    ),
    
    plot.subtitle = element_text(
      size = 11,
      color = "#647480"
    ),
    
    legend.position = "right"
  )


# grafico_na_region

ggsave(
  "output/graficos/na_por_region.png",
  plot = grafico_na_region,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

# ============================================================
# Diagnóstico de NA: todas las variables relevantes
# ============================================================

# Seleccionamos las 12 variables de la segunda entrega

variables_relevantes <- c(
  "poblacion_total",
  "poblacion_65_mas",
  "pib_per_capita_ppa",
  "migracion_neta",
  "indice_cobertura_salud",
  "gasto_salud_pib",
  "gasto_publico_salud_pib",
  "gasto_bolsillo_salud",
  "fecundidad",
  "esperanza_vida",
  "dependencia_mayor",
  "crecimiento_poblacional"
)

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
View(tabla_na_anual)

# Resumen para los años de los análisis principales

tabla_na_anios_elegidos <- tabla_na_anual |>
  filter(anio %in% c(1980, 1995, 2000, 2010, 2019, 2023, 2024)) |>
  pivot_wider(
    names_from = anio,
    values_from = cantidad_na
  )

View(tabla_na_anios_elegidos)


# ==============================================================================
# Test de Little: diagnóstico del mecanismo de datos faltantes
# ==============================================================================

# Años relevantes para los análisis
anios_mcar <- c(2000, 2010, 2019, 2023)

# Tabla para almacenar los resultados
resultados_little <- tibble()

for (anio_elegido in anios_mcar) {
  
  # Seleccionar los datos del año correspondiente
  datos_mcar <- wdi_limpia |>
    filter(anio == anio_elegido) |>
    select(all_of(variables_relevantes))
  
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
  
  # Ejecutar el test de Little
  resultado_anio <- naniar::mcar_test(datos_mcar_std) |>
    mutate(anio = anio_elegido, .before = 1)
  
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


# ============================================================
# Tabla resumen de NA por variable (1960-2025)
# ============================================================

tabla_na_total <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  summarise(
    across(
      all_of(variables_relevantes),
      ~ sum(is.na(.))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "cantidad_na"
  ) |>
  mutate(
    total_observaciones = sum(
      wdi_limpia$anio >= 1960 &
        wdi_limpia$anio <= 2025,
      na.rm = TRUE
    ),
    porcentaje_na = round(
      cantidad_na / total_observaciones * 100, 2
    )
  ) |>
  arrange(desc(porcentaje_na))

# Exportación de resultados

# Diagnóstico horizontal: resumen por variable y año
write_csv(
  resumen_outliers_horizontal,
  "output/tablas/resumen_outliers_horizontal.csv"
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

# Datos faltantes: resumen general
write_csv(
  tabla_na_total,
  "output/tablas/na_resumen_general.csv"
)

# Verificar que todos los archivos se guardaron
archivos_resultados <- c(
  "output/tablas/resumen_outliers_horizontal.csv",
  "output/tablas/diagnostico_vertical_completo.csv",
  "output/tablas/na_por_anio.csv",
  "output/tablas/na_anios_elegidos.csv",
  "output/tablas/na_resumen_general.csv"
)

stopifnot(all(file.exists(archivos_resultados)))

print("Todos los resultados se guardaron correctamente.")
