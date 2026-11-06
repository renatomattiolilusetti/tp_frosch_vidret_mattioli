# ==============================================================================
# Análisis de NA y Outliers
# ==============================================================================

wdi_limpia <- read_csv("repo_wdi/wdi_limpia.csv")

# Outliers ####

# Análisis HORIZONTAL ####

# Para cada variable relevante y para cada año disponible, comparamos todos los países 
# entre sí y detectamos qué observaciones son extremas respecto de la distribución 
# de esa variable en ese año (contra el resto de países).

# Seleccionamos las variables relevantes:

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

# Trabajamos con todos los años disponibles:

anios_horizontal <- sort(
  unique(wdi_limpia$anio)
)

# Formamos una base 

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

# Pasamos a ancho

tabla_horizontal <- datos_horizontal |>
  select(
    anio,
    variable,
    pais,
    valor
  ) |>
  pivot_wider(
    names_from = pais,
    values_from = valor
  ) |>
  arrange(
    anio,
    variable
  )

tabla_horizontal

# Detectamos outliers de forma horizontal: dentro de cada año y variable, calculamos
# Q1, Q2 e IQR usando los países que tienen dato y marcamos qué país queda fuera de 
# los límites. Decidimos que una combinación año-variable se evalúa si cuenta con al 
# menos 30 observaciones válidas.

# para cuáles datos son outliers

analisis_horizontal <- datos_horizontal |>
  group_by(anio, variable) |>
  mutate(
    n_validos = sum(!is.na(valor)),
    
    q1 = if_else(
      n_validos >= 30,
      quantile(valor, 0.25, na.rm = TRUE),
      NA_real_
    ),
    
    q3 = if_else(
      n_validos >= 30,
      quantile(valor, 0.75, na.rm = TRUE),
      NA_real_
    ),
    
    iqr = q3 - q1,
    
    limite_inferior = q1 - 1.5 * iqr,
    limite_superior = q3 + 1.5 * iqr,
    
    outlier_horizontal = case_when(
      is.na(valor) ~ NA,
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

# Necesitamos una comparación del valor outlier contra los valores cercanos del mismo
# país. ESO nos va a confirmar que sea o no outlier.
# Análisis robusto con mediana + MAD: chequeamos qué tan lejos estaría un valor de la 
# mediana, en relación con la variabilidad habitual de esos años vecinos.

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

# Renombramos el año y el valor del outlier para diferenciarlos
# de los años vecinos que vamos a buscar.

candidatos_vertical <- candidatos_outliers |>
  rename(
    anio_candidato = anio,
    valor_candidato = valor
  )

# Para candidato, buscamos la misma variable del mismo país durante los 5 años
# posteriores y anteriores

vecinos_vertical <- candidatos_vertical |>
  left_join(
    datos_horizontal |>
      select(
        pais,
        codigo_pais,
        anio,
        variable,
        valor
      ) |>
      rename(
        anio_vecino = anio,
        valor_vecino = valor
      ),
    by = c(
      "pais",
      "codigo_pais",
      "variable"
    ),
    relationship = "many-to-many"
  ) |>
  filter(
    anio_vecino >= anio_candidato - 5,
    anio_vecino <= anio_candidato + 5,
    anio_vecino != anio_candidato
  )

# Resumimos la trayectoria local de cada candidato

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
    
    n_vecinos = sum(!is.na(valor_vecino)),
    
    mediana_vecinos = if (
      sum(!is.na(valor_vecino)) > 0
    ) {
      median(valor_vecino, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    minimo_vecinos = if (
      sum(!is.na(valor_vecino)) > 0
    ) {
      min(valor_vecino, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    maximo_vecinos = if (
      sum(!is.na(valor_vecino)) > 0
    ) {
      max(valor_vecino, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    mad_vecinos = if (
      sum(!is.na(valor_vecino)) >= 2
    ) {
      mad(valor_vecino, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

# Miramos ahora: el valor está fuera de todo lo observado en los años vecinos?
# pero tambien con el criterio MAD de cuánto se aleja la mediana en términos del MAD.

diagnostico_vertical <- diagnostico_vertical |>
  mutate(
    
    # ¿Está fuera de todo lo observado en los años vecinos?
    fuera_rango_local =
      valor_candidato < minimo_vecinos |
      valor_candidato > maximo_vecinos,
    
    # ¿Cuánto se aleja de la mediana en términos del MAD?
    z_robusto = if_else(
      !is.na(mad_vecinos) & mad_vecinos > 0,
      abs(valor_candidato - mediana_vecinos) / mad_vecinos,
      NA_real_
    )
  )

# Resumimos diagnostico en base a lo anterior:

diagnostico_vertical |>
  summarise(
    total = n(),
    
    fuera_rango =
      sum(fuera_rango_local == TRUE, na.rm = TRUE),
    
    supera_3_mad =
      sum(z_robusto > 3, na.rm = TRUE),
    
    cumple_ambos =
      sum(
        fuera_rango_local == TRUE &
          z_robusto > 3,
        na.rm = TRUE
      )
  )

# Cuántos no pudieron evaluarse bien con MAD por tener pocos años vecinos con datos
# o MAD=0? (problema ante NA)

diagnostico_vertical |>
  summarise(
    z_na = sum(is.na(z_robusto)),
    mad_cero = sum(mad_vecinos == 0, na.rm = TRUE),
    pocos_vecinos = sum(n_vecinos < 5)
  )

diagnostico_vertical |>
  filter(is.na(z_robusto)) |>
  select(
    pais,
    variable,
    anio_candidato,
    valor_candidato,
    n_vecinos,
    mad_vecinos
  )

# Clasificación de los candidatos segun el diagnóstico vertical:

diagnostico_vertical <- diagnostico_vertical |>
  mutate(
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

diagnostico_vertical |>
  count(clasificacion_vertical, sort = TRUE)

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


# Ver la tabla

#tabla_diagnostico_gt

# ==============================================================================

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

#

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

# ============================================================
# Test de Little: variables estandarizadas
# ============================================================

# Estandarizamos las 12 variables
datos_mcar_2019_std <- datos_mcar_2019 |>
  mutate(
    across(
      everything(),
      ~ as.numeric(scale(.))
    )
  )

# Verificamos que los NA no hayan cambiado
colSums(is.na(datos_mcar_2019_std))

# Reintentamos el test de Little
resultado_mcar_2019_std <- naniar::mcar_test(
  datos_mcar_2019_std
)

resultado_mcar_2019_std

# ============================================================
# Test de Little para 2000, 2010 y 2023
# ============================================================

for (anio_elegido in c(2000, 2010, 2023)) {
  
  # Seleccionamos el año
  datos_mcar <- wdi_limpia |>
    filter(anio == anio_elegido) |>
    select(all_of(variables_relevantes))
  
  # Estandarizamos
  datos_mcar_std <- datos_mcar |>
    mutate(
      across(
        everything(),
        ~ as.numeric(scale(.))
      )
    )
  
  # Ejecutamos el test
  resultado <- naniar::mcar_test(datos_mcar_std)
  
  # Mostramos los resultados
  cat("\nAño:", anio_elegido, "\n")
  print(resultado)
}

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

# Ver la tabla completa
View(tabla_na_total)