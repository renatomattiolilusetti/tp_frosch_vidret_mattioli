# ==============================================================================
# Estadísticas descriptivas antes y después del tratamiento de NA y outliers
# ==============================================================================

# Paquetes
library(tidyverse)
library(scales)
library(gt)

# Crear las carpetas de resultados si no existen
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

# Este script compara las bases que generan los scripts 04, 07, 08 y 10
archivos_necesarios <- c(
  "input/wdi_limpia.csv",
  "output/tablas/outliers_horizontales_wdi.csv",
  "auxiliar/base_cluster_demografico_cortes.csv",
  "output/tablas/capacidad_2023_clusters.csv",
  "output/tablas/base_demo_interpolada.csv"
)

archivos_faltantes <- archivos_necesarios[!file.exists(archivos_necesarios)]

if (length(archivos_faltantes) > 0) {
  stop("Faltan archivos necesarios: ", paste(archivos_faltantes, collapse = ", "))
}

# Lectura de las bases
wdi_limpia <- read_csv("input/wdi_limpia.csv", show_col_types = FALSE)
outliers_wdi <- read_csv("output/tablas/outliers_horizontales_wdi.csv", show_col_types = FALSE)
base_cluster <- read_csv("auxiliar/base_cluster_demografico_cortes.csv", show_col_types = FALSE)
capacidad_2023 <- read_csv("output/tablas/capacidad_2023_clusters.csv", show_col_types = FALSE)
base_interpolada <- read_csv("output/tablas/base_demo_interpolada.csv", show_col_types = FALSE)

vars_demograficas <- c("poblacion_65_mas", "fecundidad", "esperanza_vida", "crecimiento_poblacional")
vars_capacidad <- c("indice_cobertura_salud", "gasto_bolsillo_salud", "pib_per_capita_ppa")

# Nombre y unidad de cada variable para las tablas
etiquetas_post <- c(
  poblacion_65_mas = "Población de 65+ (% del total)",
  fecundidad = "Fecundidad (hijos por mujer)",
  esperanza_vida = "Esperanza de vida (años)",
  crecimiento_poblacional = "Crecimiento poblacional (% anual)",
  dependencia_mayor = "Dependencia de mayores (65+ por cada 100 de 15 a 64)",
  gasto_salud_pib = "Gasto en salud (% del PIB)",
  indice_cobertura_salud = "Cobertura de servicios de salud (índice 0-100)",
  gasto_bolsillo_salud = "Gasto de bolsillo (% del gasto en salud)",
  pib_per_capita_ppa = "PIB per cápita PPA (USD constantes de 2021)"
)

# 1. Armado de las comparaciones ####

# Cada bloque junta la base original ("antes") y la tratada ("despues") en formato largo

# 1.1. Clustering por año: países con las cuatro variables en los cuatro cortes
bloque_cluster <- bind_rows(
  wdi_limpia |>
    filter(anio %in% c(1980, 2024)) |>
    select(codigo_pais, anio, all_of(vars_demograficas)) |>
    mutate(version = "antes"),
  base_cluster |>
    filter(anio %in% c(1980, 2024)) |>
    select(codigo_pais, anio, all_of(vars_demograficas)) |>
    mutate(version = "despues")
) |>
  pivot_longer(all_of(vars_demograficas), names_to = "variable", values_to = "valor") |>
  mutate(
    anio = as.character(anio),
    tratamiento = "Clustering por año: países con las 4 variables en los 4 cortes"
  )

# 1.2. Cluster continuo: exclusión de Cisjordania y Gaza e interpolación de faltantes
bloque_interpolacion <- bind_rows(
  wdi_limpia |>
    filter(between(anio, 1961, 2024)) |>
    select(codigo_pais, anio, all_of(vars_demograficas)) |>
    mutate(version = "antes"),
  base_interpolada |>
    select(codigo_pais, anio, all_of(vars_demograficas)) |>
    mutate(version = "despues")
) |>
  pivot_longer(all_of(vars_demograficas), names_to = "variable", values_to = "valor") |>
  mutate(
    anio = "1961-2024",
    tratamiento = "Cluster continuo: exclusión de Cisjordania y Gaza e interpolación lineal"
  )

# 1.3. Outliers: se conservan. Mostramos que sacarlos no cambia mucho las descriptivas
cluster_2024_largo <- base_cluster |>
  filter(anio == 2024) |>
  select(codigo_pais, anio, all_of(vars_demograficas)) |>
  pivot_longer(all_of(vars_demograficas), names_to = "variable", values_to = "valor")

outliers_2024 <- outliers_wdi |>
  filter(anio == 2024, variable %in% vars_demograficas) |>
  select(codigo_pais, anio, variable)

bloque_outliers <- bind_rows(
  cluster_2024_largo |>
    mutate(version = "antes"),
  cluster_2024_largo |>
    anti_join(outliers_2024, by = c("codigo_pais", "anio", "variable")) |>
    mutate(version = "despues")
) |>
  mutate(
    anio = as.character(anio),
    tratamiento = "Outliers 2024: se conservan (la base tratada es solo una prueba sin ellos)"
  )

# 1.4. Regresiones 2019: esperanza de vida de República Centroafricana como NA
esperanza_2019 <- wdi_limpia |>
  filter(anio == 2019) |>
  transmute(codigo_pais, anio, variable = "esperanza_vida", valor = esperanza_vida)

bloque_caf <- bind_rows(
  esperanza_2019 |>
    mutate(version = "antes"),
  esperanza_2019 |>
    mutate(
      valor = if_else(codigo_pais == "CAF", NA_real_, valor),
      version = "despues"
    )
) |>
  mutate(
    anio = as.character(anio),
    tratamiento = "Regresiones 2019: esperanza de vida de República Centroafricana como NA"
  )

# 1.5. Quintiles 2019: países con dependencia mayor y gasto en salud
quintiles_2019 <- wdi_limpia |>
  filter(anio == 2019) |>
  transmute(
    codigo_pais, anio,
    dependencia_mayor = 100 * poblacion_65_mas / poblacion_15_64,
    gasto_salud_pib
  )

bloque_quintiles <- bind_rows(
  quintiles_2019 |>
    mutate(version = "antes"),
  quintiles_2019 |>
    drop_na(dependencia_mayor, gasto_salud_pib) |>
    mutate(version = "despues")
) |>
  pivot_longer(c(dependencia_mayor, gasto_salud_pib), names_to = "variable", values_to = "valor") |>
  mutate(
    anio = as.character(anio),
    tratamiento = "Quintiles 2019: países con dependencia mayor y gasto en salud"
  )

# 1.6. Capacidad de respuesta 2023: casos completos en las tres variables
bloque_capacidad <- bind_rows(
  wdi_limpia |>
    filter(anio == 2023) |>
    select(codigo_pais, anio, all_of(vars_capacidad)) |>
    mutate(version = "antes"),
  capacidad_2023 |>
    select(codigo_pais, anio, all_of(vars_capacidad)) |>
    mutate(version = "despues")
) |>
  pivot_longer(all_of(vars_capacidad), names_to = "variable", values_to = "valor") |>
  mutate(
    anio = as.character(anio),
    tratamiento = "Capacidad de respuesta 2023: casos completos en las tres variables"
  )

# 2. Comparación antes y después ####

orden_tratamientos <- c(
  unique(bloque_cluster$tratamiento),
  unique(bloque_interpolacion$tratamiento),
  unique(bloque_outliers$tratamiento),
  unique(bloque_caf$tratamiento),
  unique(bloque_quintiles$tratamiento),
  unique(bloque_capacidad$tratamiento)
)

comparacion_post <- bind_rows(
  bloque_cluster, bloque_interpolacion, bloque_outliers,
  bloque_caf, bloque_quintiles, bloque_capacidad
) |>
  summarise(
    n = sum(!is.na(valor)),
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    desvio = sd(valor, na.rm = TRUE),
    .by = c(tratamiento, variable, anio, version)
  ) |>
  pivot_wider(
    names_from = version,
    values_from = c(n, media, mediana, desvio),
    names_glue = "{.value}_{version}"
  ) |>
  mutate(
    cambio_media = 100 * (media_despues / media_antes - 1),
    etiqueta = unname(etiquetas_post[variable]),
    tratamiento = factor(tratamiento, levels = orden_tratamientos)
  ) |>
  arrange(tratamiento, factor(variable, levels = names(etiquetas_post)), anio)

print(comparacion_post, n = Inf, width = Inf)

write_csv(comparacion_post, "output/tablas/descriptivas_antes_despues.csv")

# 3. Tabla resumida para la diapositiva ####

# Una fila por tratamiento: la variable con el mayor cambio en la media,
# con su cantidad de observaciones antes y después
resumen_tratamientos <- comparacion_post |>
  slice_max(abs(cambio_media), n = 1, with_ties = FALSE, by = tratamiento) |>
  transmute(
    tratamiento,
    n_antes,
    n_despues,
    mayor_cambio = cambio_media,
    variable_mayor_cambio = etiqueta
  )

print(resumen_tratamientos, width = Inf)

tabla_resumen_post <- resumen_tratamientos |>
  gt() |>
  cols_label(
    tratamiento = "Tratamiento",
    n_antes = "N antes",
    n_despues = "N después",
    mayor_cambio = "Mayor cambio de la media (%)",
    variable_mayor_cambio = "Variable con el mayor cambio"
  ) |>
  fmt_number(columns = c(n_antes, n_despues), decimals = 0, sep_mark = ".") |>
  fmt_number(columns = mayor_cambio, decimals = 2, dec_mark = ",", sep_mark = ".") |>
  cols_align(align = "left", columns = c(tratamiento, variable_mayor_cambio)) |>
  cols_align(align = "center", columns = c(n_antes, n_despues, mayor_cambio)) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_source_note(
    source_note = "Fuente: Banco Mundial (WDI). Elaboración propia. N: observaciones con dato (país-año en el cluster continuo)."
  ) |>
  tab_options(
    table.font.size = px(14),
    data_row.padding = px(6)
  )

tabla_resumen_post

gtsave(
  tabla_resumen_post,
  "output/tablas/tabla_resumen_antes_despues.png",
  zoom = 2
)

# 4. Tablas completas para el anexo ####

# Partimos la tabla en dos para que se lea en la diapositiva:
# 4A con el clustering por año y el continuo, y 4B con el resto de los tratamientos
tratamientos_4a <- orden_tratamientos[1:2]

for (parte in c("a", "b")) {
  
  datos_parte <- if (parte == "a") {
    comparacion_post |> filter(tratamiento %in% tratamientos_4a)
  } else {
    comparacion_post |> filter(!tratamiento %in% tratamientos_4a)
  }
  
  tabla_parte <- datos_parte |>
    mutate(tratamiento = as.character(tratamiento)) |>
    select(
      tratamiento, variable, etiqueta, anio,
      n_antes, media_antes, mediana_antes, desvio_antes,
      n_despues, media_despues, mediana_despues, desvio_despues,
      cambio_media
    ) |>
    gt(groupname_col = "tratamiento") |>
    cols_hide(columns = variable) |>
    tab_spanner(label = "Base original", columns = ends_with("_antes")) |>
    tab_spanner(label = "Base tratada", columns = ends_with("_despues")) |>
    cols_label(
      etiqueta = "Variable",
      anio = "Año",
      n_antes = "N", media_antes = "Media", mediana_antes = "Mediana", desvio_antes = "Desvío",
      n_despues = "N", media_despues = "Media", mediana_despues = "Mediana", desvio_despues = "Desvío",
      cambio_media = "Cambio de la media (%)"
    ) |>
    fmt_number(
      columns = c(starts_with("media"), starts_with("mediana"), starts_with("desvio"), cambio_media),
      decimals = 2, dec_mark = ",", sep_mark = "."
    ) |>
    # Solo el PIB per cápita va sin decimales (el gasto en % del PIB sí los lleva)
    fmt_number(
      columns = c(starts_with("media"), starts_with("mediana"), starts_with("desvio")),
      rows = variable == "pib_per_capita_ppa",
      decimals = 0, dec_mark = ",", sep_mark = "."
    ) |>
    fmt_number(columns = starts_with("n_"), decimals = 0, sep_mark = ".") |>
    tab_header(
      title = "Estadísticas descriptivas antes y después del tratamiento de faltantes y outliers",
      subtitle = "Unidad de observación: país (país-año en el cluster continuo)"
    ) |>
    cols_align(align = "left", columns = etiqueta) |>
    cols_align(align = "center", columns = -etiqueta) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(cells_column_labels(), cells_column_spanners(), cells_row_groups())
    ) |>
    tab_source_note(source_note = "Fuente: Banco Mundial (WDI). Elaboración propia.") |>
    tab_options(
      table.font.size = px(13),
      heading.title.font.size = px(17),
      heading.subtitle.font.size = px(12),
      data_row.padding = px(5)
    )
  
  print(tabla_parte)
  
  gtsave(
    tabla_parte,
    paste0("output/tablas/tabla_antes_despues_", parte, ".png"),
    zoom = 2
  )
}

# 5. Control final de archivos generados ####

archivos_generados <- c(
  "output/tablas/descriptivas_antes_despues.csv",
  "output/tablas/tabla_resumen_antes_despues.png",
  "output/tablas/tabla_antes_despues_a.png",
  "output/tablas/tabla_antes_despues_b.png"
)

stopifnot(all(file.exists(archivos_generados)))

print("13 finalizado: todos los archivos fueron generados.")