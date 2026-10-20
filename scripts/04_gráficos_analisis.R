# ==============================================================================
# Gráficos correspondientes a la sección de Análisis de variables 
# ==============================================================================

library(patchwork)
library(ggplot2)

wdi_limpia <- read_csv("repo_wdi/wdi_limpia.csv")

# Población total por región y año ###

poblacion_region <- wdi_limpia |>
  filter(
    between(anio, 1960, 2025),
    !is.na(region),
    region != ""
  ) |>
  group_by(anio, region) |>
  summarise(
    # Si todos los datos faltan, conservar NA en lugar de generar un cero
    poblacion = if (all(is.na(poblacion_total))) {
      NA_real_
    } else {
      sum(poblacion_total, na.rm = TRUE)
    },
    .groups = "drop"
  )

# Gráfico de Evolución de la población por región ####

grafico_poblacion_region <- ggplot(
  poblacion_region,
  aes(
    x = anio,
    y = poblacion / 1e9,
    color = region
  )
) +
  geom_line(linewidth = 1.1) +
  facet_wrap(
    ~region,
    ncol = 3,
    scales = "free_y",
    labeller = label_wrap_gen(width = 32),
    axes = "all",
    axis.labels = "all"
  ) +
  scale_x_continuous(
    breaks = c(seq(1960, 2020, by = 10), 2025),
    limits = c(1960, 2025),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      decimal.mark = ",",
      big.mark = "."
    )
  ) +
  labs(
    title = "Evolución de la población por región",
    subtitle = "1960–2025 | Escala vertical diferente por región",
    x = "Año",
    y = "Población (miles de millones)"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

print(grafico_poblacion_region)

# Grafico sobre natalidad y mortalidad por región ####

# Calcular pesos poblacionales por región y año.

pesos_crecimiento_natural <- wdi_limpia |>
  filter(
    !is.na(region),
    region != "",
    region != "Aggregates",
    between(anio, 1960, 2025),
    !is.na(poblacion_total),
    poblacion_total > 0,
    !is.na(natalidad),
    !is.na(mortalidad)
  ) |>
  group_by(region, anio) |>
  mutate(
    poblacion_region_con_datos = sum(poblacion_total),
    peso_poblacional = poblacion_total / poblacion_region_con_datos
  ) |>
  ungroup()

# Calcular las tasas regionales ponderadas

crecimiento_natural_regional <- pesos_crecimiento_natural |>
  group_by(region, anio) |>
  summarise(
    natalidad_pct = sum(natalidad * peso_poblacional) / 10,
    mortalidad_pct = sum(mortalidad * peso_poblacional) / 10,
    .groups = "drop"
  ) |>
  mutate(
    crecimiento_natural_pct = natalidad_pct - mortalidad_pct
  )

# Pasar las dos tasas a formato largo para graficarlas

df_brecha_regional <- crecimiento_natural_regional |>
  select(region, anio, natalidad_pct, mortalidad_pct) |>
  pivot_longer(
    cols = c(natalidad_pct, mortalidad_pct),
    names_to = "tasa",
    values_to = "valor"
  )

# Graficar paneles por región

grafico_crecimiento_natural <- ggplot(
  df_brecha_regional,
  aes(x = anio, y = valor, color = tasa)
) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~region,
    ncol = 3,
    labeller = label_wrap_gen(width = 32),
    axes = "all_x",
    axis.labels = "all_x"
  ) +
  scale_x_continuous(
    breaks = c(1960, 1980, 2000, 2025),
    limits = c(1960, 2025),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_color_manual(
    values = c(
      natalidad_pct = "#2b5c8f",
      mortalidad_pct = "#d95f02"
    ),
    breaks = c("natalidad_pct", "mortalidad_pct"),
    labels = c("Natalidad (%)", "Mortalidad (%)")
  ) +
  labs(
    title = "Natalidad y mortalidad por región (1960–2025)",
    subtitle = "Tasas ponderadas por población de cada año",
    x = "Año",
    y = "Tasa (%)",
    color = "Componente",
    caption = paste(
      "La diferencia entre natalidad y mortalidad es el crecimiento natural.",
      "Se incluyen países con datos disponibles de ambas tasas y población."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

print(grafico_crecimiento_natural)

# # Gráfico media de la tasa de crecimiento poblacional ponderada por población ####

# Resume el crecimiento poblacional de cada país por década

crecimiento_por_decada <- wdi_limpia |>
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
  group_by(pais, codigo_pais, region, decada) |>
  summarise(
    anios_con_dato = sum(!is.na(crecimiento_poblacional)),
    media = if (anios_con_dato > 0) {
      mean(crecimiento_poblacional, na.rm = TRUE)
    } else {
      NA_real_
    },
    mediana = if (anios_con_dato > 0) {
      median(crecimiento_poblacional, na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |>
  arrange(pais, decada)

# Calcular el peso poblacional de cada país en cada año

pesos_poblacionales <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  select(
    pais, codigo_pais, region, anio,
    poblacion_total, crecimiento_poblacional
  ) |>
  filter(
    !is.na(poblacion_total),
    poblacion_total > 0,
    !is.na(crecimiento_poblacional)
  ) |>
  group_by(region, anio) |>
  mutate(
    poblacion_region_con_datos = sum(poblacion_total),
    peso_poblacional = poblacion_total / poblacion_region_con_datos,
    porcentaje_poblacion = peso_poblacional * 100,
    aporte_crecimiento = peso_poblacional * crecimiento_poblacional
  ) |>
  ungroup()

# Sumar los aportes para obtener la media ponderada regional

crecimiento_regional_ponderado <- pesos_poblacionales |>
  group_by(region, anio) |>
  summarise(
    economias_con_dato = n(),
    suma_pesos = sum(peso_poblacional),
    crecimiento_ponderado = sum(aporte_crecimiento),
    .groups = "drop"
  )

# Graficar la evolución anual

grafico_crecimiento_ponderado <- ggplot(
  crecimiento_regional_ponderado,
  aes(
    x = anio,
    y = crecimiento_ponderado,
    color = region,
    group = region
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray60"
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(1960, 2020, by = 10),
    limits = c(1960, 2025)
  ) +
  labs(
    title = "Crecimiento poblacional por región ponderado",
    subtitle = "Media de las tasas anuales ponderada por población de cada año",
    x = "Año",
    y = "Crecimiento anual ponderado (%)",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(ncol = 1))

print(grafico_crecimiento_ponderado)

cobertura_crecimiento <- wdi_limpia |>
  filter(
    anio >= 1960, anio <= 2025,
    !is.na(poblacion_total),
    poblacion_total > 0
  ) |>
  group_by(region, anio) |>
  summarise(
    economias_con_poblacion = n(),
    economias_con_ambos_datos = sum(!is.na(crecimiento_poblacional)),
    cobertura_poblacion = 100 *
      sum(poblacion_total[!is.na(crecimiento_poblacional)]) /
      sum(poblacion_total),
    .groups = "drop"
  )

# Comparar la tasa de crecimiento entre el período inicial y el reciente

comparacion_crecimiento <- crecimiento_regional_ponderado |>
  filter(
    between(anio, 1961, 1970) |
      between(anio, 2016, 2025)
  ) |>
  mutate(
    periodo = if_else(
      anio <= 1970,
      "1961_1970",
      "2016_2025"
    )
  ) |>
  group_by(region, periodo) |>
  summarise(
    tasa_promedio = mean(crecimiento_ponderado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = periodo,
    values_from = tasa_promedio,
    names_prefix = "tasa_"
  ) |>
  mutate(
    cambio_pp = tasa_2016_2025 - tasa_1961_1970,
    
    # Calcular el cambio relativo cuando la tasa inicial es positiva
    cambio_porcentual = if_else(
      tasa_1961_1970 > 0,
      (cambio_pp / tasa_1961_1970) * 100,
      NA_real_
    )
  ) |>
  arrange(cambio_porcentual) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

View(comparacion_crecimiento)

# Gráfico comparativo entre tasa de mortalidad y PBI per cápita PPA ####

# Seleccionar el año y formar cuartiles de PIB per cápita PPA

anio_elegido <- 2024

base_cuartiles <- wdi_limpia |>
  filter(
    anio == anio_elegido,
    !is.na(codigo_pais),
    !is.na(region),
    region != "Aggregates",
    is.finite(pib_per_capita_ppa),
    pib_per_capita_ppa > 0
  ) |>
  arrange(pib_per_capita_ppa, codigo_pais)

# Comprobar que haya una sola observación por país

stopifnot(
  nrow(base_cuartiles) >= 4,
  anyDuplicated(base_cuartiles$codigo_pais) == 0
)

base_cuartiles <- base_cuartiles |>
  mutate(
    cuartil = factor(
      ntile(pib_per_capita_ppa, 4),
      levels = 1:4,
      labels = c("Q1", "Q2", "Q3", "Q4")
    ),
    poblacion_valida = is.finite(poblacion_total) &
      poblacion_total > 0,
    datos_completos = poblacion_valida &
      is.finite(mortalidad) &
      is.finite(poblacion_65_mas)
  )

# Revisar composición y cobertura de cada cuartil

cobertura_cuartiles <- base_cuartiles |>
  group_by(cuartil) |>
  summarise(
    paises_clasificados = n(),
    paises_utilizados = sum(datos_completos),
    paises_sin_poblacion_valida = sum(!poblacion_valida),
    pib_ppa_min = min(pib_per_capita_ppa),
    pib_ppa_max = max(pib_per_capita_ppa),
    poblacion_conocida = sum(
      poblacion_total[poblacion_valida]
    ),
    poblacion_utilizada = sum(
      poblacion_total[datos_completos]
    ),
    .groups = "drop"
  ) |>
  mutate(
    cobertura_poblacion_pct = if_else(
      poblacion_conocida > 0,
      100 * poblacion_utilizada / poblacion_conocida,
      NA_real_
    )
  )

# Usar los mismos países para mortalidad y envejecimiento

datos_comparacion <- base_cuartiles |>
  filter(datos_completos)

#Evitar graficar si algún cuartil queda sin datos
if (n_distinct(datos_comparacion$cuartil) < 4) {
  stop("Algún cuartil no tiene países con datos completos. Revisá la cobertura.")
}

#Calcular las medias ponderadas por población
resumen_cuartiles <- datos_comparacion |>
  group_by(cuartil) |>
  summarise(
    mortalidad_ponderada = weighted.mean(
      mortalidad,
      w = poblacion_total
    ),
    mayores_65_ponderado = weighted.mean(
      poblacion_65_mas,
      w = poblacion_total
    ),
    .groups = "drop"
  )

# Tabla de resultados y controles
tabla_comparacion_cuartiles <- cobertura_cuartiles |>
  left_join(resumen_cuartiles, by = "cuartil")

print(
  tabla_comparacion_cuartiles |>
    mutate(across(where(is.numeric), ~ round(.x, 2))),
  width = Inf
)

# Preparar los dos paneles

datos_grafico_cuartiles <- resumen_cuartiles |>
  pivot_longer(
    cols = c(mortalidad_ponderada, mayores_65_ponderado),
    names_to = "indicador",
    values_to = "valor"
  ) |>
  mutate(
    indicador = factor(
      indicador,
      levels = c(
        "mortalidad_ponderada",
        "mayores_65_ponderado"
      ),
      labels = c(
        "Mortalidad bruta\n(defunciones por cada 1.000 habitantes)",
        "Población de 65 años o más\n(% de la población)"
      )
    ),
    etiqueta = scales::number(
      valor,
      accuracy = 0.1,
      decimal.mark = ","
    )
  )

colores_cuartiles <- c(
  "Q1" = "#C6DBEF",
  "Q2" = "#6BAED6",
  "Q3" = "#2171B5",
  "Q4" = "#08306B"
)

# Función para crear cada gráfico con su propia unidad

crear_panel <- function(variable, titulo, etiqueta_y) {
  
  ggplot(
    resumen_cuartiles,
    aes(
      x = cuartil,
      y = .data[[variable]],
      fill = cuartil
    )
  ) +
    geom_col(width = 0.6) +
    geom_text(
      aes(
        label = scales::number(
          .data[[variable]],
          accuracy = 0.1,
          decimal.mark = ","
        )
      ),
      vjust = -0.5,
      size = 5.5,
      fontface = "bold"
    ) +
    scale_fill_manual(values = colores_cuartiles) +
    scale_x_discrete(
      labels = c(
        "Q1" = "Q1\nMenor PIB",
        "Q2" = "Q2",
        "Q3" = "Q3",
        "Q4" = "Q4\nMayor PIB"
      )
    ) +
    scale_y_continuous(
      labels = scales::label_number(decimal.mark = ","),
      expand = expansion(mult = c(0, 0.16))
    ) +
    labs(
      title = titulo,
      x = "Cuartil de PIB per cápita PPA",
      y = etiqueta_y
    ) +
    theme_minimal(base_size = 17) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 19,
        hjust = 0.5,
        margin = margin(b = 15)
      ),
      axis.title.x = element_text(
        size = 14,
        margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        size = 14,
        margin = margin(r = 12)
      ),
      axis.text = element_text(size = 14, color = "gray20"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(15, 20, 15, 15)
    )
}

grafico_mortalidad <- crear_panel(
  variable = "mortalidad_ponderada",
  titulo = "Mortalidad bruta",
  etiqueta_y = "Defunciones por cada 1.000 habitantes"
)

grafico_envejecimiento <- crear_panel(
  variable = "mayores_65_ponderado",
  titulo = "Población de 65 años o más",
  etiqueta_y = "Población de 65 años o más (%)"
)

# Unir ambos gráficos
grafico_cuartiles <- (
  grafico_mortalidad | grafico_envejecimiento
) +
  plot_annotation(
    title = "Ingreso, mortalidad y envejecimiento",
    subtitle = paste0(
      anio_elegido,
      " | Indicadores ponderados por población | ",
      nrow(datos_comparacion),
      " países con datos completos"
    ),
    caption = paste0(
      "Fuente: WDI, Banco Mundial. Cuartiles con cantidades similares ",
      "de países; no equivalen a los grupos de ingreso del Banco Mundial.\n",
      "Ambos indicadores utilizan los mismos países y pesos poblacionales. ",
      "La mortalidad bruta no está ajustada por edad."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 25),
      plot.subtitle = element_text(
        size = 15,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = 10,
        hjust = 0,
        margin = margin(t = 12)
      ),
      plot.margin = margin(15, 20, 15, 15)
    )
  )

print(grafico_cuartiles)

# Comparación estática con media de edad +65 por grupo económico ####

anio_inicial <- 1990
anio_actual  <- 2024

base_65 <- wdi_limpia |>
  filter(
    anio %in% c(anio_inicial, anio_actual),
    !is.na(poblacion_65_mas),
    !is.na(ingreso),
    ingreso != "Sin clasificar"
  )

resumen_65 <- base_65 |>
  group_by(anio, ingreso) |>
  summarise(
    n_paises = n(),
    mediana_65 = median(poblacion_65_mas, na.rm = TRUE),
    .groups = "drop"
  )

limite_y <- ceiling(max(resumen_65$mediana_65, na.rm = TRUE) / 5) * 5

grafico_actual <- resumen_65 |>
  filter(anio == anio_actual) |>
  ggplot(aes(x = ingreso, y = mediana_65)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(mediana_65, 1), "%")),
    vjust = -0.4
  ) +
  scale_y_continuous(
    limits = c(0, limite_y),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = paste0("Población de 65 años y más en ", anio_actual),
    subtitle = "Mediana de los países según su grupo de ingreso actual",
    x = "Grupo de ingreso",
    y = "% de población de 65 años y más"
  ) +
  theme_minimal()

grafico_1990 <- resumen_65 |>
  filter(anio == anio_inicial) |>
  ggplot(aes(x = ingreso, y = mediana_65)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(mediana_65, 1), "%")),
    vjust = -0.4
  ) +
  scale_y_continuous(
    limits = c(0, limite_y),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Población de 65 años y más en 1990",
    subtitle = "Mediana de los países según su grupo de ingreso actual",
    x = "Grupo de ingreso",
    y = "% de población de 65 años y más"
  ) +
  theme_minimal()

grafico_1990
grafico_actual

# GRÁFICO 1: 1990
grafico_1990 <- resumen_65 |>
  filter(anio == anio_inicial) |>
  ggplot(aes(x = ingreso, y = mediana_65)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(mediana_65, 1), "%")),
    vjust = -0.2,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, limite_y),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Población de 65 años y más en 1990",
    subtitle = "Mediana de los países según su grupo de ingreso actual",
    x = "Grupo de ingreso",
    y = "% de población de 65 años y más"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(5, 5, 5, 5)
  )


# GRÁFICO 2: ACTUALIDAD
grafico_actual <- resumen_65 |>
  filter(anio == anio_actual) |>
  ggplot(aes(x = ingreso, y = mediana_65)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(mediana_65, 1), "%")),
    vjust = -0.2,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, limite_y),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = paste0("Población de 65 años y más en ", anio_actual),
    subtitle = "Mediana de los países según su grupo de ingreso actual",
    x = "Grupo de ingreso",
    y = "% de población de 65 años y más"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(5, 5, 5, 5)
  )
