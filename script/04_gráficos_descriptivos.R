# ==============================================================================
# Gráficos correspondientes a la sección de Análisis de variables 
# ==============================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(ggplot2)

wdi_limpia <- read_csv("input/wdi_limpia.csv")

# Evolución de la población por región #####

# Detectar automáticamente la columna de población total si tuviera otro nombre
col_pob <- intersect(
  c("poblacion_total", "poblacion", "pob_total", "SP.POP.TOTL"),
  names(wdi_limpia)
)[1]

if (is.na(col_pob)) {
  stop(
    "No encontré la columna de población total. Columnas disponibles: ",
    paste(names(wdi_limpia), collapse = ", ")
  )
}

poblacion_region <- wdi_limpia |>
  filter(
    between(as.numeric(anio), 1960, 2025),
    !is.na(region),
    region != ""
  ) |>
  mutate(
    anio = as.numeric(anio),
    pob_num = as.numeric(.data[[col_pob]])
  ) |>
  group_by(anio, region) |>
  summarise(
    poblacion = if (all(is.na(pob_num))) {
      NA_real_
    } else {
      sum(pob_num, na.rm = TRUE)
    },
    .groups = "drop"
  )

orden_regiones <- c(
  "África Subsahariana",
  "América del Norte",
  "América Latina y el Caribe",
  "Asia Meridional",
  "Asia Oriental y Pacífico",
  "Europa y Asia Central",
  "Oriente Medio, Norte de África, Afganistán y Pakistán"
)

paleta_regiones <- c(
  "África Subsahariana" = "#00856A",
  "América del Norte" = "#C85A17",
  "América Latina y el Caribe" = "#7E6BA8",
  "Asia Meridional" = "#C82878",
  "Asia Oriental y Pacífico" = "#6A9E1F",
  "Europa y Asia Central" = "#D99B00",
  "Oriente Medio, Norte de África, Afganistán y Pakistán" = "#8C6D1F"
)

datos_grafico <- poblacion_region |>
  mutate(region = factor(region, levels = orden_regiones)) |>
  filter(!is.na(region), !is.na(poblacion))

grafico_poblacion_region <- ggplot(
  datos_grafico,
  aes(
    x = anio,
    y = poblacion / 1e9,
    color = region,
    group = region
  )
) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  facet_wrap(
    ~region,
    ncol = 3,
    scales = "free_y",
    labeller = label_wrap_gen(width = 32),
    axes = "all",
    axis.labels = "all"
  ) +
  scale_color_manual(values = paleta_regiones, drop = FALSE) +
  scale_x_continuous(
    breaks = c(seq(1960, 2020, by = 10), 2025),
    limits = c(1960, 2025),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  scale_y_continuous(
    labels = label_number(
      decimal.mark = ",",
      big.mark = "."
    )
  ) +
  labs(
    title = "Evolución de la población por región",
    subtitle = "1960–2025 | Escala vertical diferente por región",
    x = "Año",
    y = "Población (miles de millones)",
    caption = "Fuente: Banco Mundial (WDI). Elaboración propia. Cada panel usa su propia escala vertical para apreciar su tendencia."
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      family = "sans", face = "bold", size = 16,
      color = "#19354D", hjust = 0, margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      family = "sans", size = 11,
      color = "#38536B", hjust = 0, margin = margin(b = 14)
    ),
    plot.caption = element_text(
      family = "sans", size = 8.8,
      color = "#52677A", hjust = 0, lineheight = 1.2,
      margin = margin(t = 12)
    ),
    legend.position = "none",
    axis.title.x = element_text(
      family = "sans", face = "bold", size = 10.5,
      color = "#19354D", margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      family = "sans", face = "bold", size = 10.5,
      color = "#19354D", margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      family = "sans", angle = 45, hjust = 1, vjust = 1,
      size = 8.5, color = "#677887"
    ),
    axis.text.y = element_text(
      family = "sans", size = 8.5, color = "#677887",
      margin = margin(r = 4)
    ),
    strip.text = element_text(
      family = "sans", face = "bold", size = 9.5,
      color = "#19354D", hjust = 0.5, margin = margin(t = 4, b = 8)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5EBEF", linewidth = 0.35),
    panel.spacing.x = unit(18, "pt"),
    panel.spacing.y = unit(18, "pt"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 22, 16, 20)
  )

print(grafico_poblacion_region)


# Natalidad y mortalidad por región (1960-2025) #####

# 1. Detectar columnas de poblacion, natalidad y mortalidad en wdi_limpia
col_pob <- intersect(
  c("poblacion_total", "poblacion", "pob_total", "SP.POP.TOTL"),
  names(wdi_limpia)
)[1]

col_nat <- intersect(
  c("tasa_natalidad", "natalidad", "tasa_bruta_natalidad", "SP.DYN.CBRT.IN"),
  names(wdi_limpia)
)[1]

col_mor <- intersect(
  c("tasa_mortalidad", "mortalidad", "tasa_bruta_mortalidad", "SP.DYN.CDRT.IN"),
  names(wdi_limpia)
)[1]

if (any(is.na(c(col_pob, col_nat, col_mor)))) {
  stop(
    "Revisa los nombres de columnas en wdi_limpia. Columnas disponibles: ",
    paste(names(wdi_limpia), collapse = ", ")
  )
}

# 2. Calculo de tasas ponderadas por poblacion en cada anio y region
tasas_region <- wdi_limpia |>
  filter(
    between(as.numeric(anio), 1960, 2025),
    !is.na(region),
    region != ""
  ) |>
  mutate(
    anio = as.numeric(anio),
    pob = as.numeric(.data[[col_pob]]),
    nat = as.numeric(.data[[col_nat]]),
    mor = as.numeric(.data[[col_mor]])
  ) |>
  filter(
    !is.na(pob), pob > 0,
    !is.na(nat),
    !is.na(mor)
  ) |>
  group_by(anio, region) |>
  summarise(
    natalidad_pct = sum(nat * pob, na.rm = TRUE) / sum(pob, na.rm = TRUE) / 10,
    mortalidad_pct = sum(mor * pob, na.rm = TRUE) / sum(pob, na.rm = TRUE) / 10,
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(natalidad_pct, mortalidad_pct),
    names_to = "componente",
    values_to = "tasa"
  ) |>
  mutate(
    componente = recode(
      componente,
      natalidad_pct = "Natalidad (%)",
      mortalidad_pct = "Mortalidad (%)"
    ),
    componente = factor(
      componente,
      levels = c("Natalidad (%)", "Mortalidad (%)")
    )
  )

orden_regiones <- c(
  "África Subsahariana",
  "América del Norte",
  "América Latina y el Caribe",
  "Asia Meridional",
  "Asia Oriental y Pacífico",
  "Europa y Asia Central",
  "Oriente Medio, Norte de África, Afganistán y Pakistán"
)

datos_tasas <- tasas_region |>
  mutate(region = factor(region, levels = orden_regiones)) |>
  filter(!is.na(region))

# 3. Construccion del grafico con la nueva paleta de colores
grafico_natalidad_mortalidad <- ggplot(
  datos_tasas,
  aes(
    x = anio,
    y = tasa,
    color = componente,
    group = componente
  )
) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  facet_wrap(
    ~region,
    ncol = 3,
    labeller = label_wrap_gen(width = 32)
  ) +
  scale_color_manual(
    values = c(
      "Natalidad (%)" = "#0072B2",
      "Mortalidad (%)" = "#E69F00"
    ),
    name = "Componente"
  ) +
  scale_x_continuous(
    breaks = c(1960, 1980, 2000, 2025),
    limits = c(1960, 2025),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(
    breaks = 0:5,
    limits = c(0, 5),
    labels = label_number(decimal.mark = ",", big.mark = ".")
  ) +
  labs(
    title = "Natalidad y mortalidad por región (1960–2025)",
    subtitle = "Tasas ponderadas por población de cada año",
    x = "Año",
    y = "Tasa (%)",
    caption = paste0(
      "Fuente: Banco Mundial (WDI). Elaboración propia. ",
      "La diferencia entre natalidad y mortalidad es el crecimiento natural. ",
      "Se incluyen países con datos disponibles de ambas tasas y población."
    )
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      family = "sans", face = "bold", size = 16,
      color = "#19354D", hjust = 0, margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      family = "sans", size = 11,
      color = "#38536B", hjust = 0, margin = margin(b = 14)
    ),
    plot.caption = element_text(
      family = "sans", size = 8.8,
      color = "#52677A", hjust = 0, lineheight = 1.2,
      margin = margin(t = 12)
    ),
    legend.position = "bottom",
    legend.title = element_text(
      family = "sans", face = "bold", size = 10,
      color = "#19354D"
    ),
    legend.text = element_text(
      family = "sans", size = 9.8,
      color = "#38536B"
    ),
    axis.title.x = element_text(
      family = "sans", face = "bold", size = 10.5,
      color = "#19354D", margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      family = "sans", face = "bold", size = 10.5,
      color = "#19354D", margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      family = "sans", size = 8.5, color = "#677887",
      margin = margin(t = 4)
    ),
    axis.text.y = element_text(
      family = "sans", size = 8.5, color = "#677887",
      margin = margin(r = 4)
    ),
    strip.text = element_text(
      family = "sans", face = "bold", size = 9.5,
      color = "#19354D", hjust = 0.5, margin = margin(t = 4, b = 8)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5EBEF", linewidth = 0.35),
    panel.spacing.x = unit(16, "pt"),
    panel.spacing.y = unit(16, "pt"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 22, 16, 20)
  )

print(grafico_natalidad_mortalidad)




# Crecimiento poblacional por región, ponderado ####

# 1. Detectar columnas de poblacion y crecimiento poblacional en wdi_limpia
col_pob <- intersect(
  c("poblacion_total", "poblacion", "pob_total", "SP.POP.TOTL"),
  names(wdi_limpia)
)[1]

col_crec <- intersect(
  c("crecimiento_poblacional", "tasa_crecimiento_poblacional", "crecimiento", "SP.POP.GROW"),
  names(wdi_limpia)
)[1]

if (any(is.na(c(col_pob, col_crec)))) {
  stop(
    "Revisa los nombres de columnas en wdi_limpia. Columnas disponibles: ",
    paste(names(wdi_limpia), collapse = ", ")
  )
}

# 2. Calculo del crecimiento anual ponderado por poblacion en cada anio y region
crecimiento_region <- wdi_limpia |>
  filter(
    between(as.numeric(anio), 1960, 2025),
    !is.na(region),
    region != ""
  ) |>
  mutate(
    anio = as.numeric(anio),
    pob = as.numeric(.data[[col_pob]]),
    crec = as.numeric(.data[[col_crec]])
  ) |>
  filter(
    !is.na(pob), pob > 0,
    !is.na(crec)
  ) |>
  group_by(anio, region) |>
  summarise(
    crecimiento_ponderado = sum(crec * pob, na.rm = TRUE) / sum(pob, na.rm = TRUE),
    .groups = "drop"
  )

orden_regiones <- c(
  "África Subsahariana",
  "América del Norte",
  "América Latina y el Caribe",
  "Asia Meridional",
  "Asia Oriental y Pacífico",
  "Europa y Asia Central",
  "Oriente Medio, Norte de África, Afganistán y Pakistán"
)

paleta_regiones <- c(
  "África Subsahariana" = "#E07A67",
  "América del Norte" = "#B89B1E",
  "América Latina y el Caribe" = "#6CA62B",
  "Asia Meridional" = "#009E73",
  "Asia Oriental y Pacífico" = "#0099D8",
  "Europa y Asia Central" = "#8D72D6",
  "Oriente Medio, Norte de África, Afganistán y Pakistán" = "#E052B6"
)

datos_crecimiento <- crecimiento_region |>
  mutate(region = factor(region, levels = orden_regiones)) |>
  filter(!is.na(region))

# 3. Construccion del grafico con el estilo de la Entrega 3
grafico_crecimiento_region <- ggplot(
  datos_crecimiento,
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
    color = "#B8C4CE",
    linewidth = 0.45
  ) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  scale_color_manual(
    values = paleta_regiones,
    name = "Región",
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(1960, 2020, by = 10),
    limits = c(1960, 2025),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    breaks = 0:3,
    labels = label_number(decimal.mark = ",", big.mark = ".")
  ) +
  labs(
    title = "Crecimiento poblacional por región ponderado",
    subtitle = "Media de las tasas anuales ponderada por población de cada año",
    x = "Año",
    y = "Crecimiento anual ponderado (%)",
    caption = "Fuente: Banco Mundial (WDI). Elaboración propia. Tasas anuales de crecimiento poblacional ponderadas por población."
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      family = "sans", face = "bold", size = 16,
      color = "#19354D", hjust = 0, margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      family = "sans", size = 11,
      color = "#38536B", hjust = 0, margin = margin(b = 14)
    ),
    plot.caption = element_text(
      family = "sans", size = 8.8,
      color = "#52677A", hjust = 0, lineheight = 1.2,
      margin = margin(t = 12)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(
      family = "sans", face = "bold", size = 10,
      color = "#19354D"
    ),
    legend.text = element_text(
      family = "sans", size = 9.5,
      color = "#38536B"
    ),
    axis.title.x = element_text(
      family = "sans", face = "bold", size = 10.5,
      color = "#19354D", margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      family = "sans", face = "bold", size = 10.5,
      color = "#19354D", margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      family = "sans", size = 9, color = "#677887",
      margin = margin(t = 4)
    ),
    axis.text.y = element_text(
      family = "sans", size = 9, color = "#677887",
      margin = margin(r = 4)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5EBEF", linewidth = 0.35),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 22, 16, 20)
  ) +
  guides(
    color = guide_legend(
      ncol = 1,
      byrow = TRUE
    )
  )

print(grafico_crecimiento_region)

# Población de 65 años y más en 2024, por grupo de ingreso ####

# 1. Detectar columnas de grupo de ingreso y poblacion de 65+ en wdi_limpia
col_ingreso <- intersect(
  c("grupo_ingreso", "grupo_de_ingreso", "ingreso", "income_group", "region_ingreso"),
  names(wdi_limpia)
)[1]

col_65 <- intersect(
  c("poblacion_65_mas", "pob_65_mas", "mayores_65", "SP.POP.65UP.TO.ZS"),
  names(wdi_limpia)
)[1]

if (any(is.na(c(col_ingreso, col_65)))) {
  stop(
    "Revisa los nombres de columnas en wdi_limpia. Columnas disponibles: ",
    paste(names(wdi_limpia), collapse = ", ")
  )
}

# 2. Mediana por grupo de ingreso en 2024
# Si en tu base el porcentaje de 65+ viene en proporcion (ej. 0.187 en vez de 18.7),
# cambia el multiplicador a 100. Si ya viene en porcentaje (ej. 18.7), dejalo en 1.
factor_pct <- 1

mediana_ingreso_2024 <- wdi_limpia |>
  filter(
    as.numeric(anio) == 2024,
    !is.na(.data[[col_ingreso]]),
    .data[[col_ingreso]] != "",
    !is.na(.data[[col_65]])
  ) |>
  mutate(
    grupo = str_trim(as.character(.data[[col_ingreso]])),
    valor_65 = as.numeric(.data[[col_65]]) * factor_pct
  ) |>
  group_by(grupo) |>
  summarise(
    mediana = median(valor_65, na.rm = TRUE),
    .groups = "drop"
  )

# Orden de categorias y paleta (de menor a mayor / distinta por grupo)
orden_grupos <- c(
  "Ingreso bajo",
  "Ingreso medio-bajo",
  "Ingreso medio-alto",
  "Ingreso alto"
)

paleta_ingreso <- c(
  "Ingreso bajo"       = "#0072B2",
  "Ingreso medio-bajo" = "#4A8FC2",
  "Ingreso medio-alto" = "#F2B84B",
  "Ingreso alto"       = "#E69F00"
)

datos_barras <- mediana_ingreso_2024 |>
  mutate(
    grupo = factor(grupo, levels = orden_grupos),
    etiqueta = if_else(
      abs(mediana - round(mediana)) < 0.05,
      paste0(round(mediana), "%"),
      paste0(formatC(round(mediana, 1), format = "f", digits = 1, decimal.mark = "."), "%")
    )
  ) |>
  filter(!is.na(grupo))

# 3. Construccion del grafico con el estilo de la Entrega 3
grafico_65_ingreso <- ggplot(
  datos_barras,
  aes(x = grupo, y = mediana, fill = grupo)
) +
  geom_col(width = 0.68, show.legend = FALSE) +
  geom_text(
    aes(label = etiqueta),
    vjust = -0.55,
    family = "sans",
    fontface = "bold",
    size = 3.8,
    color = "#19354D"
  ) +
  scale_fill_manual(values = paleta_ingreso, drop = FALSE) +
  scale_y_continuous(
    breaks = seq(0, 20, by = 5),
    limits = c(0, max(22, max(datos_barras$mediana, na.rm = TRUE) * 1.15)),
    labels = label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    title = "Población de 65 años y más en 2024",
    subtitle = "Mediana de los países según su grupo de ingreso actual",
    x = "Grupo de ingreso",
    y = "% de población de 65 años y más",
    caption = "Fuente: Banco Mundial (WDI). Elaboración propia. Mediana del porcentaje de población de 65 años y más en 2024 por grupo de ingreso."
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      family = "sans", face = "bold", size = 16,
      color = "#19354D", hjust = 0, margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      family = "sans", size = 11,
      color = "#38536B", hjust = 0, margin = margin(b = 14)
    ),
    plot.caption = element_text(
      family = "sans", size = 8.8,
      color = "#52677A", hjust = 0, lineheight = 1.2,
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      family = "sans", face = "bold", size = 10.5,
      color = "#19354D", margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      family = "sans", face = "bold", size = 10.5,
      color = "#19354D", margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      family = "sans", size = 9.5, color = "#38536B",
      margin = margin(t = 5)
    ),
    axis.text.y = element_text(
      family = "sans", size = 9, color = "#677887",
      margin = margin(r = 4)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E5EBEF", linewidth = 0.35),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 22, 16, 20)
  )

print(grafico_65_ingreso)



carpeta_outputs <- file.path("output", "graficos")
dir.create(carpeta_outputs, recursive = TRUE, showWarnings = FALSE)

lista_graficos <- list(
  "evolucion_poblacion_region.jpg"     = list(plot = grafico_poblacion_region,     w = 25, h = 22),
  "natalidad_mortalidad_region.jpg"    = list(plot = grafico_natalidad_mortalidad, w = 25, h = 22),
  "crecimiento_poblacional_region.jpg" = list(plot = grafico_crecimiento_region,   w = 24, h = 18),
  "poblacion_65_mas_ingreso_2024.jpg"  = list(plot = grafico_65_ingreso,           w = 22, h = 16)
)

for (nombre in names(lista_graficos)) {
  ggplot2::ggsave(
    filename = file.path(carpeta_outputs, nombre),
    plot = lista_graficos[[nombre]]$plot,
    width = lista_graficos[[nombre]]$w,
    height = lista_graficos[[nombre]]$h,
    units = "cm",
    dpi = 300,
    bg = "white",
    quality = 95
  )
}