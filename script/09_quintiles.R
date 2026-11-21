# ==============================================================================
# Análisis de quintiles: aplicado a tasa de dependencia mayor y gasto en salud 
# ==============================================================================

# Paquetes
library(tidyverse)
library(scales)

# Función estandarizada para exportar los gráficos 
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

guardar_grafico <- function(plot, nombre_archivo, ancho = 10, alto = 6.5) {
  ruta <- file.path("output/graficos", nombre_archivo)
  ggsave(
    filename = ruta,
    plot = plot,
    width = ancho,
    height = alto,
    units = "in",
    dpi = 300,
    device = "jpeg",
    quality = 95
  )
  message("Gráfico guardado en: ", ruta)
}

# Paleta Okabe-Ito para los 5 quintiles (de menor a mayor carga demográfica)
paleta_quintiles <- c(
  "Q1 (Baja)"       = "#56B4E9",
  "Q2 (Media-Baja)" = "#0072B2",
  "Q3 (Media)"      = "#009E73",
  "Q4 (Media-Alta)" = "#E69F00",
  "Q5 (Alta)"       = "#D55E00"
)

# Tema base estandarizado (sans-serif, jerarquía visual clara)
tema_estandar <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.25), color = "#111111", margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.95), color = "#444444", margin = margin(b = 12)),
      plot.caption = element_text(size = rel(0.8), color = "#666666", hjust = 0, margin = margin(t = 10)),
      axis.title = element_text(face = "bold", size = rel(0.95), color = "#222222"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(size = rel(0.9), color = "#222222"),
      axis.text.x = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.5),
      legend.position = "none",
      plot.margin = margin(14, 16, 12, 14)
    )
}

# Cargamos la base de datos 
wdi_limpia <- read_csv("input/wdi_limpia.csv", show_col_types = FALSE)

# Calculamos la Tasa de Dependencia Mayor para el año 2019 de forma dinámica
datos_2019_demografia <- wdi_limpia %>%
  filter(anio == 2019) %>%
  mutate(dep_mayor_2019 = (poblacion_65_mas / poblacion_15_64) * 100) %>%
  select(pais, dep_mayor_2019) %>%
  drop_na(dep_mayor_2019)

# Filtramos el Gasto en Salud (% del PIB) para el año 2019
datos_salud_2019 <- wdi_limpia %>%
  filter(anio == 2019) %>%
  select(pais, gasto_salud_pib) %>%
  drop_na(gasto_salud_pib)

# Unimos ambas fuentes de datos por país (sincronizado en 2019)
df_quintiles <- datos_2019_demografia %>%
  inner_join(datos_salud_2019, by = "pais")

# 5 Quintiles
df_quintiles <- df_quintiles %>%
  mutate(
    quintil_DM = ntile(dep_mayor_2019, 5),
    quintil_DM = factor(
      quintil_DM,
      levels = 1:5,
      labels = c("Q1 (Baja)", "Q2 (Media-Baja)", "Q3 (Media)", "Q4 (Media-Alta)", "Q5 (Alta)")
    )
  )

# Tabla resumen
tabla_resumen <- df_quintiles %>%
  group_by(quintil_DM) %>%
  summarise(
    n_paises = n(),
    min_DM = min(dep_mayor_2019),
    max_DM = max(dep_mayor_2019),
    promedio_DM = mean(dep_mayor_2019),
    promedio_salud_pib = mean(gasto_salud_pib),
    sd_salud_pib = sd(gasto_salud_pib),
    mediana_salud_pib = median(gasto_salud_pib),
    .groups = "drop"
  )

print("--- TABLA DESCRIPTIVA DE QUINTILES (2019) ---")
print(tabla_resumen)

# Para identificar países excluidos
paises_totales <- unique(wdi_limpia$pais)
paises_en_analisis <- unique(df_quintiles$pais)
paises_excluidos <- setdiff(paises_totales, paises_en_analisis)

print("Países excluidos:")
print(paises_excluidos)

# Inferencia ANOVA de Un Factor####

modelo_anova <- aov(gasto_salud_pib ~ quintil_DM, data = df_quintiles)
summary_anova <- summary(modelo_anova)

print("--- RESULTADOS DEL TEST ANOVA (2019) ---")
print(summary_anova)

# Tamaño del Efecto (Eta Cuadrado - Eta^2)
ss_efecto <- summary_anova[[1]]["quintil_DM", "Sum Sq"]
ss_total <- sum(summary_anova[[1]][, "Sum Sq"])
eta_cuadrado <- ss_efecto / ss_total

cat(sprintf(
  "Tamaño del efecto (Eta^2): %.4f (%.2f%% de la varianza explicada)\n",
  eta_cuadrado, eta_cuadrado * 100
))

# Test Post-Hoc de Tukey HSD
tukey_res <- TukeyHSD(modelo_anova)
print("--- TEST POST-HOC DE TUKEY HSD (2019) ---")
print(tukey_res)

# Detección de Outliers (IQR Y boxplot)
outliers_gasto <- df_quintiles %>%
  group_by(quintil_DM) %>%
  mutate(
    Q1_val = quantile(gasto_salud_pib, 0.25),
    Q3_val = quantile(gasto_salud_pib, 0.75),
    IQR_val = IQR(gasto_salud_pib),
    limite_superior = Q3_val + 1.5 * IQR_val,
    limite_inferior = Q1_val - 1.5 * IQR_val
  ) %>%
  filter(gasto_salud_pib > limite_superior | gasto_salud_pib < limite_inferior) %>%
  select(pais, quintil_DM, gasto_salud_pib, limite_superior)

print("--- OUTLIERS DETECTADOS (2019) ---")
print(outliers_gasto)

# Visualización ####

# Gráfico de Barras con Promedios y Observaciones
promedios <- df_quintiles %>%
  group_by(quintil_DM) %>%
  summarise(
    prom_salud = mean(gasto_salud_pib),
    n = n(),
    .groups = "drop"
  )

y_max_barras <- max(c(df_quintiles$gasto_salud_pib, promedios$prom_salud), na.rm = TRUE) * 1.12

grafico_barras <- ggplot() +
  geom_col(
    data = promedios,
    aes(x = quintil_DM, y = prom_salud, fill = quintil_DM),
    width = 0.56,
    color = "#222222",
    linewidth = 0.4,
    alpha = 0.88
  ) +
  geom_jitter(
    data = df_quintiles,
    aes(x = quintil_DM, y = gasto_salud_pib),
    width = 0.14,
    alpha = 0.45,
    size = 1.7,
    color = "#1B1B1B"
  ) +
  geom_text(
    data = promedios,
    aes(x = quintil_DM, y = prom_salud, label = sprintf("%.1f%%", prom_salud)),
    vjust = -0.6,
    fontface = "bold",
    size = 3.6,
    color = "#111111"
  ) +
  scale_fill_manual(values = paleta_quintiles) +
  scale_x_discrete(labels = function(x) paste0(x, "\n(n=", promedios$n[match(x, promedios$quintil_DM)], ")")) +
  scale_y_continuous(
    limits = c(0, ceiling(y_max_barras / 2) * 2),
    breaks = seq(0, ceiling(y_max_barras / 2) * 2, by = 2),
    labels = function(y) paste0(y, "%"),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    title = "Gasto promedio en salud aumenta con la tasa de dependencia mayor",
    subtitle = paste0(
      "Promedio y distribución por quintil de dependencia mayor en 2019 (N = ",
      nrow(df_quintiles), " países; ANOVA \u03B7\u00B2 = ", sprintf("%.2f", eta_cuadrado), ")"
    ),
    x = "Quintiles de Tasa de Dependencia Mayor (2019)",
    y = "Gasto en salud (% del PIB)",
    caption = "Fuente: Elaboración propia en base a World Development Indicators (WDI, 2019). Puntos: países observados; etiquetas: promedio del quintil."
  ) +
  tema_estandar(base_size = 11)

print(grafico_barras)
guardar_grafico(grafico_barras, "quintiles_dm_gasto_salud_barras_2019.jpg", ancho = 10, alto = 6.5)

# Generación del Boxplot solo con los puntos de outliers
grafico_boxplot <- ggplot(df_quintiles, aes(x = quintil_DM, y = gasto_salud_pib, fill = quintil_DM)) +
  geom_boxplot(
    width = 0.52,
    color = "#222222",
    linewidth = 0.45,
    alpha = 0.85,
    outlier.color = "#D55E00",
    outlier.fill = "#D55E00",
    outlier.shape = 21,
    outlier.size = 2.3
  ) +
  scale_fill_manual(values = paleta_quintiles) +
  scale_y_continuous(
    labels = function(y) paste0(y, "%"),
    expand = expansion(mult = c(0.03, 0.06))
  ) +
  labs(
    title = "La dispersión del gasto en salud es mayor en los quintiles más envejecidos",
    subtitle = paste0(
      "Mediana, rango intercuartílico y valores atípicos por quintil de dependencia mayor en 2019 (N = ",
      nrow(df_quintiles), " países)"
    ),
    x = "Quintiles de Tasa de Dependencia Mayor (2019)",
    y = "Gasto en salud (% del PIB)",
    caption = "Fuente: Elaboración propia en base a World Development Indicators (WDI, 2019). Cajas: Q1–Q3 y mediana; puntos naranjas: valores atípicos (1.5 × IQR)."
  ) +
  tema_estandar(base_size = 11)

print(grafico_boxplot)
guardar_grafico(grafico_boxplot, "quintiles_dm_gasto_salud_boxplot_2019.jpg", ancho = 10, alto = 6.5)

# Tablas agrupadas por Quintil (con y sin PIB per cápita)
paises_agrupados <- df_quintiles %>%
  select(quintil_DM, pais, dep_mayor_2019, gasto_salud_pib) %>%
  arrange(quintil_DM, pais)

print(paises_agrupados)

# Extraer el PIB per cápita PPA de 2019 desde la base limpia
datos_pib_2019 <- wdi_limpia %>%
  filter(anio == 2019) %>%
  select(pais, pib_per_capita_ppa)

# Unir el PIB al dataframe de quintiles sin modificar el modelo principal
df_quintiles_con_pib <- df_quintiles %>%
  left_join(datos_pib_2019, by = "pais")

# Armar la tabla agrupada por Quintil con País, Tasa DM, Gasto en Salud y PIB per cápita
paises_agrupados_pib <- df_quintiles_con_pib %>%
  select(quintil_DM, pais, dep_mayor_2019, gasto_salud_pib, pib_per_capita_ppa) %>%
  arrange(quintil_DM, desc(dep_mayor_2019))

# Gráfico de barras (gasto en salud por quintil de dependencia mayor para 2019)

# Función estandarizada de exportación (300 dpi en output/graficos/)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

guardar_grafico <- function(plot, nombre_archivo, ancho = 10, alto = 6.5) {
  ruta <- file.path("output/graficos", nombre_archivo)
  ggsave(
    filename = ruta,
    plot = plot,
    width = ancho,
    height = alto,
    units = "in",
    dpi = 300,
    device = "jpeg",
    quality = 95
  )
  message("Gráfico guardado en: ", ruta)
}

# Colores originales del gráfico de referencia
colores_barras_quintiles <- c(
  "Q1 (Baja)"       = "#2b5c8f",
  "Q2 (Media-Baja)" = "#4682b4",
  "Q3 (Media)"      = "#6baed6",
  "Q4 (Media-Alta)" = "#d95f02",
  "Q5 (Alta)"       = "#e7298a"
)

# Resumen de promedios por quintil (requiere df_quintiles ya creado en el script)
promedios_barras <- df_quintiles %>%
  group_by(quintil_DM) %>%
  summarise(
    prom_salud = mean(gasto_salud_pib),
    .groups = "drop"
  )

y_max_barras <- max(c(df_quintiles$gasto_salud_pib, promedios_barras$prom_salud), na.rm = TRUE)
y_limite_sup <- ceiling(y_max_barras / 2) * 2

grafico_barras <- ggplot() +
  geom_col(
    data = promedios_barras,
    aes(x = quintil_DM, y = prom_salud, fill = quintil_DM),
    width = 0.55,
    color = "#111111",
    linewidth = 0.35,
    alpha = 0.9
  ) +
  geom_jitter(
    data = df_quintiles,
    aes(x = quintil_DM, y = gasto_salud_pib),
    width = 0.14,
    alpha = 0.45,
    size = 1.7,
    color = "#222222"
  ) +
  geom_text(
    data = promedios_barras,
    aes(x = quintil_DM, y = prom_salud, label = sprintf("%.2f%%", prom_salud)),
    vjust = -0.55,
    fontface = "bold",
    size = 3.5,
    color = "#111111"
  ) +
  scale_fill_manual(values = colores_barras_quintiles) +
  scale_y_continuous(
    limits = c(0, y_limite_sup),
    breaks = seq(0, y_limite_sup, by = 2),
    labels = function(y) paste0(y, "%"),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "Gasto Promedio en Salud (% del PIB) según Quintil de Dependencia Mayor (2019)",
    subtitle = paste0("Análisis comparativo global (N = ", nrow(df_quintiles), " países)"),
    x = "Quintiles de Tasa de Dependencia Mayor (2019)",
    y = "Gasto en Salud (% del PIB)",
    caption = "Fuente: Elaboración propia en base a World Development Indicators (WDI, 2019)."
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), color = "#111111", margin = margin(b = 4)),
    plot.subtitle = element_text(size = rel(0.95), color = "#444444", margin = margin(b = 12)),
    plot.caption = element_text(size = rel(0.8), color = "#666666", hjust = 0, margin = margin(t = 10)),
    axis.title = element_text(face = "bold", size = rel(0.95), color = "#222222"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(size = rel(0.9), color = "#222222"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.5),
    legend.position = "none",
    plot.margin = margin(14, 16, 12, 14)
  )

print(grafico_barras)
guardar_grafico(grafico_barras, "quintiles_dm_gasto_salud_barras_2019.jpg", ancho = 10, alto = 6.5)

# Guardar gráficos ####

# Para guardar los gráficos
guardar_grafico <- function(plot, nombre_archivo, ancho = 10, alto = 6.5) {
  dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
  ruta <- file.path("output/graficos", nombre_archivo)
  ggsave(
    filename = ruta,
    plot = plot,
    width = ancho,
    height = alto,
    units = "in",
    dpi = 300,
    device = "jpeg",
    quality = 95
  )
  message("Gráfico guardado en: ", ruta)
}

guardar_grafico(grafico_barras, "quintiles_dm_gasto_salud_barras_2019.jpg", ancho = 10, alto = 6.5)
guardar_grafico(grafico_boxplot, "quintiles_dm_gasto_salud_boxplot_2019.jpg", ancho = 10, alto = 6.5)
