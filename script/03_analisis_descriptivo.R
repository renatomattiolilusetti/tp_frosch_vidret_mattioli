# ==============================================================================
# Análisis descriptivo de la base WDI
# ==============================================================================

library(tidyverse)

# Funciones propias
source("utils/etiquetar_variables.R")

wdi_limpia <- read_csv(
  "input/wdi_limpia.csv",
  show_col_types = FALSE
)

# Indicadores principales: utilizados en métodos estadísticos y gráficos.
# Las variables de pirámides se analizan por separado.
variables_principales <- c(
  "poblacion_total",
  "crecimiento_poblacional",
  "natalidad",
  "mortalidad",
  "migracion_neta",
  "fecundidad",
  "esperanza_vida",
  "poblacion_15_64",
  "poblacion_65_mas",
  "pib_per_capita_ppa",
  "gasto_salud_pib",
  "gasto_publico_salud_pib",
  "gasto_bolsillo_salud",
  "indice_cobertura_salud"
)

variables_piramides <- names(wdi_limpia)[
  startsWith(names(wdi_limpia), "mujeres_") |
    startsWith(names(wdi_limpia), "varones_")
]

columnas_identificacion <- c(
  "pais", "codigo_pais", "anio", "region", "ingreso", "poblacion_mill"
)

# Confirmar que el recorte coincida con la descarga de 50 indicadores.
stopifnot(
  length(variables_principales) == 14,
  length(variables_piramides) == 36,
  all(c(variables_principales, columnas_identificacion) %in% names(wdi_limpia)),
  setequal(
    names(wdi_limpia),
    c(variables_principales, variables_piramides, columnas_identificacion)
  ),
  !anyNA(wdi_limpia$codigo_pais),
  !anyNA(wdi_limpia$anio),
  !anyDuplicated(wdi_limpia[c("codigo_pais", "anio")])
)

resumen_base <- wdi_limpia |>
  summarise(
    observaciones = n(),
    columnas = ncol(wdi_limpia),
    paises = n_distinct(codigo_pais),
    primer_anio = min(anio),
    ultimo_anio = max(anio),
    cantidad_anios = n_distinct(anio)
  )

print(resumen_base)
cat("Indicadores principales:", length(variables_principales), "\n")
cat("Indicadores para pirámides:", length(variables_piramides), "\n")

# Análisis NA ####
# Cada fila de la base es una combinación país-año.
# Los NA son datos faltantes; no deben interpretarse como ceros.
faltantes_variables <- tibble(
  variable = variables_principales,
  cantidad_na = colSums(is.na(wdi_limpia[variables_principales])),
  total_observaciones = nrow(wdi_limpia)
) |>
  mutate(
    porcentaje_na = round(100 * cantidad_na / total_observaciones, 2)
  ) |>
  arrange(desc(porcentaje_na), variable)

print(faltantes_variables, n = Inf)

# Resumen por región: cada país-año pesa lo mismo, no su población.
tabla_na_region <- wdi_limpia |>
  group_by(region) |>
  summarise(
    across(
      all_of(variables_principales),
      ~ 100 * mean(is.na(.))
    ),
    .groups = "drop"
  )

# Excluir del mapa las variables que tengan menos de 5% de NA en TODAS las 
# regiones. Es suficiente alcanzar 5% en UNA región para entrar.
variables_mapa <- tabla_na_region |>
  select(-region) |>
  select(where(~ any(. >= 5))) |>
  names()

cat("Variables incluidas en el mapa de calor:", length(variables_mapa), "\n")
print(variables_mapa)

# El último período (2020-2025) tiene seis años, no diez.
na_decadas <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  mutate(
    decada = cut(
      anio,
      breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2026),
      labels = c(
        "1960–1969", "1970–1979", "1980–1989", "1990–1999",
        "2000–2009", "2010–2019", "2020–2025"
      ),
      right = FALSE
    )
  ) |>
  group_by(region, decada) |>
  summarise(
    across(
      all_of(variables_mapa),
      ~ 100 * mean(is.na(.))
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = all_of(variables_mapa),
    names_to = "variable",
    values_to = "porcentaje_na"
  )

etiquetas_variables <- c(
  poblacion_total = "Población total",
  crecimiento_poblacional = "Crecimiento poblacional",
  natalidad = "Natalidad",
  mortalidad = "Mortalidad",
  migracion_neta = "Migración neta",
  fecundidad = "Fecundidad",
  esperanza_vida = "Esperanza de vida",
  poblacion_15_64 = "Población 15 a 64 años",
  poblacion_65_mas = "Población de 65 años o más",
  pib_per_capita_ppa = "PIB per cápita PPA",
  gasto_salud_pib = "Gasto en salud",
  gasto_publico_salud_pib = "Gasto público en salud",
  gasto_bolsillo_salud = "Gasto de bolsillo en salud",
  indice_cobertura_salud = "Cobertura de servicios de salud"
)

stopifnot(all(variables_principales %in% names(etiquetas_variables)))

# La figura usa una escala común (0-100%) para comparar regiones y décadas.
# El porcentaje es de observaciones país-año sin datos, no de población.
grafico_mapas_na <- ggplot(
  na_decadas,
  aes(x = decada, y = variable, fill = porcentaje_na)
) +
  geom_tile(
    width = 0.96,
    height = 0.90,
    color = "white",
    linewidth = 0.2
  ) +
  facet_wrap(
    ~ region,
    ncol = 4,
    labeller = label_wrap_gen(width = 22)
  ) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    name = "Datos faltantes (%)"
  ) +
  scale_x_discrete(
    labels = function(x) substr(x, 1, 4),
    drop = FALSE,
    expand = expansion(add = 0.05)
  ) +
  scale_y_discrete(
    limits = rev(variables_mapa),
    labels = \(x) etiquetar_variables(x, etiquetas_variables),
    expand = expansion(add = 0.05)
  ) +
  labs(
    title = "Datos faltantes en indicadores económicos y sanitarios por región y década",
    subtitle = "Indicadores con al menos 5% de faltantes en alguna región",
    x = "Inicio del período",
    y = NULL,
    caption = paste(
      "Fuente: Banco Mundial (WDI). Elaboración propia.",
      "Cada país-año pesa lo mismo; 2020-2025 abarca seis años."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    plot.caption = element_text(size = 9, hjust = 0),
    panel.spacing = grid::unit(0.5, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      barwidth = grid::unit(7, "cm"),
      barheight = grid::unit(0.3, "cm")
    )
  )

print(grafico_mapas_na)

# Diccionario de variables ####

# Incluye todas las columnas de wdi_limpia
diccionario <- tribble(
  ~variable, ~descripcion, ~tipo, ~unidad, ~rango_teorico,
  # Identificación y clasificaciones
  "pais",
  "Nombre del país",
  "Categórica", "-", "-",
  "codigo_pais",
  "Código de tres letras utilizado para identificar al país",
  "Categórica", "-", "-",
  "anio",
  "Año al que corresponde la observación",
  "Numérica discreta", "Año calendario", "1960 a 2025",
  "region",
  "Región del Banco Mundial asociada al país en la descarga; no representa una clasificación histórica anual",
  "Categórica", "-", "7 categorías",
  "ingreso",
  "Grupo de ingreso del Banco Mundial en la descarga; no necesariamente corresponde al grupo de cada año histórico",
  "Ordinal", "-", "4 grupos y Sin clasificar",
  
  # Demografía y estructura por edad
  "poblacion_total",
  "Población total del país",
  "Numérica", "Personas", "> 0",
  "poblacion_mill",
  "Población total del país dividida por un millón",
  "Numérica", "Millones de personas", "> 0",
  "crecimiento_poblacional",
  "Tasa de crecimiento exponencial anual de la población del país",
  "Numérica", "% anual", "-Inf a Inf",
  "natalidad",
  "Nacimientos vivos durante el año respecto de la población a mitad de año",
  "Numérica", "Nacimientos por cada 1.000 habitantes", ">= 0",
  "mortalidad",
  "Defunciones durante el año respecto de la población a mitad de año",
  "Numérica", "Defunciones por cada 1.000 habitantes", ">= 0",
  "migracion_neta",
  "Cantidad de inmigrantes menos emigrantes durante el año",
  "Numérica", "Personas", "-Inf a Inf",
  "fecundidad",
  "Número de hijos que tendría una mujer bajo las tasas de fecundidad por edad del período",
  "Numérica", "Hijos por mujer", ">= 0",
  "esperanza_vida",
  "Años que viviría un recién nacido bajo las tasas de mortalidad por edad del período",
  "Numérica", "Años", ">= 0",
  "poblacion_15_64",
  "Personas de 15 a 64 años como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100",
  "poblacion_65_mas",
  "Personas de 65 años o más como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100",
  
  # Condiciones económicas
  "pib_per_capita_ppa",
  "PIB por habitante, ajustado por paridad de poder adquisitivo",
  "Numérica", "Dólares internacionales constantes de 2021 por habitante", "> 0",
  
  # Salud
  "gasto_salud_pib",
  "Gasto corriente en salud, público y privado, en relación con el PIB",
  "Numérica", "% del PIB", ">= 0",
  "gasto_publico_salud_pib",
  "Gasto corriente del gobierno general en salud financiado con fuentes internas, en relación con el PIB",
  "Numérica", "% del PIB", ">= 0",
  "gasto_bolsillo_salud",
  "Pagos directos de los hogares por salud respecto del gasto corriente total en salud",
  "Numérica", "% del gasto corriente en salud", "0 a 100",
  "indice_cobertura_salud",
  "Índice de cobertura de servicios esenciales de salud (UHC)",
  "Numérica", "Índice de 0 a 100", "0 a 100",
  
  # Pirámides poblacionales: mujeres
  "mujeres_0_4_pct",
  "Mujeres de 0 a 4 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_5_9_pct",
  "Mujeres de 5 a 9 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_10_14_pct",
  "Mujeres de 10 a 14 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_15_19_pct",
  "Mujeres de 15 a 19 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_20_24_pct",
  "Mujeres de 20 a 24 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_25_29_pct",
  "Mujeres de 25 a 29 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_30_34_pct",
  "Mujeres de 30 a 34 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_35_39_pct",
  "Mujeres de 35 a 39 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_40_44_pct",
  "Mujeres de 40 a 44 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_45_49_pct",
  "Mujeres de 45 a 49 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_50_54_pct",
  "Mujeres de 50 a 54 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_55_59_pct",
  "Mujeres de 55 a 59 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_60_64_pct",
  "Mujeres de 60 a 64 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_65_69_pct",
  "Mujeres de 65 a 69 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_70_74_pct",
  "Mujeres de 70 a 74 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_75_79_pct",
  "Mujeres de 75 a 79 años",
  "Numérica", "% de la población femenina", "0 a 100",
  "mujeres_80_mas_pct",
  "Mujeres de 80 años o más",
  "Numérica", "% de la población femenina", "0 a 100",
  
  # Pirámides poblacionales: varones
  "varones_0_4_pct",
  "Varones de 0 a 4 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_5_9_pct",
  "Varones de 5 a 9 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_10_14_pct",
  "Varones de 10 a 14 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_15_19_pct",
  "Varones de 15 a 19 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_20_24_pct",
  "Varones de 20 a 24 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_25_29_pct",
  "Varones de 25 a 29 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_30_34_pct",
  "Varones de 30 a 34 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_35_39_pct",
  "Varones de 35 a 39 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_40_44_pct",
  "Varones de 40 a 44 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_45_49_pct",
  "Varones de 45 a 49 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_50_54_pct",
  "Varones de 50 a 54 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_55_59_pct",
  "Varones de 55 a 59 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_60_64_pct",
  "Varones de 60 a 64 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_65_69_pct",
  "Varones de 65 a 69 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_70_74_pct",
  "Varones de 70 a 74 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_75_79_pct",
  "Varones de 75 a 79 años",
  "Numérica", "% de la población masculina", "0 a 100",
  "varones_80_mas_pct",
  "Varones de 80 años o más",
  "Numérica", "% de la población masculina", "0 a 100",
  
  # Composición por sexo
  "mujeres_total_pct",
  "Mujeres como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100",
  "varones_total_pct",
  "Varones como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100"
  
)
# Confirmar que el diccionario coincida exactamente con la base limpia.
stopifnot(
  !anyDuplicated(diccionario$variable),
  setequal(diccionario$variable, names(wdi_limpia))
)

# Exportar resultados
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

write_csv(diccionario, "output/tablas/diccionario_variables.csv")
write_csv(faltantes_variables, "output/tablas/faltantes_variables.csv")
write_csv(
  tabla_na_region |>
    mutate(across(all_of(variables_principales), ~ round(., 2))),
  "output/tablas/faltantes_por_region.csv"
)
write_csv(
  na_decadas |>
    mutate(porcentaje_na = round(porcentaje_na, 2)),
  "output/tablas/faltantes_region_decada.csv"
)

ggsave(
  filename = "output/graficos/mapas_na_todas_las_regiones.png",
  plot = grafico_mapas_na,
  width = 16,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat("Diccionario validado:", nrow(diccionario), "variables.\n")
cat("Resultados guardados en output/tablas y output/graficos.\n")
