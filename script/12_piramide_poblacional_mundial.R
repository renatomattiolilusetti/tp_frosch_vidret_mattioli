# ==============================================================================
# Pirámide poblacional mundial: evolución 1960-2024 (animación)
# ==============================================================================

# Paquetes
library(tidyverse)
library(scales)
library(gifski)

# Crear las carpetas de resultados si no existen
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

# Leer la base limpia
wdi_limpia <- read_csv("input/wdi_limpia.csv", show_col_types = FALSE)

# 1. Preparación de los datos ####

# Años de la animación
anios_gif <- 1960:2024

# Grupos quinquenales, de menor a mayor edad
grupos_edad <- c(
  "0_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39",
  "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_mas"
)
etiquetas_edad <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"
)
grupos_mayores <- c("65-69", "70-74", "75-79", "80+")

vars_piramide <- c(
  paste0("mujeres_", grupos_edad, "_pct"),
  paste0("varones_", grupos_edad, "_pct")
)

stopifnot(
  all(c(vars_piramide, "poblacion_total", "mujeres_total_pct", "varones_total_pct")
      %in% names(wdi_limpia))
)

# Los grupos de edad vienen como % de la población de cada sexo. Para poder sumar
# países, primero pasamos esos porcentajes a cantidad de personas
piramides_paises <- wdi_limpia |>
  filter(anio %in% anios_gif) |>
  select(
    pais, codigo_pais, anio, poblacion_total,
    mujeres_total_pct, varones_total_pct, all_of(vars_piramide)
  ) |>
  # Solo países con la pirámide completa en ese año
  filter(
    if_all(
      c(poblacion_total, mujeres_total_pct, varones_total_pct, all_of(vars_piramide)),
      ~ !is.na(.x)
    )
  ) |>
  pivot_longer(
    all_of(vars_piramide),
    names_to = c("sexo", "grupo"),
    names_pattern = "^(mujeres|varones)_(.*)_pct$",
    values_to = "pct_sexo"
  ) |>
  mutate(
    poblacion_sexo = if_else(
      sexo == "mujeres",
      poblacion_total * mujeres_total_pct / 100,
      poblacion_total * varones_total_pct / 100
    ),
    personas = poblacion_sexo * pct_sexo / 100,
    grupo = factor(grupo, levels = grupos_edad, labels = etiquetas_edad),
    sexo = if_else(sexo == "mujeres", "Mujeres", "Varones"),
    edad = if_else(grupo %in% grupos_mayores, "65 años o más", "Menos de 65 años")
  )

# Control: en cada país-año, la suma de los grupos tiene que dar la población total
control_suma <- piramides_paises |>
  summarise(
    proporcion = sum(personas) / first(poblacion_total),
    .by = c(codigo_pais, anio)
  )

stopifnot(all(abs(control_suma$proporcion - 1) < 0.02))

# Usamos los mismos países en todos los años, así los cambios de la pirámide reflejan 
# cambios demográficos y no la entrada o salida de países
paises_completos <- piramides_paises |>
  distinct(codigo_pais, anio) |>
  count(codigo_pais) |>
  filter(n == length(anios_gif)) |>
  pull(codigo_pais)

# Países que quedan afuera por no tener datos en todo el período
piramides_paises |>
  filter(!codigo_pais %in% paises_completos) |>
  distinct(codigo_pais, pais) |>
  print()

piramides_paises <- piramides_paises |>
  filter(codigo_pais %in% paises_completos)

n_paises <- length(paises_completos)

cat("Países con la pirámide completa en todos los años:", n_paises, "\n")

# 2. Pirámide mundial ####

# Sumamos las personas de todos los países y calculamos el peso de cada grupo
# sobre la población mundial de cada año (equivale a ponderar por población)
piramide_mundo <- piramides_paises |>
  summarise(
    personas = sum(personas),
    .by = c(anio, sexo, grupo, edad)
  ) |>
  mutate(pct = 100 * personas / sum(personas), .by = anio) |>
  # Varones a la izquierda (valores negativos), mujeres a la derecha
  mutate(valor = if_else(sexo == "Varones", -pct, pct))

stopifnot(setequal(unique(piramide_mundo$anio), anios_gif))

# Cuánto pesa la población de 65+ en cada año
resumen_mundo <- piramide_mundo |>
  summarise(
    pct_65 = sum(pct[edad == "65 años o más"]),
    .by = anio
  )

print(resumen_mundo |> filter(anio %in% c(1960, 1980, 2000, 2024)))

write_csv(piramide_mundo, "output/tablas/piramide_mundial.csv")

# 3. Un gráfico por año ####

# Estilo
colores_edad <- c("Menos de 65 años" = "#B9C6D0", "65 años o más" = "#E69F00")
azul_titulo <- "#19354D"
azul_texto  <- "#38536B"
azul_nota   <- "#4F6478"
gris_ejes   <- "#677887"

# Mismo eje en todos los años, para que la pirámide no "salte" entre cuadros
lim_mundo <- 2 * ceiling(max(abs(piramide_mundo$valor)) / 2)

contorno_1960 <- piramide_mundo |>
  filter(anio == 1960)

graficos_mundo <- list()

for (anio_elegido in anios_gif) {
  
  datos_anio <- piramide_mundo |>
    filter(anio == anio_elegido)
  
  resumen_anio <- resumen_mundo |>
    filter(anio == anio_elegido)
  
  # Desde 1961 dibujamos el contorno de 1960 para comparar
  capa_contorno <- if (anio_elegido != 1960) {
    geom_col(
      data = contorno_1960, aes(x = valor, y = grupo),
      inherit.aes = FALSE, fill = NA,
      color = azul_titulo, linewidth = 0.35, width = 0.9
    )
  } else {
    NULL
  }
  
  # La nota tiene siempre dos renglones, así el gráfico ocupa el mismo lugar en todos los años
  nota_contorno <- if (anio_elegido != 1960) "Contorno: pirámide de 1960." else " "
  
  graficos_mundo[[as.character(anio_elegido)]] <- ggplot(
    datos_anio, aes(x = valor, y = grupo, fill = edad)
  ) +
    geom_col(width = 0.9) +
    capa_contorno +
    geom_vline(xintercept = 0, color = "white", linewidth = 0.8) +
    scale_fill_manual(
      values = colores_edad,
      breaks = c("Menos de 65 años", "65 años o más"),
      name = NULL
    ) +
    scale_x_continuous(
      limits = c(-lim_mundo, lim_mundo),
      breaks = seq(-lim_mundo, lim_mundo, by = 2),
      labels = \(x) paste0(abs(x), "%")
    ) +
    labs(
      title = paste0("Población mundial por edad y sexo, ", anio_elegido),
      subtitle = paste0(
        format(round(resumen_anio$pct_65, 1), decimal.mark = ",", nsmall = 1),
        "% de la población mundial tenía 65 años o más"
      ),
      x = "% de la población mundial  ·  varones a la izquierda, mujeres a la derecha",
      y = "Edad (años)",
      caption = paste0(
        "Fuente: Banco Mundial (WDI). Elaboración propia. Agregado de ",
        n_paises, " países con datos en todo el período 1960-2024.\n",
        nota_contorno
      )
    ) +
    theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = 17, color = azul_titulo, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 12, color = azul_texto, margin = margin(b = 12)),
      plot.caption = element_text(size = 9.5, color = azul_nota, hjust = 0, lineheight = 1.2, margin = margin(t = 10)),
      axis.title = element_text(size = 10.5, color = azul_texto),
      axis.text = element_text(size = 9.5, color = gris_ejes),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E5EBEF", linewidth = 0.3),
      legend.position = "bottom",
      legend.text = element_text(size = 10.5, color = azul_texto),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(16, 18, 14, 16)
    )
}

print(graficos_mundo[["2024"]])

# 4. Animación ####

# Un cuadro por año. Repetimos el primer y el último año para que la animación
# haga una pausa al empezar y al terminar
cuadros_inicio <- 8
cuadros_final <- 20

gifski::save_gif(
  {
    for (i in seq_len(cuadros_inicio)) print(graficos_mundo[["1960"]])
    for (grafico in graficos_mundo) print(grafico)
    for (i in seq_len(cuadros_final)) print(graficos_mundo[["2024"]])
  },
  gif_file = "output/graficos/piramide_mundial_evolucion.gif",
  width = 1440,
  height = 1020,
  res = 150,
  delay = 0.15
)

# 5. Control final de archivos generados ####

archivos_generados <- c(
  "output/graficos/piramide_mundial_evolucion.gif",
  "output/tablas/piramide_mundial.csv"
)

stopifnot(all(file.exists(archivos_generados)))

print("12 finalizado: todos los archivos fueron generados.")