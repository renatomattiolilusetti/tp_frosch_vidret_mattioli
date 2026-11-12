# ==============================================================================
# TP CIENCIA DE DATOS - FCE UBA | TRANSICION DEMOGRAFICA Y SILVER ECONOMY
# MAPAS COMUNICATIVOS: 1980 vs. 2024 | COMPOSICION AUTOMATICA
# ==============================================================================
# Este script crea los mapas con R + ggplot2 y exporta tres archivos JPG:
#   output/graficos/grupo_10_comunicativo.jpg  <- enviar como comunicativo
#   output/graficos/mapa_perfiles_1980.jpg     <- para presentar por separado
#   output/graficos/mapa_perfiles_2024.jpg     <- para presentar por separado
#
# IMPORTANTE: los dos mapas juntos constituyen UN grafico comunicativo.
# La instancia 3 tambien solicita UN grafico EXPLORATORIO, distinto de este.
#
# Metodologia conservada del script anterior:
# PCA (2 componentes) + HCPC (2 grupos) para 1980 y 2024 POR SEPARADO.
# El grupo 'transicion avanzada' es aquel cuyo promedio de poblacion de
# 65 anos o mas es mayor en ese anio. No es un umbral fijo de envejecimiento.
# El universo de paises es IDENTICO en los dos anos, con datos completos.
#
# Guardar este archivo en script/ y ejecutarlo completo desde el proyecto.
# Codificacion del archivo: UTF-8 (acepta tildes y eñes en los graficos).
# ==============================================================================

# 1. PAQUETES ---------------------------------------------------------------
# La instalacion ocurre solo cuando hace falta (la carga ocurre en cada sesion).
paquetes <- c("tidyverse", "FactoMineR", "sf", "rnaturalearth",
              "rnaturalearthdata", "ggtext", "patchwork")

faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) install.packages(faltan)

library(tidyverse)
library(FactoMineR)
library(sf)
library(rnaturalearth)
library(ggtext)
library(patchwork)

# 2. LEER LA BASE LIMPIA SIN MODIFICARLA ------------------------------------
# Usa un CSV del proyecto para que se pueda correr en una sesion limpia.
rutas_posibles <- c(
  "input/wdi_limpia.csv",
  "repo_wdi/wdi_limpia.csv",
  "wdi_limpia.csv"
)
rutas_existentes <- rutas_posibles[file.exists(rutas_posibles)]

if (length(rutas_existentes) == 0) {
  stop(paste0(
    "No encuentro wdi_limpia.csv. Abrí el proyecto del TP en RStudio ",
    "y verificá que el CSV esté en input/, repo_wdi/ o en la raíz."
  ))
}

wdi_limpia <- readr::read_csv(rutas_existentes[1], show_col_types = FALSE)

variables <- c("poblacion_65_mas", "fecundidad",
               "esperanza_vida", "crecimiento_poblacional")
columnas_necesarias <- c("pais", "codigo_pais", "anio", variables)

if (!all(columnas_necesarias %in% names(wdi_limpia))) {
  stop(paste("Faltan columnas en wdi_limpia:",
             paste(setdiff(columnas_necesarias, names(wdi_limpia)),
                   collapse = ", ")))
}

# 3. DEFINIR EL MISMO UNIVERSO PARA 1980 Y 2024 ------------------------------
# Se conserva la exclusion de PSE utilizada en el script anterior.
base_mapas <- wdi_limpia |>
  filter(anio %in% c(1980, 2024),
         !is.na(codigo_pais), codigo_pais != "PSE") |>
  select(all_of(columnas_necesarias))

# La unidad de observacion debe ser pais-ano.
if (anyDuplicated(base_mapas[c("codigo_pais", "anio")]) > 0) {
  stop("Se encontraron filas duplicadas para un mismo país y año.")
}

# No se reemplazan NA por cero ni se imputan valores faltantes.
base_mapas <- base_mapas |>
  filter(if_all(all_of(variables), ~ !is.na(.x) & is.finite(.x)))

paises_comunes <- base_mapas |>
  count(codigo_pais, name = "n_anios") |>
  filter(n_anios == 2) |>
  pull(codigo_pais)

base_mapas <- base_mapas |>
  filter(codigo_pais %in% paises_comunes)

n_comparables <- length(paises_comunes)
if (n_comparables < 3) {
  stop("Hay menos de tres países con datos completos en ambos años.")
}

message("Países comparables para ambos años: ", n_comparables)

# 4. PCA + HCPC (MISMA LOGICA DEL ANALISIS ORIGINAL) -------------------------
clasificar_anio <- function(anio_elegido) {

  datos <- base_mapas |>
    filter(anio == anio_elegido)

  matriz <- datos |>
    select(all_of(variables)) |>
    as.data.frame()
  rownames(matriz) <- datos$codigo_pais

  pca <- FactoMineR::PCA(
    matriz, scale.unit = TRUE, ncp = 2, graph = FALSE
  )

  set.seed(2026)
  hcpc <- FactoMineR::HCPC(
    pca, nb.clust = 2, consol = TRUE, graph = FALSE
  )

  grupos <- hcpc$data.clust |>
    as.data.frame() |>
    tibble::rownames_to_column("codigo_pais") |>
    transmute(codigo_pais, cluster = as.character(clust))

  stopifnot(anyDuplicated(grupos$codigo_pais) == 0)

  clasificados <- datos |>
    left_join(grupos, by = "codigo_pais", relationship = "one-to-one")

  if (any(is.na(clasificados$cluster))) {
    stop(paste("No se asignaron todos los grupos en", anio_elegido))
  }

  # Los numeros 1 y 2 del clustering NO tienen significado demografico.
  # Identificamos el grupo mas avanzado por su promedio de poblacion 65+.
  grupo_avanzado <- clasificados |>
    group_by(cluster) |>
    summarise(media_65 = mean(poblacion_65_mas), .groups = "drop") |>
    slice_max(media_65, n = 1, with_ties = FALSE) |>
    pull(cluster)

  clasificados |>
    mutate(perfil = if_else(cluster == grupo_avanzado,
                            "Transición avanzada", "Perfil demográfico joven")) |>
    select(anio, pais, codigo_pais, perfil)
}

perfiles <- bind_rows(
  clasificar_anio(1980),
  clasificar_anio(2024)
)

stopifnot(anyDuplicated(perfiles[c("codigo_pais", "anio")]) == 0)
stopifnot(nrow(perfiles) == 2 * n_comparables)

resumen <- perfiles |>
  count(anio, perfil, name = "paises") |>
  arrange(anio, perfil)

print(resumen)

n_1980 <- sum(perfiles$anio == 1980 &
                perfiles$perfil == "Transición avanzada")
n_2024 <- sum(perfiles$anio == 2024 &
                perfiles$perfil == "Transición avanzada")
cambio_neto <- n_2024 - n_1980

# 5. CARTOGRAFIA Y UNION CONTROLADA ------------------------------------------
# Natural Earth: la escala 50 requiere a veces una descarga inicial; 110
# funciona como respaldo. No cambia la clasificacion de los paises.
mundo <- tryCatch(
  rnaturalearth::ne_countries(scale = 50, returnclass = "sf"),
  error = function(e) {
    message("Se utilizará la cartografía Natural Earth escala 110.")
    rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
  }
)

mundo <- mundo |>
  filter(admin != "Antarctica") |>
  mutate(
    codigo_pais = coalesce(na_if(iso_a3, "-99"), adm0_a3),
    codigo_pais = if_else(codigo_pais == "KOS", "XKX", codigo_pais)
  ) |>
  select(codigo_pais, geometry)

sin_geometria <- perfiles |>
  distinct(codigo_pais, pais) |>
  anti_join(mundo |>
              st_drop_geometry() |>
              distinct(codigo_pais), by = "codigo_pais")

if (nrow(sin_geometria) > 0) {
  message("Países sin polígono en Natural Earth: ",
          paste(sin_geometria$pais, collapse = ", "))
}

mapa_1980 <- mundo |>
  left_join(perfiles |>
              filter(anio == 1980) |>
              select(codigo_pais, perfil),
            by = "codigo_pais", relationship = "many-to-one") |>
  mutate(anio = 1980)

mapa_2024 <- mundo |>
  left_join(perfiles |>
              filter(anio == 2024) |>
              select(codigo_pais, perfil),
            by = "codigo_pais", relationship = "many-to-one") |>
  mutate(anio = 2024)

# 6. PALETA Y TEXTOS --------------------------------------------------------
# Azul y naranja: paleta Okabe-Ito, de alto contraste.
paleta <- c(
  "Perfil demográfico joven" = "#0072B2",
  "Transición avanzada"     = "#E69F00",
  "Sin clasificación"       = "#D9D2E5"
)

mapas <- bind_rows(mapa_1980, mapa_2024) |>
  mutate(
    perfil_mapa = if_else(is.na(perfil), "Sin clasificación",
                           as.character(perfil)),
    perfil_mapa = factor(perfil_mapa, levels = names(paleta))
  )

# Chequeo: los nombres de la clasificación deben coincidir con la paleta.
stopifnot(!any(is.na(mapas$perfil_mapa)))

if (cambio_neto > 0) {
  mensaje_cambio <- paste0(cambio_neto, " países más")
} else if (cambio_neto < 0) {
  mensaje_cambio <- paste0(abs(cambio_neto), " países menos")
} else {
  mensaje_cambio <- "la misma cantidad de países"
}

# Un único bloque de título: nunca posicionar sus partes a mano.
titulo_principal <- paste0(
  "El perfil demográfico avanzado reúne<br>",
  "<span style='color:#A66A00'><b>", mensaje_cambio,
  "</b></span> que en 1980"
)

# Subtítulo pedido, con cortes de línea deliberados.
subtitulo_principal <- paste0(
  "En 2024 hay más países con baja fecundidad, mayor esperanza de vida\n",
  "y mayor peso de la población de 65+ que en 1980.\n",
  "Se comparan ", n_comparables,
  " países con datos completos en 1980 y 2024."
)

nota_fuente <- paste(
  "Fuente: Banco Mundial (WDI). Cartografía: Natural Earth. Elaboración propia.",
  "Método: PCA y HCPC; población 65+, fecundidad, esperanza de vida y crecimiento.",
  "Nota: perfiles relativos estimados por año, sin umbral fijo; lavanda: sin clasificación.",
  sep = "\n"
)

color_titulo <- "#18344D"
color_texto  <- "#36536B"
color_nota   <- "#52677A"

# 7. CREAR CADA MAPA CON SU ENCABEZADO --------------------------------------
# Son dos ggplots normales: el ancho del mapa y el del encabezado se adaptan
# al ancho del gráfico exportado. No se usan coordenadas x/y para los textos.
crear_mapa <- function(anio_elegido, cantidad_avanzados) {

  datos <- mapas |>
    filter(anio == anio_elegido)

  ggplot(datos) +
    geom_sf(
      aes(fill = perfil_mapa),
      color = "white", linewidth = 0.075
    ) +
    scale_fill_manual(
      name = NULL,
      values = paleta,
      breaks = names(paleta),
      limits = names(paleta),
      drop = FALSE,
      guide = guide_legend(nrow = 1)
    ) +
    coord_sf(crs = "+proj=robin", datum = NA, expand = FALSE) +
    labs(
      title = paste0(anio_elegido, "  ·  ", cantidad_avanzados,
                     " países con perfil avanzado")
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#F2F8FD", color = NA),
      plot.title.position = "plot",
      plot.title = element_text(
        face = "bold", size = 14.5, color = color_titulo,
        hjust = 0.5, margin = margin(b = 7)
      ),
      plot.margin = margin(t = 5, r = 0, b = 5, l = 0),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 11.2, color = color_titulo),
      legend.key.width = grid::unit(0.75, "cm"),
      legend.key.height = grid::unit(0.36, "cm")
    )
}

mapa_grafico_1980 <- crear_mapa(1980, n_1980)
mapa_grafico_2024 <- crear_mapa(2024, n_2024)

# 8. COMBINAR AUTOMÁTICAMENTE CON PATCHWORK ---------------------------------
# Alineacion hjust = 0.5: titulo, subtitulo, encabezados y notas centrados.
# No modifica el tamano de los mapas ni el calculo de los perfiles.
# patchwork alinea ambos mapas y organiza título, leyenda y fuente sin usar
# posiciones fijas. Es un paquete complementario visto en la materia.
comparacion <- (mapa_grafico_1980 / mapa_grafico_2024) +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(1, 1),
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    title = titulo_principal,
    subtitle = subtitulo_principal,
    caption = nota_fuente,
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggtext::element_markdown(
        face = "bold", size = 18.5, color = color_titulo,
        hjust = 0.5, halign = 0.5, lineheight = 1.14, margin = margin(b = 9)
      ),
      # Subtítulo más grande, con las tres líneas centradas.
      plot.subtitle = element_text(
        size = 13.2, color = color_texto, hjust = 0.5,
        lineheight = 1.22, margin = margin(b = 19)
      ),
      # Fuente y nota más legibles en el JPG a 300 dpi.
      plot.caption = element_text(
        size = 10.5, color = color_nota, hjust = 0.5,
        lineheight = 1.22, margin = margin(t = 14)
      ),
      plot.margin = margin(t = 14, r = 18, b = 14, l = 18)
    )
  ) &
  theme(legend.position = "bottom")

# 9. EXPORTAR LOS TRES JPG --------------------------------------------------
# La imagen COMUNICATIVA es VERTICAL: dos mapas superpuestos con buen tamaño.
# El zoom horizontal de RStudio no reproduce esta proporción. El archivo JPG sí.
carpeta_salida <- file.path("output", "graficos")
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

archivo_comparacion <- file.path(carpeta_salida, "grupo_10_comunicativo.jpg")

ggsave(
  filename = archivo_comparacion,
  plot = comparacion,
  # Un poco más de alto para dar aire al texto, sin achicar los mapas.
  width = 24.5, height = 27.0, units = "cm", dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(carpeta_salida, "mapa_perfiles_1980.jpg"),
  plot = mapa_grafico_1980,
  width = 24.5, height = 15.5, units = "cm", dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(carpeta_salida, "mapa_perfiles_2024.jpg"),
  plot = mapa_grafico_2024,
  width = 24.5, height = 15.5, units = "cm", dpi = 300,
  bg = "white"
)

message("Gráfico comunicativo guardado en: ", normalizePath(archivo_comparacion))
message("Perfil avanzado: ", n_1980, " países en 1980 y ", n_2024,
        " en 2024. Diferencia neta: ", cambio_neto, ".")

# En Mac: abrir automáticamente el JPG exportado, que es la versión para la
# entrega. Evitamos el botón 'Zoom' de RStudio, cuya ventana es horizontal.
if (interactive() && Sys.info()[["sysname"]] == "Darwin") {
  system2("open", shQuote(normalizePath(archivo_comparacion)), wait = FALSE)
}
