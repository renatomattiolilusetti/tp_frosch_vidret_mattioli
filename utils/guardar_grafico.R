# ==============================================================================
# Función: guardar_grafico
# ==============================================================================
# Guarda un gráfico en output/graficos/ como JPG a 300 dpi.
# ==============================================================================

guardar_grafico <- function(plot, nombre_archivo, ancho = 10, alto = 6.5) {
  
  dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
  
  ruta <- file.path("output/graficos", nombre_archivo)
  
  ggplot2::ggsave(
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
