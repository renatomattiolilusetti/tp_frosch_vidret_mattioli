# ==============================================================================
# Función: etiquetar_variables
# ==============================================================================
# Recibe nombres de variables y un vector de etiquetas (nombre = etiqueta).
# Devuelve la etiqueta de cada variable, o su nombre sin guiones bajos si no
# tiene etiqueta, cortada en renglones para que entre en el eje del gráfico.
# ==============================================================================

etiquetar_variables <- function(x, etiquetas, ancho = 24) {
  
  resultado <- unname(etiquetas[x])
  
  # Para las variables sin etiqueta, usar su nombre sin guiones bajos
  faltan <- is.na(resultado)
  resultado[faltan] <- gsub("_", " ", x[faltan])
  
  stringr::str_wrap(resultado, width = ancho)
}
