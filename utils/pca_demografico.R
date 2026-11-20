# ==============================================================================
# Función: PCA demográfico
# ==============================================================================
# Recibe una base de países y un vector de variables demográficas.
# Estandariza las variables y calcula los dos primeros componentes.
# Devuelve los datos, las correlaciones y los resultados del PCA.
# ==============================================================================

pca_demografico <- function(datos, variables) {
  
  # 1. Seleccionar las variables del análisis
  X <- datos |>
    select(all_of(variables)) |>
    as.data.frame()
  
  # Identificar cada observación por país
  rownames(X) <- datos$pais
  
  # 2. Comprobar que los datos sean adecuados
  stopifnot(
    !anyDuplicated(datos$pais),
    all(sapply(X, is.numeric)),
    all(is.finite(as.matrix(X)))
  )
  
  # 3. Calcular las correlaciones
  correlaciones <- cor(X)
  
  # 4. Estimar el PCA con variables estandarizadas
  pca <- FactoMineR::PCA(
    X,
    scale.unit = TRUE,
    ncp = 2,
    graph = FALSE
  )
  
  # 5. Recuperar las coordenadas de los países
  coordenadas <- pca$ind$coord[, 1:2, drop = FALSE]
  
  # 6. Devolver los resultados
  return(
    list(
      datos_pca = X,
      correlaciones = correlaciones,
      pca = pca,
      coordenadas = coordenadas
    )
  )
  
}