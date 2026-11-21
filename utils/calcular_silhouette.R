# ==============================================================================
# Función: calcular_silhouette
# ==============================================================================
# Recibe un HCPC y el PCA sobre el que se estimó. Ordena los clusters según el
# orden de los países en el PCA y calcula la silhouette.
# ==============================================================================

calcular_silhouette <- function(hcpc, pca) {
  
  # Coordenadas del PCA
  coordenadas <- pca$ind$coord[, 1:2, drop = FALSE]
  
  # Comprobar que ambos objetos contienen los mismos países
  stopifnot(
    setequal(
      rownames(coordenadas),
      rownames(hcpc$data.clust)
    )
  )
  
  # Ordenar los clusters según el orden del PCA
  grupos <- as.integer(
    hcpc$data.clust[rownames(coordenadas), "clust"]
  )
  
  stopifnot(!anyNA(grupos))
  
  # Calcular la silhouette con las asignaciones alineadas
  cluster::silhouette(grupos, dist(coordenadas))
}
