# ==============================================================================
# Función: evaluar cantidades de clusters mediante silhouette
# ==============================================================================
# Recibe un PCA y compara soluciones con diferentes cantidades de grupos.
# Devuelve la silhouette media y el tamaño del grupo más pequeño.
# ==============================================================================

evaluar_clusters_demograficos <- function(pca, valores_k = 2:10) {
  
  # Coordenadas de los países en los dos primeros componentes
  coordenadas <- pca$ind$coord[, 1:2, drop = FALSE]
  
  # Evaluar cada cantidad de clusters
  resultados <- purrr::map_dfr(valores_k, function(k) {
    
    # Fijar la semilla para reproducir cada solución
    set.seed(42)
    
    # Construir los clusters
    modelo_k <- FactoMineR::HCPC(
      pca,
      nb.clust = k,
      consol = TRUE,
      graph = FALSE
    )
    
    # Comprobar que estén presentes los mismos países
    stopifnot(
      setequal(
        rownames(coordenadas),
        rownames(modelo_k$data.clust)
      )
    )
    
    # Ordenar las asignaciones según las coordenadas
    grupos <- as.integer(
      modelo_k$data.clust[rownames(coordenadas), "clust"]
    )
    
    # Calcular la silhouette
    sil <- cluster::silhouette(
      grupos,
      dist(coordenadas)
    )
    
    # Guardar los resultados de esta solución
    tibble::tibble(
      k = k,
      silhouette_promedio = mean(sil[, "sil_width"]),
      grupo_mas_chico = min(table(grupos))
    )
    
  })
  
  return(resultados)
  
}