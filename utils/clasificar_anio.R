# ==============================================================================
# Función: clasificar_anio
# ==============================================================================
# Arma el PCA y el HCPC (k = 2) de un año y le asigna a cada país su perfil.
# Los números 1 y 2 del clustering no tienen significado demográfico: el grupo
# avanzado se identifica por su promedio de población de 65+.
# ==============================================================================

clasificar_anio <- function(base, variables, anio_elegido) {
  
  datos <- base |>
    dplyr::filter(anio == anio_elegido)
  
  matriz <- datos |>
    dplyr::select(dplyr::all_of(variables)) |>
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
    dplyr::transmute(codigo_pais, cluster = as.character(clust))
  
  stopifnot(anyDuplicated(grupos$codigo_pais) == 0)
  
  clasificados <- datos |>
    dplyr::left_join(grupos, by = "codigo_pais", relationship = "one-to-one")
  
  if (any(is.na(clasificados$cluster))) {
    stop(paste("No se asignaron todos los grupos en", anio_elegido))
  }
  
  # Identificamos el grupo más avanzado por su promedio de población 65+
  grupo_avanzado <- clasificados |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(media_65 = mean(poblacion_65_mas), .groups = "drop") |>
    dplyr::slice_max(media_65, n = 1, with_ties = FALSE) |>
    dplyr::pull(cluster)
  
  clasificados |>
    dplyr::mutate(perfil = dplyr::if_else(cluster == grupo_avanzado,
                                          "Transición avanzada", "Perfil demográfico joven")) |>
    dplyr::select(anio, pais, codigo_pais, perfil)
}
