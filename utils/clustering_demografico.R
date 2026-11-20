# ==============================================================================
# Función: clustering demográfico
# ==============================================================================
# Recibe el PCA y los datos de un año.
# Construye los clusters, calcula la silhouette y caracteriza los grupos.
# ==============================================================================

clustering_demografico <- function(pca, datos, variables, k = 2) {
  
  # 1. Estimar el clustering
  set.seed(42)
  
  hcpc <- FactoMineR::HCPC(
    pca,
    nb.clust = k,
    consol = TRUE,
    graph = FALSE
  )
  
  # 2. Recuperar el grupo asignado a cada país
  perfiles <- hcpc$data.clust |>
    as.data.frame() |>
    tibble::rownames_to_column("pais") |>
    dplyr::select(pais, clust)
  
  # 3. Incorporar los clusters a la base original
  datos_cluster <- datos |>
    dplyr::left_join(
      perfiles,
      by = "pais",
      relationship = "one-to-one"
    )
  
  # Comprobar que todos los países recibieron un cluster
  stopifnot(
    nrow(datos_cluster) == nrow(datos),
    !anyNA(datos_cluster$clust)
  )
  
  # 4. Recuperar las coordenadas del PCA
  coordenadas <- pca$ind$coord[, 1:2, drop = FALSE]
  
  # Ordenar los grupos según las coordenadas
  grupos <- as.integer(
    perfiles$clust[
      match(rownames(coordenadas), perfiles$pais)
    ]
  )
  
  stopifnot(!anyNA(grupos))
  
  # 5. Calcular la silhouette
  sil <- cluster::silhouette(
    grupos,
    dist(coordenadas)
  )
  
  # 6. Caracterizar los grupos
  caracterizacion <- datos_cluster |>
    dplyr::group_by(clust) |>
    dplyr::summarise(
      n_paises = dplyr::n(),
      dplyr::across(
        dplyr::all_of(variables),
        mean
      ),
      .groups = "drop"
    )
  
  # 7. Devolver los resultados
  return(
    list(
      hcpc = hcpc,
      perfiles = perfiles,
      datos_cluster = datos_cluster,
      grupos = grupos,
      silhouette = sil,
      caracterizacion = caracterizacion
    )
  )
  
}