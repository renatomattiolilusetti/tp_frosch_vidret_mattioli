# ==============================================================================
# Clustering demográficos
# ==============================================================================

# install.packages("gt")
library(dplyr)
library(webshot2)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(cluster)
library(gt)
wdi_limpia <- read_csv("repo_wdi/wdi_limpia.csv")

# Buscamos identificar qué perfiles demográficos existen en distintos momentos 
# del tiempo y observar si los países cambian entre esos perfiles. 

vars_demograficas <- c(
  "poblacion_65_mas",
  "fecundidad",
  "esperanza_vida",
  "crecimiento_poblacional"
)

base_cluster_demografico <- wdi_limpia |>
  select(
    pais,
    codigo_pais,
    anio,
    poblacion_65_mas,
    fecundidad,
    esperanza_vida,
    crecimiento_poblacional
  )

# Contamos para cada año cuántos países tienen dato en cada variable y cuántos tienen 
# completas las cuatro variables seleccionadas simultáneamente.

cobertura_cluster_demografico <- base_cluster_demografico |>
  group_by(anio) |>
  summarise(
    paises_65_mas =
      sum(!is.na(poblacion_65_mas)),
    paises_fecundidad =
      sum(!is.na(fecundidad)),
    paises_esperanza_vida =
      sum(!is.na(esperanza_vida)),
    paises_crecimiento =
      sum(!is.na(crecimiento_poblacional)),
    paises_completos =
      sum(
        !is.na(poblacion_65_mas) &
          !is.na(fecundidad) &
          !is.na(esperanza_vida) &
          !is.na(crecimiento_poblacional)
      ),
    .groups = "drop"
  )

# Los momentos elegidos son 2024, 2010, 1995 y 1980. Pero 1980 tiene un país con 
# datos faltantes. Analizamos primero esto:

datos_1980 <- wdi_limpia |>
  filter(anio == 1980) |>
  select(
    pais,
    codigo_pais,
    poblacion_65_mas,
    fecundidad,
    esperanza_vida,
    crecimiento_poblacional
  )

# Lo identificamos 

datos_1980 |>
  filter(
    is.na(poblacion_65_mas) |
      is.na(fecundidad) |
      is.na(esperanza_vida) |
      is.na(crecimiento_poblacional)
  )

# El país faltante es West Bank and Gaza. Para ver qué hacemos con él, revisamos 
# qué pasa con los faltantes de los años cercanos. No hay completitud en los datos
# desde 1960-1991 (pero en 1960 se corresponde a 0 datos en paises_crecimiento).

faltantes_1961_1991 <- wdi_limpia |>
  filter(anio >= 1961, anio <= 1991) |>
  select(
    pais,
    anio,
    poblacion_65_mas,
    fecundidad,
    esperanza_vida,
    crecimiento_poblacional
  ) |>
  mutate(
    falta_65_mas = is.na(poblacion_65_mas),
    falta_fecundidad = is.na(fecundidad),
    falta_esperanza_vida = is.na(esperanza_vida),
    falta_crecimiento = is.na(crecimiento_poblacional),
    
    cantidad_faltantes =
      falta_65_mas +
      falta_fecundidad +
      falta_esperanza_vida +
      falta_crecimiento
  ) |>
  filter(cantidad_faltantes > 0) |>
  rowwise() |>
  mutate(
    variables_faltantes = paste(
      c(
        if (falta_65_mas) "poblacion_65_mas",
        if (falta_fecundidad) "fecundidad",
        if (falta_esperanza_vida) "esperanza_vida",
        if (falta_crecimiento) "crecimiento_poblacional"
      ),
      collapse = ", "
    )
  ) |>
  ungroup() |>
  select(
    anio,
    pais,
    cantidad_faltantes,
    variables_faltantes
  ) |>
  arrange(desc(anio), pais)

faltantes_1961_1991

# La falta de información viene de un único país y afecta tres de las cuatro variables
# utilizadas. Dado que imputarlas implicaría estimar prácticamente todo su perfil demográfico 
# y que representa solo el 0,5% de la muestra, decidimos excluir del análisis a este caso.

base_cluster_demografico <- base_cluster_demografico |>
  filter(codigo_pais != "PSE")

# Ahora sí, generamos los momentos:

datos_1980 <- base_cluster_demografico |>
  filter(anio == 1980)

datos_1995 <- base_cluster_demografico |>
  filter(anio == 1995)

datos_2010 <- base_cluster_demografico |>
  filter(anio == 2010)

datos_2024 <- base_cluster_demografico |>
  filter(anio == 2024)

# Prueba de cluster 2024 ####

# Vemos las correlaciones entre las variables: si tienen correlación es lógico 
# poder agruparlas.

datos_2024 |>
  select(
    poblacion_65_mas,
    fecundidad,
    esperanza_vida,
    crecimiento_poblacional
  ) |>
  cor() |>
  round(2)

# Armamos PCA demográfico
# Matriz que entra al PCA:

X_2024 <- datos_2024 |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_2024) <- datos_2024$pais

# Armamos PCA

pca_2024 <- PCA(
  X_2024,
  scale.unit = TRUE, # estandarizamos las cuatro variables
  ncp = 2,
  graph = FALSE
)

# Cuánta información explica cada componente

pca_2024$eig

# Cómo se relaciona cada variable con los componentes

pca_2024$var$coord[, 1:2]

# Contribución de cada variable a cada componente

pca_2024$var$contrib[, 1:2]

# Como PC1 junto con PC2 explican el 89% de la información, conservamos esos PC.

# Coordenadas de los países en PC1 y PC2

coords_2024 <- pca_2024$ind$coord[, 1:2]

# Comparar distintas cantidades de clusters

silhouette_2024 <- map_dfr(2:10, function(k) {
  
  hcpc_k <- HCPC(
    pca_2024,
    nb.clust = k,
    consol = TRUE,
    graph = FALSE
  )
  
  grupos_k <- as.integer(hcpc_k$data.clust$clust)
  
  sil_k <- silhouette(
    grupos_k,
    dist(coords_2024)
  )
  
  tibble(
    k = k,
    silhouette_promedio = mean(sil_k[, "sil_width"]),
    grupo_mas_chico = min(table(grupos_k))
  )
})

silhouette_2024

# Construimos el cluster definitivo, con separación en k=2.

hcpc_2024_k2 <- HCPC(
  pca_2024,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

# Tamaño de cada grupo

table(hcpc_2024_k2$data.clust$clust)

# Analizamos silhouette por cluster

grupos_2024 <- as.integer(hcpc_2024_k2$data.clust$clust)

sil_2024 <- silhouette(
  grupos_2024,
  dist(coords_2024)
)

as.data.frame(sil_2024) |>
  as_tibble() |>
  group_by(cluster) |>
  summarise(
    n = n(),
    media = mean(sil_width),
    mediana = median(sil_width),
    negativos = sum(sil_width < 0)
  )

# Caracterizamos los dos perfiles demográficos

perfiles_2024 <- hcpc_2024_k2$data.clust |>
  as.data.frame() |>
  rownames_to_column("pais") |>
  select(pais, clust)

datos_2024_cluster <- datos_2024 |>
  left_join(perfiles_2024, by = "pais")

datos_2024_cluster |>
  group_by(clust) |>
  summarise(
    n = n(),
    poblacion_65_mas = mean(poblacion_65_mas),
    fecundidad = mean(fecundidad),
    esperanza_vida = mean(esperanza_vida),
    crecimiento_poblacional = mean(crecimiento_poblacional)
  )

# Prueba de cluster 2010 ####

# Armamos mismo PCA

X_2010 <- datos_2010 |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_2010) <- datos_2010$pais

pca_2010 <- PCA(
  X_2010,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)

# Cuánta información explica cada componente

pca_2010$eig

# Cómo se relacionan las variables con PC1 y PC2

pca_2010$var$coord[, 1:2]

# Contribución de cada variable

pca_2010$var$contrib[, 1:2]

# Vemos qué cantidad de clusters funciona mejor:

coords_2010 <- pca_2010$ind$coord[, 1:2]

silhouette_2010 <- map_dfr(2:10, function(k) {
  
  hcpc_k <- HCPC(
    pca_2010,
    nb.clust = k,
    consol = TRUE,
    graph = FALSE
  )
  
  grupos_k <- as.integer(hcpc_k$data.clust$clust)
  
  sil_k <- silhouette(
    grupos_k,
    dist(coords_2010)
  )
  
  tibble(
    k = k,
    silhouette_promedio = mean(sil_k[, "sil_width"]),
    grupo_mas_chico = min(table(grupos_k))
  )
})

silhouette_2010

# Nos quedamos con k=2 también para este período. Repetimos chequeo de silhouette
# por grupo:

hcpc_2010_k2 <- HCPC(
  pca_2010,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

# Tamaño de cada grupo

table(hcpc_2010_k2$data.clust$clust)

# Silhouette por cluster

grupos_2010 <- as.integer(hcpc_2010_k2$data.clust$clust)

sil_2010 <- silhouette(
  grupos_2010,
  dist(coords_2010)
)

as.data.frame(sil_2010) |>
  as_tibble() |>
  group_by(cluster) |>
  summarise(
    n = n(),
    media = mean(sil_width),
    mediana = median(sil_width),
    negativos = sum(sil_width < 0)
  )

# Caracterizamos los dos perfiles:

perfiles_2010 <- hcpc_2010_k2$data.clust |>
  as.data.frame() |>
  rownames_to_column("pais") |>
  select(pais, clust)

datos_2010_cluster <- datos_2010 |>
  left_join(perfiles_2010, by = "pais")

datos_2010_cluster |>
  group_by(clust) |>
  summarise(
    n = n(),
    poblacion_65_mas = mean(poblacion_65_mas),
    fecundidad = mean(fecundidad),
    esperanza_vida = mean(esperanza_vida),
    crecimiento_poblacional = mean(crecimiento_poblacional)
  )

# Prueba de cluster 1995 ####

# Armamos el PCA:

X_1995 <- datos_1995 |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_1995) <- datos_1995$pais

pca_1995 <- PCA(
  X_1995,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)

pca_1995$eig
pca_1995$var$coord[, 1:2]
pca_1995$var$contrib[, 1:2]

# Repetimos el procedimiento de los clusters anteriores para elegir cantidad en 
# este año

coords_1995 <- pca_1995$ind$coord[, 1:2]

silhouette_1995 <- map_dfr(2:10, function(k) {
  
  hcpc_k <- HCPC(
    pca_1995,
    nb.clust = k,
    consol = TRUE,
    graph = FALSE
  )
  
  grupos_k <- as.integer(hcpc_k$data.clust$clust)
  
  sil_k <- silhouette(
    grupos_k,
    dist(coords_1995)
  )
  
  tibble(
    k = k,
    silhouette_promedio = mean(sil_k[, "sil_width"]),
    grupo_mas_chico = min(table(grupos_k))
  )
})

silhouette_1995

# Seguimos con k=2

hcpc_1995_k2 <- HCPC(
  pca_1995,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

# Tamaño de cada grupo

table(hcpc_1995_k2$data.clust$clust)

# Silhouette por cluster

grupos_1995 <- as.integer(hcpc_1995_k2$data.clust$clust)

sil_1995 <- silhouette(
  grupos_1995,
  dist(coords_1995)
)

as.data.frame(sil_1995) |>
  as_tibble() |>
  group_by(cluster) |>
  summarise(
    n = n(),
    media = mean(sil_width),
    mediana = median(sil_width),
    negativos = sum(sil_width < 0)
  )

# Dejamos la caracterización de los dos perfiles

perfiles_1995 <- hcpc_1995_k2$data.clust |>
  as.data.frame() |>
  rownames_to_column("pais") |>
  select(pais, clust)

datos_1995_cluster <- datos_1995 |>
  left_join(perfiles_1995, by = "pais")

datos_1995_cluster |>
  group_by(clust) |>
  summarise(
    n = n(),
    poblacion_65_mas = mean(poblacion_65_mas),
    fecundidad = mean(fecundidad),
    esperanza_vida = mean(esperanza_vida),
    crecimiento_poblacional = mean(crecimiento_poblacional)
  )

# Prueba de cluster 1980 ####

# Seguimos con 1980:

X_1980 <- datos_1980 |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_1980) <- datos_1980$pais

pca_1980 <- PCA(
  X_1980,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)

# Cuánta información explica cada componente

pca_1980$eig

# Cómo se relaciona cada variable con PC1 y PC2

pca_1980$var$coord[, 1:2]

# Cuánto aporta cada variable a PC1 y PC2

pca_1980$var$contrib[, 1:2]

# Para ver selección de clusters:

coords_1980 <- pca_1980$ind$coord[, 1:2]

silhouette_1980 <- map_dfr(2:10, function(k) {
  
  hcpc_k <- HCPC(
    pca_1980,
    nb.clust = k,
    consol = TRUE,
    graph = FALSE
  )
  
  grupos_k <- as.integer(hcpc_k$data.clust$clust)
  
  sil_k <- silhouette(
    grupos_k,
    dist(coords_1980)
  )
  
  tibble(
    k = k,
    silhouette_promedio = mean(sil_k[, "sil_width"]),
    grupo_mas_chico = min(table(grupos_k))
  )
})

silhouette_1980

# Efectivamente tambien conviene quedarnos con 2 grupos. Guardamos esta data:

hcpc_1980_k2 <- HCPC(
  pca_1980,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

table(hcpc_1980_k2$data.clust$clust)

grupos_1980 <- as.integer(hcpc_1980_k2$data.clust$clust)

sil_1980 <- silhouette(
  grupos_1980,
  dist(coords_1980)
)

as.data.frame(sil_1980) |>
  as_tibble() |>
  group_by(cluster) |>
  summarise(
    n = n(),
    media = mean(sil_width),
    mediana = median(sil_width),
    negativos = sum(sil_width < 0)
  )

# Caracterizamos los perfiles:

perfiles_1980 <- hcpc_1980_k2$data.clust |>
  as.data.frame() |>
  rownames_to_column("pais") |>
  select(pais, clust)

datos_1980_cluster <- datos_1980 |>
  left_join(perfiles_1980, by = "pais")

datos_1980_cluster |>
  group_by(clust) |>
  summarise(
    n = n(),
    poblacion_65_mas = mean(poblacion_65_mas),
    fecundidad = mean(fecundidad),
    esperanza_vida = mean(esperanza_vida),
    crecimiento_poblacional = mean(crecimiento_poblacional)
  )

# Análisis de outliers ####

# Utilizando el método IQR

outliers_cluster_demografico <- bind_rows(
  datos_1980 |> mutate(anio = 1980),
  datos_1995 |> mutate(anio = 1995),
  datos_2010 |> mutate(anio = 2010),
  datos_2024 |> mutate(anio = 2024)
) |>
  pivot_longer(
    cols = all_of(vars_demograficas),
    names_to = "variable",
    values_to = "valor"
  ) |>
  group_by(anio, variable) |>
  mutate(
    q1 = quantile(valor, 0.25),
    q3 = quantile(valor, 0.75),
    iqr = q3 - q1,
    limite_inferior = q1 - 1.5 * iqr,
    limite_superior = q3 + 1.5 * iqr
  ) |>
  filter(
    valor < limite_inferior |
      valor > limite_superior
  ) |>
  ungroup() |>
  select(
    anio,
    pais,
    variable,
    valor
  ) |>
  arrange(variable, anio, desc(valor))

outliers_cluster_demografico

# Qué países aparecen más de una vez como outliers?

outliers_cluster_demografico |>
  count(pais, sort = TRUE)

# Se repiten en la misma variable? 

outliers_cluster_demografico |>
  count(pais, variable, sort = TRUE)

# Los outliers son genuinos y en varios casos persistentes en el tiempo. Así que 
# se conservan en el análisis.

# Comparaciones de los momentos ####

# Primero, los clusters mantienen el mismo significado a lo largo del tiempo?

perfiles_comparados <- bind_rows(
  datos_1980_cluster |>
    mutate(anio = 1980),
  
  datos_1995_cluster |>
    mutate(anio = 1995),
  
  datos_2010_cluster |>
    mutate(anio = 2010),
  
  datos_2024_cluster |>
    mutate(anio = 2024)
) |>
  group_by(anio, clust) |>
  summarise(
    n = n(),
    poblacion_65_mas = mean(poblacion_65_mas),
    fecundidad = mean(fecundidad),
    esperanza_vida = mean(esperanza_vida),
    crecimiento_poblacional = mean(crecimiento_poblacional),
    .groups = "drop"
  ) |>
  arrange(anio, clust)

perfiles_comparados

# Vemos que sí, se mantienen las caracteristicas de los grupos
# Ahora formamos trayectoria_paises, agregando el grupo a cada país, para ver 
# cuándo cambiaron. 

trayectorias_paises <- perfiles_1980 |>
  rename(cluster_1980 = clust) |>
  left_join(
    perfiles_1995 |>
      rename(cluster_1995 = clust),
    by = "pais"
  ) |>
  left_join(
    perfiles_2010 |>
      rename(cluster_2010 = clust),
    by = "pais"
  ) |>
  left_join(
    perfiles_2024 |>
      rename(cluster_2024 = clust),
    by = "pais"
  )

# Contamos cuántos siguieron cada trayectoria

trayectorias_paises |>
  count(
    cluster_1980,
    cluster_1995,
    cluster_2010,
    cluster_2024,
    sort = TRUE
  )

# 74 permanecieron en el perfil 2, mientras que 71 en el perfil 1. Suman 145 países.
# 33 países cambiaron de la trayectoria 1 a la 2 y se mantuvieron en la 2 luego.
# (13+14+6). Mientras que 16 alternan entre grupos (filas 5 y de 7 a 10). 

# Cuáles son los países que pasan entre perfiles? (de los 33). 

# Cambiaron entre 1980 y 1995

trayectorias_paises |>
  filter(
    cluster_1980 == 1,
    cluster_1995 == 2,
    cluster_2010 == 2,
    cluster_2024 == 2
  ) |>
  select(pais)

# Cambiaron entre 1995 y 2010

trayectorias_paises |>
  filter(
    cluster_1980 == 1,
    cluster_1995 == 1,
    cluster_2010 == 2,
    cluster_2024 == 2
  ) |>
  select(pais)

# Cambiaron entre 2010 y 2024

trayectorias_paises |>
  filter(
    cluster_1980 == 1,
    cluster_1995 == 1,
    cluster_2010 == 1,
    cluster_2024 == 2
  ) |>
  select(pais)

# Qué sucede con los que tienen trayectorias no lineales?

trayectorias_no_lineales <- trayectorias_paises |>
  filter(
    !(cluster_1980 == 1 & cluster_1995 == 1 & cluster_2010 == 1 & cluster_2024 == 1),
    !(cluster_1980 == 2 & cluster_1995 == 2 & cluster_2010 == 2 & cluster_2024 == 2),
    !(cluster_1980 == 1 & cluster_1995 == 2 & cluster_2010 == 2 & cluster_2024 == 2),
    !(cluster_1980 == 1 & cluster_1995 == 1 & cluster_2010 == 2 & cluster_2024 == 2),
    !(cluster_1980 == 1 & cluster_1995 == 1 & cluster_2010 == 1 & cluster_2024 == 2)
  )

trayectorias_no_lineales

# Prueba de sensibilidad (sin outliers) ####

# Si un país aparece como outlier al menos una vez en cualquiera de los cuatro años 
# o variables, lo excluimos de 1980, 1995, 2010 y 2024 para esta prueba de sensibilidad

# Identificamos todos los países que aparecen como outlier
# en al menos una variable y en al menos uno de los cuatro años


paises_outliers_total <- outliers_cluster_demografico |>
  distinct(pais)

paises_outliers_total

# Cantidad de países distintos que presentan al menos un outlier

nrow(paises_outliers_total)

# Cuántos países perdemos y cuántos quedan para la prueba de sensibilidad

resumen_exclusion_outliers <- tibble(
  paises_originales = 194,
  paises_excluidos = nrow(paises_outliers_total),
  paises_restantes = 194 - nrow(paises_outliers_total),
  porcentaje_excluido = 100 * nrow(paises_outliers_total) / 194
)

resumen_exclusion_outliers

# Excluimos esos mismos países de los cuatro períodos

datos_1980_sin_outliers <- datos_1980 |>
  anti_join(paises_outliers_total, by = "pais")

datos_1995_sin_outliers <- datos_1995 |>
  anti_join(paises_outliers_total, by = "pais")

datos_2010_sin_outliers <- datos_2010 |>
  anti_join(paises_outliers_total, by = "pais")

datos_2024_sin_outliers <- datos_2024 |>
  anti_join(paises_outliers_total, by = "pais")

# Repetimos PCA
# 1980

X_1980_sin_outliers <- datos_1980_sin_outliers |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_1980_sin_outliers) <- datos_1980_sin_outliers$pais

pca_1980_sin_outliers <- PCA(
  X_1980_sin_outliers,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)


# 1995

X_1995_sin_outliers <- datos_1995_sin_outliers |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_1995_sin_outliers) <- datos_1995_sin_outliers$pais

pca_1995_sin_outliers <- PCA(
  X_1995_sin_outliers,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)


# 2010

X_2010_sin_outliers <- datos_2010_sin_outliers |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_2010_sin_outliers) <- datos_2010_sin_outliers$pais

pca_2010_sin_outliers <- PCA(
  X_2010_sin_outliers,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)


# 2024

X_2024_sin_outliers <- datos_2024_sin_outliers |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_2024_sin_outliers) <- datos_2024_sin_outliers$pais

pca_2024_sin_outliers <- PCA(
  X_2024_sin_outliers,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)

# Comparamos cuanto explican los componentes CON y SIN outliers

sensibilidad_pca_outliers <- tibble(
  anio = c(1980, 1995, 2010, 2024),
  
  pc1_original = c(
    pca_1980$eig[1, 2],
    pca_1995$eig[1, 2],
    pca_2010$eig[1, 2],
    pca_2024$eig[1, 2]
  ),
  
  pc1_sin_outliers = c(
    pca_1980_sin_outliers$eig[1, 2],
    pca_1995_sin_outliers$eig[1, 2],
    pca_2010_sin_outliers$eig[1, 2],
    pca_2024_sin_outliers$eig[1, 2]
  ),
  
  pc1_pc2_original = c(
    pca_1980$eig[2, 3],
    pca_1995$eig[2, 3],
    pca_2010$eig[2, 3],
    pca_2024$eig[2, 3]
  ),
  
  pc1_pc2_sin_outliers = c(
    pca_1980_sin_outliers$eig[2, 3],
    pca_1995_sin_outliers$eig[2, 3],
    pca_2010_sin_outliers$eig[2, 3],
    pca_2024_sin_outliers$eig[2, 3]
  )
)

sensibilidad_pca_outliers

# Repetimos clustering sin países con outliers

hcpc_1980_sin_outliers <- HCPC(
  pca_1980_sin_outliers,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

hcpc_1995_sin_outliers <- HCPC(
  pca_1995_sin_outliers,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

hcpc_2010_sin_outliers <- HCPC(
  pca_2010_sin_outliers,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

hcpc_2024_sin_outliers <- HCPC(
  pca_2024_sin_outliers,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

# Comparación de silhouette

sil_1980_sin_outliers <- silhouette(
  as.integer(hcpc_1980_sin_outliers$data.clust$clust),
  dist(pca_1980_sin_outliers$ind$coord[, 1:2])
)

sil_1995_sin_outliers <- silhouette(
  as.integer(hcpc_1995_sin_outliers$data.clust$clust),
  dist(pca_1995_sin_outliers$ind$coord[, 1:2])
)

sil_2010_sin_outliers <- silhouette(
  as.integer(hcpc_2010_sin_outliers$data.clust$clust),
  dist(pca_2010_sin_outliers$ind$coord[, 1:2])
)

sil_2024_sin_outliers <- silhouette(
  as.integer(hcpc_2024_sin_outliers$data.clust$clust),
  dist(pca_2024_sin_outliers$ind$coord[, 1:2])
)

sensibilidad_cluster_outliers <- tibble(
  anio = c(1980, 1995, 2010, 2024),
  
  silhouette_original = c(
    mean(sil_1980[, "sil_width"]),
    mean(sil_1995[, "sil_width"]),
    mean(sil_2010[, "sil_width"]),
    mean(sil_2024[, "sil_width"])
  ),
  
  silhouette_sin_outliers = c(
    mean(sil_1980_sin_outliers[, "sil_width"]),
    mean(sil_1995_sin_outliers[, "sil_width"]),
    mean(sil_2010_sin_outliers[, "sil_width"]),
    mean(sil_2024_sin_outliers[, "sil_width"])
  )
)

sensibilidad_cluster_outliers

# Silhouette no cambia demasiado al sacar los países que tuvieron algún outlier.
# No hay evidencia de que los outliers estén afectando artificialmente los dos grupos.

# Tabla resumen por año y variable 

outliers_cluster_demografico |>
  count(anio, variable)

outliers_cluster_demografico |>
  arrange(anio, variable, pais)

# Gráficos para PPT ####

# Silhouette media de la solución k = 2 por año

silhouette_resumen <- tibble(
  anio = factor(
    c("1980", "1995", "2010", "2024"),
    levels = c("1980", "1995", "2010", "2024")
  ),
  silhouette_media = c(
    0.552,
    0.488,
    0.498,
    0.507
  )
)

grafico_silhouette <- ggplot(
  silhouette_resumen,
  aes(
    x = anio,
    y = silhouette_media
  )
) +
  geom_col(
    width = 0.6
  ) +
  geom_text(
    aes(label = round(silhouette_media, 3)),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "Calidad de la solución de dos clusters",
    x = NULL,
    y = "Silhouette media"
  ) +
  ylim(0, 0.62) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 11
    )
  )

grafico_silhouette

ggsave(
  "silhouette_clusters.png",
  plot = grafico_silhouette,
  width = 5,
  height = 3,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Correlaciones entre las variables - ejemplo 2024

cor_2024 <- datos_2024 |>
  select(all_of(vars_demograficas)) |>
  cor() |>
  as.data.frame() |>
  rownames_to_column("variable_1") |>
  pivot_longer(
    -variable_1,
    names_to = "variable_2",
    values_to = "correlacion"
  ) |>
  mutate(
    variable_1 = recode(
      variable_1,
      poblacion_65_mas = "Población 65+",
      fecundidad = "Fecundidad",
      esperanza_vida = "Esperanza de vida",
      crecimiento_poblacional = "Crecimiento"
    ),
    variable_2 = recode(
      variable_2,
      poblacion_65_mas = "Población 65+",
      fecundidad = "Fecundidad",
      esperanza_vida = "Esperanza de vida",
      crecimiento_poblacional = "Crecimiento"
    )
  )

grafico_correlaciones <- ggplot(
  cor_2024,
  aes(
    x = variable_2,
    y = variable_1,
    fill = correlacion
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = round(correlacion, 2)),
    size = 3.2
  ) +
  scale_fill_gradient2(
    low = "firebrick",
    mid = "white",
    high = "steelblue",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlaciones entre variables",
    subtitle = "Ejemplo: 2024",
    x = NULL,
    y = NULL,
    fill = "r"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 7
    ),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(
      face = "bold",
      size = 11
    ),
    plot.subtitle = element_text(size = 8),
    legend.position = "none"
  )

grafico_correlaciones

ggsave(
  "correlaciones_2024.png",
  plot = grafico_correlaciones,
  width = 4.5,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Trayectorias resumen

trayectorias_resumen <- trayectorias_paises |>
  count(
    cluster_1980,
    cluster_1995,
    cluster_2010,
    cluster_2024,
    sort = TRUE
  ) |>
  rename(
    `1980` = cluster_1980,
    `1995` = cluster_1995,
    `2010` = cluster_2010,
    `2024` = cluster_2024,
    `Países` = n
  )

tabla_trayectorias <- trayectorias_resumen |>
  gt() |>
  tab_header(
    title = "Trayectorias entre perfiles demográficos",
    subtitle = "Pertenencia a clusters en los cuatro momentos"
  ) |>
  cols_align(
    align = "center"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(11),
    data_row.padding = px(4)
  )

tabla_trayectorias

gtsave(
  tabla_trayectorias,
  "tabla_trayectorias.png",
  zoom = 2
)

# Tabla perfiles comparados 

tabla_perfiles <- perfiles_comparados |>
rename(
  `Año` = anio,
  `Cluster` = clust,
  `Países` = n,
  `Población 65+ (%)` = poblacion_65_mas,
  `Fecundidad` = fecundidad,
  `Esperanza de vida` = esperanza_vida,
  `Crecimiento (%)` = crecimiento_poblacional
) |>
  gt() |>
  fmt_number(
    columns = c(
      `Población 65+ (%)`,
      `Fecundidad`,
      `Esperanza de vida`,
      `Crecimiento (%)`
    ),
    decimals = 2
  ) |>
  tab_header(
    title = "Evolución de los perfiles demográficos",
    subtitle = "Características promedio de los clusters"
  ) |>
  cols_align(
    align = "center"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(11),
    data_row.padding = px(4)
  )

tabla_perfiles

gtsave(
  tabla_perfiles,
  "tabla_perfiles_demograficos.png",
  zoom = 2
)

# Cambio de pesos 

evolucion_clusters <- perfiles_comparados |>
  select(anio, clust, n) |>
  mutate(
    perfil = if_else(
      clust == 1,
      "Perfil joven y expansivo",
      "Transición avanzada"
    )
  )

grafico_evolucion_clusters <- ggplot(
  evolucion_clusters,
  aes(
    x = factor(anio),
    y = n,
    fill = perfil
  )
) +
  geom_col(
    position = "stack",
    width = 0.65
  ) +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  labs(
    title = "Cambio en los perfiles demográficos",
    subtitle = "Cantidad de países por cluster",
    x = NULL,
    y = "Países",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 11
    ),
    legend.position = "bottom"
  )

grafico_evolucion_clusters

ggsave(
  "evolucion_clusters.png",
  plot = grafico_evolucion_clusters,
  width = 5,
  height = 3.3,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Silhouettes por cluster

silhouette_clusters <- bind_rows(
  
  as.data.frame(sil_1980) |>
    as_tibble() |>
    group_by(cluster) |>
    summarise(
      n = n(),
      media = mean(sil_width),
      mediana = median(sil_width),
      negativos = sum(sil_width < 0),
      .groups = "drop"
    ) |>
    mutate(anio = 1980),
  
  as.data.frame(sil_1995) |>
    as_tibble() |>
    group_by(cluster) |>
    summarise(
      n = n(),
      media = mean(sil_width),
      mediana = median(sil_width),
      negativos = sum(sil_width < 0),
      .groups = "drop"
    ) |>
    mutate(anio = 1995),
  
  as.data.frame(sil_2010) |>
    as_tibble() |>
    group_by(cluster) |>
    summarise(
      n = n(),
      media = mean(sil_width),
      mediana = median(sil_width),
      negativos = sum(sil_width < 0),
      .groups = "drop"
    ) |>
    mutate(anio = 2010),
  
  as.data.frame(sil_2024) |>
    as_tibble() |>
    group_by(cluster) |>
    summarise(
      n = n(),
      media = mean(sil_width),
      mediana = median(sil_width),
      negativos = sum(sil_width < 0),
      .groups = "drop"
    ) |>
    mutate(anio = 2024)
  
) |>
  select(anio, cluster, n, media, mediana, negativos) |>
  arrange(anio, cluster)

silhouette_clusters

tabla_silhouette_clusters <- silhouette_clusters |>
  rename(
    `Año` = anio,
    `Cluster` = cluster,
    `Países` = n,
    `Silhouette media` = media,
    `Silhouette mediana` = mediana,
    `Negativas` = negativos
  ) |>
  gt() |>
  fmt_number(
    columns = c(
      `Silhouette media`,
      `Silhouette mediana`
    ),
    decimals = 3
  ) |>
  tab_header(
    title = "Calidad de los clusters por período",
    subtitle = "Silhouette media y mediana por cluster"
  ) |>
  cols_align(
    align = "center"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_source_note(
    source_note = "Una silhouette negativa indica que un país está más próximo al otro cluster que al cluster al que fue asignado."
  ) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(11),
    data_row.padding = px(4)
  )

tabla_silhouette_clusters

gtsave(
  tabla_silhouette_clusters,
  "silhouette_por_cluster.png",
  zoom = 2
)
