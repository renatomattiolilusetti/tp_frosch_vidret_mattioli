# ==============================================================================
# Clustering demográfico
# ==============================================================================

# 1. Preparación del análisis

# Cargar los paquetes
library(tidyverse)
library(cluster)
library(FactoMineR)
library(gt)

# Fijar semilla para reproducibilidad
set.seed(42)

# Crear carpetas de resultados si no existen
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("auxiliar", recursive = TRUE, showWarnings = FALSE)

# Verificar que existan los archivos necesarios
archivos_necesarios <- c(
  "input/wdi_limpia.csv",
  "output/tablas/outliers_horizontales_wdi.csv",
  "utils/pca_demografico.R",
  "utils/evaluar_clusters_demograficos.R",
  "utils/clustering_demografico.R",
  "utils/calcular_silhouette.R"
)

archivos_faltantes <- archivos_necesarios[
  !file.exists(archivos_necesarios)
]

if (length(archivos_faltantes) > 0) {
  stop(
    "Faltan archivos necesarios: ",
    paste(archivos_faltantes, collapse = ", ")
  )
}

# Leer la base limpia
wdi_limpia <- read_csv(
  "input/wdi_limpia.csv",
  show_col_types = FALSE
)

# Verificar identificadores y duplicados
stopifnot(
  !anyNA(wdi_limpia$codigo_pais),
  !anyNA(wdi_limpia$anio),
  !anyDuplicated(wdi_limpia[c("codigo_pais", "anio")])
)

# Mostrar las dimensiones
print(dim(wdi_limpia))


# Buscamos identificar qué perfiles demográficos existen en distintos momentos 
# del tiempo y observar si los países cambian entre esos perfiles
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
# completas las cuatro variables seleccionadas simultáneamente
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

anios_analisis <- c(1980, 1995, 2010, 2024)

# Cantidad de países con información disponible
cobertura_cluster_demografico |>
  filter(anio %in% anios_analisis) |>
  print(n = Inf)


# Identificar países con alguna variable faltante
paises_incompletos <- base_cluster_demografico |>
  filter(anio %in% anios_analisis) |>
  filter(
    if_any(all_of(vars_demograficas), is.na)
  ) |>
  arrange(anio, pais)

print(paises_incompletos, n = Inf)

# Identificar países con información incompleta
paises_excluidos_cluster <- paises_incompletos |>
  distinct(codigo_pais, pais)

print(paises_excluidos_cluster)

# Excluir esos países de los cuatro períodos
base_cluster_demografico <- base_cluster_demografico |>
  filter(
    !codigo_pais %in% paises_excluidos_cluster$codigo_pais
  )

# Comprobar la cantidad de países y los faltantes
control_muestra_cluster <- base_cluster_demografico |>
  filter(anio %in% anios_analisis) |>
  group_by(anio) |>
  summarise(
    paises = n_distinct(codigo_pais),
    filas_incompletas = sum(
      if_any(all_of(vars_demograficas), is.na)
    ),
    .groups = "drop"
  )

print(control_muestra_cluster)

# Verificar que los cuatro años tengan igual cantidad
# de países y ninguna observación incompleta
stopifnot(
  all(control_muestra_cluster$paises ==
        control_muestra_cluster$paises[1]),
  all(control_muestra_cluster$filas_incompletas == 0)
)

# Cortes temporales

# Seleccionar los países de cada año
datos_1980 <- base_cluster_demografico |>
  filter(anio == 1980) |>
  arrange(codigo_pais)

datos_1995 <- base_cluster_demografico |>
  filter(anio == 1995) |>
  arrange(codigo_pais)

datos_2010 <- base_cluster_demografico |>
  filter(anio == 2010) |>
  arrange(codigo_pais)

datos_2024 <- base_cluster_demografico |>
  filter(anio == 2024) |>
  arrange(codigo_pais)


# Comprobar que los cuatro años contienen los mismos países
stopifnot(
  identical(datos_1980$codigo_pais, datos_1995$codigo_pais),
  identical(datos_1980$codigo_pais, datos_2010$codigo_pais),
  identical(datos_1980$codigo_pais, datos_2024$codigo_pais)
)

# Mostrar la cantidad de países en cada año
tibble(
  anio = c(1980, 1995, 2010, 2024),
  paises = c(
    nrow(datos_1980),
    nrow(datos_1995),
    nrow(datos_2010),
    nrow(datos_2024)
  )
) |>
  print()

# PCA y clustering demográfico - 2024 ####

# Cargar nuestra función
source("utils/pca_demografico.R")

# Ejecutar el PCA de 2024
resultado_pca_2024 <- pca_demografico(
  datos_2024,
  vars_demograficas
)

# Recuperar los resultados con los nombres originales
X_2024 <- resultado_pca_2024$datos_pca

matriz_correlaciones_2024 <- resultado_pca_2024$correlaciones

pca_2024 <- resultado_pca_2024$pca

coords_2024 <- resultado_pca_2024$coordenadas


# Mostrar los resultados
print(round(matriz_correlaciones_2024, 2))

print(round(pca_2024$eig, 2))

print(round(pca_2024$var$coord[, 1:2], 2))

print(round(pca_2024$var$contrib[, 1:2], 2))

# Selección cantidad de clusters 2024 

# Cargar la función de evaluación
source("utils/evaluar_clusters_demograficos.R")

# Comparar soluciones de 2 a 10 clusters
silhouette_2024 <- evaluar_clusters_demograficos(
  pca_2024,
  valores_k = 2:10
)

# Mostrar los resultados
print(silhouette_2024, n = Inf)

# Clustering definitivo 2024

# Cargar la función
source("utils/clustering_demografico.R")

# Ejecutar el clustering de 2024
resultado_cluster_2024 <- clustering_demografico(
  pca = pca_2024,
  datos = datos_2024,
  variables = vars_demograficas,
  k = 2
)

# Recuperar los resultados
hcpc_2024_k2 <- resultado_cluster_2024$hcpc

perfiles_2024 <- resultado_cluster_2024$perfiles

datos_2024_cluster <- resultado_cluster_2024$datos_cluster

grupos_2024 <- resultado_cluster_2024$grupos

sil_2024 <- resultado_cluster_2024$silhouette

caracterizacion_2024 <- resultado_cluster_2024$caracterizacion

# Mostrar los resultados
cat(
  "Silhouette media 2024:",
  round(mean(sil_2024[, "sil_width"]), 3),
  "\n"
)

print(caracterizacion_2024, n = Inf)

# PCA y clustering demográfico - 2010 ####

# 1. PCA
resultado_pca_2010 <- pca_demografico(
  datos_2010,
  vars_demograficas
)

X_2010 <- resultado_pca_2010$datos_pca
matriz_correlaciones_2010 <- resultado_pca_2010$correlaciones
pca_2010 <- resultado_pca_2010$pca
coords_2010 <- resultado_pca_2010$coordenadas

# Mostrar resultados del PCA
print(round(matriz_correlaciones_2010, 2))
print(round(pca_2010$eig, 2))
print(round(pca_2010$var$coord[, 1:2], 2))
print(round(pca_2010$var$contrib[, 1:2], 2))


# 2. Evaluar cantidades de clusters
silhouette_2010 <- evaluar_clusters_demograficos(
  pca_2010,
  valores_k = 2:10
)

print(silhouette_2010, n = Inf)


# 3. Clustering definitivo con k = 2
resultado_cluster_2010 <- clustering_demografico(
  pca = pca_2010,
  datos = datos_2010,
  variables = vars_demograficas,
  k = 2
)

# Recuperar los resultados
hcpc_2010_k2 <- resultado_cluster_2010$hcpc
perfiles_2010 <- resultado_cluster_2010$perfiles
datos_2010_cluster <- resultado_cluster_2010$datos_cluster
grupos_2010 <- resultado_cluster_2010$grupos
sil_2010 <- resultado_cluster_2010$silhouette
caracterizacion_2010 <- resultado_cluster_2010$caracterizacion

# Mostrar resultados
cat(
  "Silhouette media 2010:",
  round(mean(sil_2010[, "sil_width"]), 3),
  "\n"
)

print(caracterizacion_2010, n = Inf)

# PCA y clustering demográfico - 1995 ####

# 1. PCA
resultado_pca_1995 <- pca_demografico(
  datos_1995,
  vars_demograficas
)

X_1995 <- resultado_pca_1995$datos_pca
matriz_correlaciones_1995 <- resultado_pca_1995$correlaciones
pca_1995 <- resultado_pca_1995$pca
coords_1995 <- resultado_pca_1995$coordenadas

# Mostrar resultados del PCA
print(round(matriz_correlaciones_1995, 2))
print(round(pca_1995$eig, 2))
print(round(pca_1995$var$coord[, 1:2], 2))
print(round(pca_1995$var$contrib[, 1:2], 2))


# 2. Evaluar cantidades de clusters
silhouette_1995 <- evaluar_clusters_demograficos(
  pca_1995,
  valores_k = 2:10
)

print(silhouette_1995, n = Inf)


# 3. Clustering definitivo con k = 2
resultado_cluster_1995 <- clustering_demografico(
  pca = pca_1995,
  datos = datos_1995,
  variables = vars_demograficas,
  k = 2
)

# Recuperar los resultados
hcpc_1995_k2 <- resultado_cluster_1995$hcpc
perfiles_1995 <- resultado_cluster_1995$perfiles
datos_1995_cluster <- resultado_cluster_1995$datos_cluster
grupos_1995 <- resultado_cluster_1995$grupos
sil_1995 <- resultado_cluster_1995$silhouette
caracterizacion_1995 <- resultado_cluster_1995$caracterizacion

# Mostrar resultados
cat(
  "Silhouette media 1995:",
  round(mean(sil_1995[, "sil_width"]), 3),
  "\n"
)

print(caracterizacion_1995, n = Inf)

# PCA y clustering demográfico - 1980 ####

# 1. PCA
resultado_pca_1980 <- pca_demografico(
  datos_1980,
  vars_demograficas
)

X_1980 <- resultado_pca_1980$datos_pca
matriz_correlaciones_1980 <- resultado_pca_1980$correlaciones
pca_1980 <- resultado_pca_1980$pca
coords_1980 <- resultado_pca_1980$coordenadas

# Mostrar resultados del PCA
print(round(matriz_correlaciones_1980, 2))
print(round(pca_1980$eig, 2))
print(round(pca_1980$var$coord[, 1:2], 2))
print(round(pca_1980$var$contrib[, 1:2], 2))


# 2. Evaluar cantidades de clusters
silhouette_1980 <- evaluar_clusters_demograficos(
  pca_1980,
  valores_k = 2:10
)

print(silhouette_1980, n = Inf)

# 3. Clustering definitivo con k = 2
resultado_cluster_1980 <- clustering_demografico(
  pca = pca_1980,
  datos = datos_1980,
  variables = vars_demograficas,
  k = 2
)

# Recuperar los resultados
hcpc_1980_k2 <- resultado_cluster_1980$hcpc
perfiles_1980 <- resultado_cluster_1980$perfiles
datos_1980_cluster <- resultado_cluster_1980$datos_cluster
grupos_1980 <- resultado_cluster_1980$grupos
sil_1980 <- resultado_cluster_1980$silhouette
caracterizacion_1980 <- resultado_cluster_1980$caracterizacion

# Mostrar resultados
cat(
  "Silhouette media 1980:",
  round(mean(sil_1980[, "sil_width"]), 3),
  "\n"
)

print(caracterizacion_1980, n = Inf)

# Control de comparabilidad entre los cuatro años

# Reunir las características demográficas de los clusters
control_perfiles <- bind_rows(
  caracterizacion_1980 |> mutate(anio = 1980),
  caracterizacion_1995 |> mutate(anio = 1995),
  caracterizacion_2010 |> mutate(anio = 2010),
  caracterizacion_2024 |> mutate(anio = 2024)
) |>
  select(anio, clust, poblacion_65_mas) |>
  mutate(clust = as.character(clust)) |>
  pivot_wider(
    names_from = clust,
    values_from = poblacion_65_mas,
    names_prefix = "cluster_"
  ) |>
  mutate(
    orden_esperado = cluster_1 < cluster_2
  ) |>
  arrange(anio)

print(control_perfiles)

# Verificar que el cluster 1 sea el menos envejecido
# y el cluster 2 el más envejecido en todos los períodos
stopifnot(all(control_perfiles$orden_esperado))

# Exportación de resultados del clustering demográfico

# 1. Calidad de las soluciones de clustering
calidad_clusters <- bind_rows(
  silhouette_1980 |> mutate(anio = 1980),
  silhouette_1995 |> mutate(anio = 1995),
  silhouette_2010 |> mutate(anio = 2010),
  silhouette_2024 |> mutate(anio = 2024)
) |>
  select(anio, k, silhouette_promedio, grupo_mas_chico) |>
  arrange(anio, k)

write_csv(
  calidad_clusters,
  "output/tablas/calidad_clusters_demograficos.csv"
)


# 2. Características demográficas de los clusters
caracterizacion_clusters <- bind_rows(
  caracterizacion_1980 |> mutate(anio = 1980),
  caracterizacion_1995 |> mutate(anio = 1995),
  caracterizacion_2010 |> mutate(anio = 2010),
  caracterizacion_2024 |> mutate(anio = 2024)
) |>
  select(anio, everything()) |>
  arrange(anio, clust)

write_csv(
  caracterizacion_clusters,
  "output/tablas/caracterizacion_clusters_demograficos.csv"
)


# 3. Asignación de cada país a un cluster
perfiles_paises <- bind_rows(
  datos_1980_cluster,
  datos_1995_cluster,
  datos_2010_cluster,
  datos_2024_cluster
) |>
  select(
    codigo_pais,
    pais,
    anio,
    clust,
    all_of(vars_demograficas)
  ) |>
  arrange(codigo_pais, anio)


# Comprobar que haya una observación por país y año
# y que estén presentes todos los países en los cuatro períodos

stopifnot(
  nrow(perfiles_paises) ==
    nrow(datos_1980) * length(anios_analisis),
  
  !anyDuplicated(
    perfiles_paises[c("codigo_pais", "anio")]
  ),
  
  !anyNA(perfiles_paises$clust)
)

write_csv(
  perfiles_paises,
  "output/tablas/perfiles_paises_demograficos.csv"
)


# 4. Varianza explicada por los componentes principales
varianza_pca <- bind_rows(
  as.data.frame(pca_1980$eig) |>
    rownames_to_column("componente") |>
    mutate(anio = 1980),
  
  as.data.frame(pca_1995$eig) |>
    rownames_to_column("componente") |>
    mutate(anio = 1995),
  
  as.data.frame(pca_2010$eig) |>
    rownames_to_column("componente") |>
    mutate(anio = 2010),
  
  as.data.frame(pca_2024$eig) |>
    rownames_to_column("componente") |>
    mutate(anio = 2024)
) |>
  select(anio, everything()) |>
  arrange(anio, componente)

write_csv(
  varianza_pca,
  "output/tablas/varianza_pca_demografico.csv"
)


# 5. Verificar que los archivos se hayan guardado
archivos_clustering <- c(
  "output/tablas/calidad_clusters_demograficos.csv",
  "output/tablas/caracterizacion_clusters_demograficos.csv",
  "output/tablas/perfiles_paises_demograficos.csv",
  "output/tablas/varianza_pca_demografico.csv"
)

print(file.exists(archivos_clustering))

# Análisis de outliers ####

# Leer los outliers detectados en 04_analisis_outliers_y_na.R
ruta_outliers <- "output/tablas/outliers_horizontales_wdi.csv"

# Comprobar que el diagnóstico general ya se haya ejecutado
stopifnot(file.exists(ruta_outliers))

# Seleccionar los outliers relevantes para nuestro clustering
outliers_cluster_demografico <- read_csv(
  ruta_outliers,
  show_col_types = FALSE
) |>
  filter(
    anio %in% anios_analisis,
    variable %in% vars_demograficas,
    codigo_pais %in% datos_1980$codigo_pais
  ) |>
  select(
    codigo_pais,
    pais,
    anio,
    variable,
    valor
  ) |>
  arrange(variable, anio, desc(valor))

# Resumir los outliers detectados
outliers_cluster_demografico |>
  count(anio, variable) |>
  print(n = Inf)

# Identificar países con outliers en más de una observación
outliers_cluster_demografico |>
  count(codigo_pais, pais, sort = TRUE) |>
  print(n = 20)

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

print(perfiles_comparados)

# Vemos que sí, se mantienen las caracteristicas de los grupos

# Ahora formamos trayectoria_paises, agregando el grupo a cada país, para ver 
# cuándo cambiaron

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

# Verificar que cada país tenga una trayectoria completa
stopifnot(
  nrow(trayectorias_paises) == nrow(datos_1980),
  !anyDuplicated(trayectorias_paises$pais),
  !anyNA(trayectorias_paises)
)

# Guardar las trayectorias de los países
write_csv(
  trayectorias_paises,
  "output/tablas/trayectorias_paises_demograficos.csv"
)

# Guardar la base utilizada en los cuatro cortes temporales
# para reutilizarla sin repetir la selección de países
base_cluster_cortes <- base_cluster_demografico |>
  filter(anio %in% anios_analisis)

write_csv(
  base_cluster_cortes,
  "auxiliar/base_cluster_demografico_cortes.csv"
)

# Verificar que ambos archivos se hayan guardado
stopifnot(
  file.exists("output/tablas/trayectorias_paises_demograficos.csv"),
  file.exists("auxiliar/base_cluster_demografico_cortes.csv")
)

print("Archivos auxiliares del clustering guardados correctamente.")

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

# Cuáles son los países que pasan entre perfiles? (de los 33)

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

# Exportación de resultados ####

# Guardamos la caracterización de los clusters de los cuatro momentos analizados
# para reutilizar estos resultados en los análisis longitudinales posteriores

perfiles_comparados |>
  mutate(
    perfil = if_else(
      clust == 1,
      "Joven y expansivo",
      "Transición avanzada"
    )
  ) |>
  write_csv("output/tablas/perfiles_clusters_1980_2024.csv")

# Prueba de sensibilidad (sin outliers) ####

# Si un país aparece como outlier al menos una vez en cualquiera de los cuatro años 
# o variables, lo excluimos de 1980, 1995, 2010 y 2024 para esta prueba de sensibilidad

# Identificamos todos los países que aparecen como outlier
# en al menos una variable y en al menos uno de los cuatro años
paises_outliers_total <- outliers_cluster_demografico |>
  distinct(codigo_pais)

paises_outliers_total

# Cantidad de países distintos que presentan al menos un outlier
nrow(paises_outliers_total)

# Cuántos países perdemos y cuántos quedan para la prueba de sensibilidad

# Cantidad de países de la muestra original
cantidad_paises_original <- nrow(datos_1980)

# Resumen de la exclusión de países con outliers
resumen_exclusion_outliers <- tibble(
  paises_originales = cantidad_paises_original,
  paises_excluidos = nrow(paises_outliers_total),
  paises_restantes = cantidad_paises_original -
    nrow(paises_outliers_total),
  porcentaje_excluido = round(
    100 * nrow(paises_outliers_total) /
      cantidad_paises_original, 2
  )
)

print(resumen_exclusion_outliers)

# Excluimos esos mismos países de los cuatro períodos

datos_1980_sin_outliers <- datos_1980 |>
  anti_join(paises_outliers_total, by = "codigo_pais")

datos_1995_sin_outliers <- datos_1995 |>
  anti_join(paises_outliers_total, by = "codigo_pais")

datos_2010_sin_outliers <- datos_2010 |>
  anti_join(paises_outliers_total, by = "codigo_pais")

datos_2024_sin_outliers <- datos_2024 |>
  anti_join(paises_outliers_total, by = "codigo_pais")

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

# Cargamos la función que calcula la silhouette asegurando que cada país
# tenga su cluster correctamente asignado
source("utils/calcular_silhouette.R")

# Aplicar el cálculo a los cuatro períodos
sil_1980_sin_outliers <- calcular_silhouette(
  hcpc_1980_sin_outliers, pca_1980_sin_outliers
)

sil_1995_sin_outliers <- calcular_silhouette(
  hcpc_1995_sin_outliers, pca_1995_sin_outliers
)

sil_2010_sin_outliers <- calcular_silhouette(
  hcpc_2010_sin_outliers, pca_2010_sin_outliers
)

sil_2024_sin_outliers <- calcular_silhouette(
  hcpc_2024_sin_outliers, pca_2024_sin_outliers
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

print(sensibilidad_cluster_outliers)

# Silhouette no cambia demasiado al sacar los países que tuvieron algún outlier.
# No hay evidencia de que los outliers estén afectando artificialmente los dos grupos

# Tabla resumen por año y variable 
outliers_cluster_demografico |>
  count(anio, variable)

outliers_cluster_demografico |>
  arrange(anio, variable, pais)

# Gráficos para PPT ####

# Silhouette media de la solución k = 2 por año
# Utilizamos los resultados calculados anteriormente

silhouette_resumen <- tibble(
  anio = factor(
    anios_analisis,
    levels = anios_analisis
  ),
  silhouette_media = c(
    mean(sil_1980[, "sil_width"]),
    mean(sil_1995[, "sil_width"]),
    mean(sil_2010[, "sil_width"]),
    mean(sil_2024[, "sil_width"])
  )
)

print(silhouette_resumen)


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
    y = "Silhouette media",
    caption = "Fuente: Banco Mundial (WDI). Elaboración propia."
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
  "output/graficos/silhouette_clusters.png",
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
    variable_1 = dplyr::recode(
      variable_1,
      poblacion_65_mas = "Población 65+",
      fecundidad = "Fecundidad",
      esperanza_vida = "Esperanza de vida",
      crecimiento_poblacional = "Crecimiento"
    ),
    variable_2 = dplyr::recode(
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
    fill = "r",
    caption = "Fuente: Banco Mundial (WDI). Elaboración propia."
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
  "output/graficos/correlaciones_2024.png",
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
  tab_source_note(source_note = "Fuente: Banco Mundial (WDI). Elaboración propia.") |>
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(11),
    data_row.padding = px(4)
  )

tabla_trayectorias

gtsave(
  tabla_trayectorias,
  "output/tablas/tabla_trayectorias.png",
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
  tab_source_note(source_note = "Fuente: Banco Mundial (WDI). Elaboración propia.") |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(11),
    data_row.padding = px(4)
  )

tabla_perfiles

gtsave(
  tabla_perfiles,
  "output/tablas/tabla_perfiles_demograficos.png",
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
    fill = NULL,
    caption = "Fuente: Banco Mundial (WDI). Elaboración propia."
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
  "output/graficos/evolucion_clusters.png",
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
  tab_source_note(source_note = "Fuente: Banco Mundial (WDI). Elaboración propia.") |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(11),
    data_row.padding = px(4)
  )

tabla_silhouette_clusters

gtsave(
  tabla_silhouette_clusters,
  "output/tablas/tabla_silhouette_por_cluster.png",
  zoom = 2
)


# ==============================================================================
# Control final de archivos generados
# ==============================================================================

archivos_generados <- c(
  "output/tablas/calidad_clusters_demograficos.csv",
  "output/tablas/caracterizacion_clusters_demograficos.csv",
  "output/tablas/perfiles_paises_demograficos.csv",
  "output/tablas/varianza_pca_demografico.csv",
  "output/tablas/trayectorias_paises_demograficos.csv",
  "output/tablas/tabla_trayectorias.png",
  "output/tablas/tabla_perfiles_demograficos.png",
  "output/graficos/silhouette_clusters.png",
  "output/graficos/correlaciones_2024.png",
  "output/graficos/evolucion_clusters.png",
  "output/tablas/tabla_silhouette_por_cluster.png",
  "output/tablas/perfiles_clusters_1980_2024.csv",
  "auxiliar/base_cluster_demografico_cortes.csv"
)

stopifnot(all(file.exists(archivos_generados)))

print("07 finalizado: todos los archivos fueron generados.")
