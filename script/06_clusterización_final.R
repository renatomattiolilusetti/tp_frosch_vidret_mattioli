# ==============================================================================
# Clusterización Final - cluster discreto para cada año
# ==============================================================================

# Paquetes 

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(zoo)

# Partimos de la base limpia

wdi_limpia <- read_csv("repo_wdi/wdi_limpia.csv")

# Intro: preparación ####
# Retomamos las variables de interés para armar los clusters

vars_demograficas <- c(
  "poblacion_65_mas",
  "fecundidad",
  "esperanza_vida",
  "crecimiento_poblacional"
)

base_demo <- wdi_limpia |>
  filter(between(anio, 1960, 2024)) |>
  select(pais, codigo_pais, anio, all_of(vars_demograficas))

# Diagnóstico de valores faltantes ####

# Cuántos NA hay por variable:

base_demo |>
  summarise(
    across(
      all_of(vars_demograficas),
      ~ sum(is.na(.))
    )
  )

# En qué años se concentran esos faltantes:

base_demo |>
  group_by(anio) |>
  summarise(
    across(
      all_of(vars_demograficas),
      ~ sum(is.na(.))
    )
  )

# Pasamos las variables a formato largo

na_demo <- base_demo |>
  pivot_longer(
    all_of(vars_demograficas),
    names_to = "variable",
    values_to = "valor"
  )

# Cuánto falta de cada variable:

na_demo |>
  summarise(
    n_na = sum(is.na(valor)),
    pct_na = round(mean(is.na(valor)) * 100, 2),
    .by = variable
  )

# En qué años están los faltantes:

na_demo |>
  filter(is.na(valor)) |>
  count(anio, variable)

# Qué países concentran los faltantes:

na_demo |>
  filter(is.na(valor)) |>
  count(pais, variable, sort = TRUE)

# 1960 tiene el 100% de faltantes en crecimiento poblacional. Decidimos entonces
# arrancar desde 1961. Corregimos base

base_demo <- wdi_limpia |>
  filter(between(anio, 1961, 2024)) |>
  select(pais, codigo_pais, anio, all_of(vars_demograficas))

na_demo <- base_demo |>
  pivot_longer(
    all_of(vars_demograficas),
    names_to = "variable",
    values_to = "valor"
  )

na_demo |>
  summarise(
    n_na = sum(is.na(valor)),
    pct_na = round(mean(is.na(valor)) * 100, 2),
    .by = variable
  )

# De que países vienen esos NA

na_demo |>
  filter(is.na(valor)) |>
  count(pais, variable, sort = TRUE)

# En qué años se concentran esos faltantes

na_demo |>
  filter(is.na(valor)) |>
  summarise(
    n = n(),
    anios = paste(anio, collapse = ", "),
    .by = c(pais, variable)
  ) |>
  arrange(desc(n))

# Mirando más precisamente Cisjordania y Gaza:

na_demo |>
  filter(pais == "West Bank and Gaza", is.na(valor)) |>
  summarise(
    inicio = min(anio),
    fin = max(anio),
    n = n(),
    consecutivos = n == fin - inicio + 1,
    .by = variable
  )

# Como tenemos tres décadas sin datos para las variables de crecimiento poblacional, 
# fecundidad y esperanza de vida, exlcuimos al país del análisis (sería perder 1 país
# de 195)

base_demo <- base_demo |>
  filter(pais != "West Bank and Gaza")

# Qué hacemos con los otros faltantes?

base_demo |>
  pivot_longer(
    all_of(vars_demograficas),
    names_to = "variable",
    values_to = "valor"
  ) |>
  filter(is.na(valor)) |>
  select(pais, anio, variable) |>
  arrange(pais, variable, anio)

# Miramos los valores alrededor de esos NA

base_demo |>
  pivot_longer(
    all_of(vars_demograficas),
    names_to = "variable",
    values_to = "valor"
  ) |>
  arrange(pais, variable, anio) |>
  group_by(pais, variable) |>
  filter(any(is.na(valor))) |>
  filter(
    is.na(valor) |
      lag(is.na(valor), default = FALSE) |
      lead(is.na(valor), default = FALSE)
  ) |>
  ungroup()

# Miramos mas precisamente Seychelles y Luxemburgo

wdi_limpia |>
  filter(
    (pais == "Luxembourg" & anio %in% 1960:1964) |
      (pais == "Seychelles" & anio %in% 2020:2024)
  ) |>
  select(pais, anio, fecundidad, crecimiento_poblacional)

# Interpolamos para todos los casos

base_demo <- wdi_limpia |>
  filter(
    between(anio, 1960, 2024),
    pais != "West Bank and Gaza"
  ) |>
  select(pais, codigo_pais, anio, all_of(vars_demograficas)) |>
  arrange(pais, anio) |>
  group_by(pais) |>
  mutate(
    across(
      all_of(vars_demograficas),
      ~ na.approx(.x, x = anio, na.rm = FALSE)
    )
  ) |>
  ungroup() |>
  filter(anio >= 1961)

# Chequeo de que la interpolación esté bien:

base_demo |>
  summarise(across(all_of(vars_demograficas), ~ sum(is.na(.x))))

# Vemos resultados de la imputación:

wdi_limpia |>
  filter(
    between(anio, 1960, 2024),
    pais != "West Bank and Gaza"
  ) |>
  select(pais, codigo_pais, anio, all_of(vars_demograficas)) |>
  pivot_longer(
    all_of(vars_demograficas),
    names_to = "variable",
    values_to = "original"
  ) |>
  arrange(pais, variable, anio) |>
  group_by(pais, variable) |>
  mutate(
    anterior = zoo::na.locf(original, na.rm = FALSE),
    siguiente = zoo::na.locf(original, fromLast = TRUE, na.rm = FALSE)
  ) |>
  ungroup() |>
  filter(anio >= 1961, is.na(original)) |>
  left_join(
    base_demo |>
      pivot_longer(
        all_of(vars_demograficas),
        names_to = "variable",
        values_to = "imputado"
      ),
    by = c("pais", "codigo_pais", "anio", "variable")
  ) |>
  select(pais, anio, variable, anterior, imputado, siguiente)

# Clusters a partir de PCA común ####

# Clusters: ¿Cuándo pasan los países de un perfil demográfico joven a uno más avanzado 
# si usamos la misma clasificación para todo el período 1961–2024?

# Armamos un PCA común:

pca_demo <- PCA(
  base_demo |> select(all_of(vars_demograficas)),
  scale.unit = TRUE,
  ncp = 4,
  graph = FALSE
)

pca_demo$eig

round(pca_demo$var$coord[, 1:2], 2)

# Para ver cuántos perfiles sugiere el clustering conjunto:

hcpc_demo <- HCPC(
  pca_demo,
  nb.clust = 0,
  min = 2,
  max = 5,
  kk = 500,
  graph = FALSE
)

table(hcpc_demo$data.clust$clust)

hcpc_demo$call$t$nb.clust

# Evaluamos qué tan bien separados están los cuatro clusters obtenidos. Usamos el 
# índice silhouette sobre una muestra de observaciones para evitar un cálculo innecesariamente pesado.

set.seed(42)

muestra <- sample(nrow(pca_demo$ind$coord), 2000)

sil <- silhouette(
  as.numeric(hcpc_demo$data.clust$clust[muestra]),
  dist(pca_demo$ind$coord[muestra, 1:2])
)

mean(sil[, "sil_width"])

# Qué agrupación separa mejor las observaciones

# Recalculamos el índice silhouette usando los cuatro componentes del PCA,
# para evaluar los clusters en el mismo espacio en el que fueron construidos.

coord_muestra <- pca_demo$ind$coord[muestra, 1:4]
dist_muestra <- dist(coord_muestra)

comparacion_k <- map_dfr(2:5, \(k) {
  
  grupos <- kmeans(
    coord_muestra,
    centers = k,
    nstart = 25
  )$cluster
  
  tibble(
    k = k,
    silhouette = mean(
      silhouette(grupos, dist_muestra)[, "sil_width"]
    )
  )
})

comparacion_k

# Fijamos dos clusters porque fue la solución con mayor silhouette promedio.

hcpc_demo <- HCPC(
  pca_demo,
  nb.clust = 2,
  kk = 500,
  graph = FALSE
)

table(hcpc_demo$data.clust$clust)

# Asociamos a cada observación país-año el cluster asignado

demo_cluster <- base_demo |>
  mutate(cluster = hcpc_demo$data.clust$clust)

# Comparamos los valores promedio de las cuatro variables demográficas entre clusters.

demo_cluster |>
  summarise(
    across(all_of(vars_demograficas), ~ round(mean(.x), 2)),
    .by = cluster
  )

# Nombramos los dos clusters según las características observadas y conservamos 
# el número de cluster y agregamos una etiqueta interpretable para seguir las trayectorias.

demo_cluster <- demo_cluster |>
  mutate(
    perfil = if_else(
      cluster == 1,
      "Joven y expansivo",
      "Transición avanzada"
    )
  )

# Ver cómo cambia la cantidad de países en cada perfil a través de los años

demo_cluster |>
  filter(anio %in% c(1961, 1970, 1980, 1990, 2000, 2010, 2020, 2024)) |>
  count(anio, perfil) |>
  pivot_wider(
    names_from = perfil,
    values_from = n
  )

# Qué países hicieron ese cruce entre grupos y en qué año? Fue masomenos simultáneo ?
# Chequeamos primero cuántas veces cambia de perfil cada país entre 1961 y 2024.
# Esto permite distinguir transiciones únicas de trayectorias.

trayectorias <- demo_cluster |>
  arrange(pais, anio) |>
  summarise(
    perfil_inicial = first(perfil),
    perfil_final = last(perfil),
    cambios = sum(perfil != lag(perfil), na.rm = TRUE),
    .by = pais
  )

trayectorias |>
  count(perfil_inicial, perfil_final, cambios)

# Resumimos las trayectorias en cuatro categorías: permanencia, transición única 
# hacia el perfil avanzado y recorridos no lineales.

trayectorias |>
  mutate(
    tipo = case_when(
      cambios == 0 & perfil_inicial == "Joven y expansivo" ~ "Siempre joven",
      cambios == 0 & perfil_inicial == "Transición avanzada" ~ "Siempre avanzado",
      cambios == 1 & perfil_inicial == "Joven y expansivo" &
        perfil_final == "Transición avanzada" ~ "Transición joven a avanzado",
      TRUE ~ "Trayectoria no lineal"
    )
  ) |>
  count(tipo, sort = TRUE)

# Identificamos el primer año en que cada país con transición limpia
# pasa del perfil joven y expansivo al perfil de transición avanzada.

transiciones <- demo_cluster |>
  filter(pais %in% paises_transicion) |>
  summarise(
    anio_transicion = min(anio[perfil == "Transición avanzada"]),
    .by = pais
  ) |>
  arrange(anio_transicion)

print(transiciones, n = Inf)

# Esos cruces ocurrieron más o menos al mismo tiempo o están muy dispersos?

transiciones |>
  summarise(
    primera = min(anio_transicion),
    mediana = median(anio_transicion),
    ultima = max(anio_transicion)
  )

# transiciones por década:

transiciones |>
  count(decada = floor(anio_transicion / 10) * 10)

# Resumimos la distribución de los años de transición para medir cuán extendido
# fue el proceso entre los países con un único cambio hacia el perfil avanzado.

transiciones |>
  summarise(
    primero = min(anio_transicion),
    p25 = quantile(anio_transicion, 0.25),
    mediana = median(anio_transicion),
    p75 = quantile(anio_transicion, 0.75),
    ultimo = max(anio_transicion)
  )

# Clusters discretos: sumamos 1961 y 1970 para la comparación con los otros momentos ####

# Análisis de perfiles demográficos: sumamos ahora 1961 y 1970 para luego comparar
# con los que ya teníamos. Replicamos la metodología.

datos_1961 <- base_demo |>
  filter(anio == 1961)

datos_1970 <- base_demo |>
  filter(anio == 1970)

# Construimos el PCA de 1961 con las mismas cuatro variables y la misma estandarización
# utilizada en los otros momentos temporales.

X_1961 <- datos_1961 |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_1961) <- datos_1961$pais

pca_1961 <- PCA(
  X_1961,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)

pca_1961$eig
pca_1961$var$coord[, 1:2]
pca_1961$var$contrib[, 1:2]

# Comparamos distintas cantidades de clusters en 1961 mediante silhouette promedio.

coords_1961 <- pca_1961$ind$coord[, 1:2]

silhouette_1961 <- map_dfr(2:10, \(k) {
  
  hcpc_k <- HCPC(
    pca_1961,
    nb.clust = k,
    consol = TRUE,
    graph = FALSE
  )
  
  grupos_k <- as.integer(hcpc_k$data.clust$clust)
  
  tibble(
    k = k,
    silhouette_promedio = mean(
      silhouette(grupos_k, dist(coords_1961))[, "sil_width"]
    ),
    grupo_mas_chico = min(table(grupos_k))
  )
})

silhouette_1961

# Repetimos el mismo PCA para 1970.

X_1970 <- datos_1970 |>
  select(all_of(vars_demograficas)) |>
  as.data.frame()

rownames(X_1970) <- datos_1970$pais

pca_1970 <- PCA(
  X_1970,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)

pca_1970$eig
pca_1970$var$coord[, 1:2]
pca_1970$var$contrib[, 1:2]

# Comparamos distintas cantidades de clusters en 1970.

coords_1970 <- pca_1970$ind$coord[, 1:2]

silhouette_1970 <- map_dfr(2:10, \(k) {
  
  hcpc_k <- HCPC(
    pca_1970,
    nb.clust = k,
    consol = TRUE,
    graph = FALSE
  )
  
  grupos_k <- as.integer(hcpc_k$data.clust$clust)
  
  tibble(
    k = k,
    silhouette_promedio = mean(
      silhouette(grupos_k, dist(coords_1970))[, "sil_width"]
    ),
    grupo_mas_chico = min(table(grupos_k))
  )
})

silhouette_1970

# Con k=3, el grupo más chico tiene 3 países en 1961 y 4 en 1970. En cambio, con k=2,
# el grupo más chico tiene 53 y 61 países. Entonces, nos quedamos con k=2 en ambos años.

# Fijamos dos clusters en 1961 y 1970.

hcpc_1961_k2 <- HCPC(
  pca_1961,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

hcpc_1970_k2 <- HCPC(
  pca_1970,
  nb.clust = 2,
  consol = TRUE,
  graph = FALSE
)

# Asociamos el cluster a cada país y resumimos las características promedio
# de los dos perfiles en 1961 y 1970.

perfiles_1961 <- hcpc_1961_k2$data.clust |>
  as.data.frame() |>
  rownames_to_column("pais") |>
  select(pais, clust)

perfiles_1970 <- hcpc_1970_k2$data.clust |>
  as.data.frame() |>
  rownames_to_column("pais") |>
  select(pais, clust)

datos_1961_cluster <- datos_1961 |>
  left_join(perfiles_1961, by = "pais")

datos_1970_cluster <- datos_1970 |>
  left_join(perfiles_1970, by = "pais")

# Caracterizamos los dos clusters de cada año para identificar cuál corresponde
# al perfil joven y cuál al perfil de transición avanzada.

datos_1961_cluster |>
  summarise(
    n = n(),
    poblacion_65_mas = mean(poblacion_65_mas),
    fecundidad = mean(fecundidad),
    esperanza_vida = mean(esperanza_vida),
    crecimiento_poblacional = mean(crecimiento_poblacional),
    .by = clust
  )

datos_1970_cluster |>
  summarise(
    n = n(),
    poblacion_65_mas = mean(poblacion_65_mas),
    fecundidad = mean(fecundidad),
    esperanza_vida = mean(esperanza_vida),
    crecimiento_poblacional = mean(crecimiento_poblacional),
    .by = clust
  )

# Comparaciones finales de los clusters en momentos ####

# Recuperamos la caracterización de los clusters obtenida en el análisis previo 
# para integrarla con los nuevos cortes de 1961 y 1970.

perfiles_1980_2024 <- read_csv(
  "auxiliar/perfiles_clusters_1980_2024.csv"
)

# Caracterizamos los clusters de 1961 y 1970 con las mismas variables utilizadas en 
# los cuatro momentos del análisis original.

perfiles_1961_1970 <- bind_rows(
  datos_1961_cluster,
  datos_1970_cluster
) |>
  summarise(
    n = n(),
    poblacion_65_mas = mean(poblacion_65_mas),
    fecundidad = mean(fecundidad),
    esperanza_vida = mean(esperanza_vida),
    crecimiento_poblacional = mean(crecimiento_poblacional),
    .by = c(anio, clust)
  ) |>
  mutate(
    perfil = if_else(
      clust == 1,
      "Joven y expansivo",
      "Transición avanzada"
    )
  )

# Unificamos el tipo de la variable de cluster para poder combinar ambas bases.

perfiles_1961_1970 <- perfiles_1961_1970 |>
  mutate(clust = as.integer(as.character(clust)))

perfiles_1980_2024 <- perfiles_1980_2024 |>
  mutate(clust = as.integer(clust))

# Juntamos los seis momentos en una única base para comparar los cambios en las 
# características de los perfiles. 

evolucion_perfiles <- bind_rows(
  perfiles_1961_1970,
  perfiles_1980_2024
) |>
  arrange(anio, clust)

evolucion_perfiles

# Gráficos ####

