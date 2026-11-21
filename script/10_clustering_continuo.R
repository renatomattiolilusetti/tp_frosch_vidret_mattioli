# ==============================================================================
# Clusterización final: cluster continuo 1961-2024 y clusters por año (1961 y 1970)
# ==============================================================================

# Paquetes 
library(tidyverse)
library(FactoMineR)
library(cluster)
library(zoo)
library(gt)

# Funciones propias
source("utils/pca_demografico.R")
source("utils/evaluar_clusters_demograficos.R")
source("utils/clustering_demografico.R")
source("utils/theme_tp.R")

# Fijar semilla: HCPC con kk = 500 arranca con un k-means, que es aleatorio
set.seed(42)

# Crear las carpetas de resultados si no existen
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

# Partimos de la base limpia
wdi_limpia <- read_csv("input/wdi_limpia.csv")

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

# Cuántos NA hay por variable
base_demo |>
  summarise(
    across(
      all_of(vars_demograficas),
      ~ sum(is.na(.))
    )
  )

# En qué años se concentran esos faltantes
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

# Cuánto falta de cada variable
na_demo |>
  summarise(
    n_na = sum(is.na(valor)),
    pct_na = round(mean(is.na(valor)) * 100, 2),
    .by = variable
  )

# En qué años están los faltantes
na_demo |>
  filter(is.na(valor)) |>
  count(anio, variable)

# Qué países concentran los faltantes
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

# Mirando más precisamente Cisjordania y Gaza
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
# fecundidad y esperanza de vida, excluimos al país del análisis (sería perder 1 país
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

# Chequeo de que la interpolación esté bien
base_demo |>
  summarise(across(all_of(vars_demograficas), ~ sum(is.na(.x))))

# Guardamos la base interpolada para usarla en otros scripts
write_csv(base_demo, "output/tablas/base_demo_interpolada.csv")

# Vemos resultados de la imputación
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

# Armamos un PCA común
pca_demo <- PCA(
  base_demo |> select(all_of(vars_demograficas)),
  scale.unit = TRUE,
  ncp = 4,
  graph = FALSE
)

pca_demo$eig

round(pca_demo$var$coord[, 1:2], 2)

# Para ver cuántos perfiles sugiere el clustering conjunto
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
# índice silhouette sobre una muestra de observaciones para evitar un cálculo innecesariamente pesado
set.seed(42)

muestra <- sample(nrow(pca_demo$ind$coord), 2000)

sil <- silhouette(
  as.numeric(hcpc_demo$data.clust$clust[muestra]),
  dist(pca_demo$ind$coord[muestra, 1:2])
)

mean(sil[, "sil_width"])

# Qué agrupación separa mejor las observaciones?

# Recalculamos el índice silhouette usando los cuatro componentes del PCA,
# para evaluar los clusters en el mismo espacio en el que fueron construidos
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

# Fijamos dos clusters porque fue la solución con mayor silhouette promedio
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

# Comparamos los valores promedio de las cuatro variables demográficas entre clusters
demo_cluster |>
  summarise(
    across(all_of(vars_demograficas), ~ round(mean(.x), 2)),
    .by = cluster
  )

# Nombramos los dos clusters según las características observadas y conservamos 
# el número de cluster y agregamos una etiqueta interpretable para seguir las trayectorias
demo_cluster <- demo_cluster |>
  mutate(
    perfil = if_else(
      cluster == 1,
      "Joven y expansivo",
      "Transición avanzada"
    )
  )

# Verificamos que el cluster 1 sea efectivamente el menos envejecido
control_perfil_continuo <- demo_cluster |>
  summarise(media_65 = mean(poblacion_65_mas), .by = perfil)

stopifnot(
  control_perfil_continuo$media_65[control_perfil_continuo$perfil == "Joven y expansivo"] <
    control_perfil_continuo$media_65[control_perfil_continuo$perfil == "Transición avanzada"]
)

# Guardamos la clasificación país-año del cluster continuo
write_csv(demo_cluster, "output/tablas/demo_cluster_continuo.csv")

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
# Esto permite distinguir transiciones únicas de trayectorias
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
# hacia el perfil avanzado y recorridos no lineales. Ahora lo guardamos para usarlo después
trayectorias <- trayectorias |>
  mutate(
    tipo = case_when(
      cambios == 0 & perfil_inicial == "Joven y expansivo" ~ "Siempre joven",
      cambios == 0 & perfil_inicial == "Transición avanzada" ~ "Siempre avanzado",
      cambios == 1 & perfil_inicial == "Joven y expansivo" &
        perfil_final == "Transición avanzada" ~ "Transición joven a avanzado",
      TRUE ~ "Trayectoria no lineal"
    )
  )

trayectorias |>
  count(tipo, sort = TRUE)

# Guardamos los países que hicieron una única transición de joven a avanzado
paises_transicion <- trayectorias |>
  filter(tipo == "Transición joven a avanzado") |>
  pull(pais)

# Identificamos el primer año en que cada país con transición limpia
# pasa del perfil joven y expansivo al perfil de transición avanzada
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

# transiciones por década
transiciones |>
  count(decada = floor(anio_transicion / 10) * 10)

# Resumimos la distribución de los años de transición para medir cuán extendido
# fue el proceso entre los países con un único cambio hacia el perfil avanzado
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
# con los que ya teníamos. Replicamos la metodología
datos_1961 <- base_demo |>
  filter(anio == 1961)

datos_1970 <- base_demo |>
  filter(anio == 1970)

# Construimos el PCA de 1961 con las mismas cuatro variables y la misma estandarización
# utilizada en los otros momentos temporales
pca_1961 <- pca_demografico(datos_1961, vars_demograficas)$pca

pca_1961$eig
pca_1961$var$coord[, 1:2]
pca_1961$var$contrib[, 1:2]

# Comparamos distintas cantidades de clusters en 1961 mediante silhouette promedio
silhouette_1961 <- evaluar_clusters_demograficos(pca_1961)

silhouette_1961

# Repetimos el mismo PCA para 1970
pca_1970 <- pca_demografico(datos_1970, vars_demograficas)$pca

pca_1970$eig
pca_1970$var$coord[, 1:2]
pca_1970$var$contrib[, 1:2]

# Comparamos distintas cantidades de clusters en 1970
silhouette_1970 <- evaluar_clusters_demograficos(pca_1970)

silhouette_1970

# Con k=3, el grupo más chico tiene 3 países en 1961 y 4 en 1970. En cambio, con k=2,
# el grupo más chico tiene 53 y 61 países. Entonces, nos quedamos con k=2 en ambos años.

# Fijamos dos clusters en 1961 y 1970 y le asociamos a cada país su cluster
datos_1961_cluster <- clustering_demografico(pca_1961, datos_1961, vars_demograficas)$datos_cluster
datos_1970_cluster <- clustering_demografico(pca_1970, datos_1970, vars_demograficas)$datos_cluster

# Caracterizamos los dos clusters de cada año para identificar cuál corresponde
# al perfil joven y cuál al perfil de transición avanzada
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
# para integrarla con los nuevos cortes de 1961 y 1970
perfiles_1980_2024 <- read_csv(
  "output/tablas/perfiles_clusters_1980_2024.csv"
)

# Caracterizamos los clusters de 1961 y 1970 con las mismas variables utilizadas en 
# los cuatro momentos del análisis original
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

# Unificamos el tipo de la variable de cluster para poder combinar ambas bases
perfiles_1961_1970 <- perfiles_1961_1970 |>
  mutate(clust = as.integer(as.character(clust)))

perfiles_1980_2024 <- perfiles_1980_2024 |>
  mutate(clust = as.integer(clust))

# Juntamos los seis momentos en una única base para comparar los cambios en las 
# características de los perfiles
evolucion_perfiles <- bind_rows(
  perfiles_1961_1970,
  perfiles_1980_2024
) |>
  arrange(anio, clust)

evolucion_perfiles

# Verificamos que en los seis momentos el cluster 1 sea el menos envejecido
control_evolucion <- evolucion_perfiles |>
  select(anio, clust, poblacion_65_mas) |>
  pivot_wider(names_from = clust, values_from = poblacion_65_mas, names_prefix = "cluster_")

stopifnot(all(control_evolucion$cluster_1 < control_evolucion$cluster_2))

# Guardamos la evolución de los perfiles en los seis momentos
write_csv(evolucion_perfiles, "output/tablas/evolucion_perfiles.csv")

# Gráficos ####

# Gráfico comparativo de las trayectorias (1961 contra 2024) ####

# Nos quedamos con 1961 y 2024 y armamos una categoría que combine perfil y año
antes_despues <- evolucion_perfiles |>
  filter(anio %in% c(1961, 2024)) |>
  mutate(
    perfil = if_else(clust == 1, "Perfil joven", "Perfil avanzado"),
    perfil = factor(perfil, levels = c("Perfil joven", "Perfil avanzado")),
    grupo = paste(perfil, anio),
    grupo = factor(
      grupo,
      levels = c(
        "Perfil joven 1961",
        "Perfil joven 2024",
        "Perfil avanzado 1961",
        "Perfil avanzado 2024"
      )
    )
  ) |>
  select(perfil, anio, grupo, fecundidad, esperanza_vida, poblacion_65_mas, crecimiento_poblacional) |>
  pivot_longer(
    c(fecundidad, esperanza_vida, poblacion_65_mas, crecimiento_poblacional),
    names_to = "variable",
    values_to = "valor"
  ) |>
  mutate(
    variable = recode(
      variable,
      fecundidad = "Fecundidad (hijos por mujer)",
      esperanza_vida = "Esperanza de vida (años)",
      poblacion_65_mas = "Población de 65+ (% del total)",
      crecimiento_poblacional = "Crecimiento poblacional (% anual)"
    ),
    variable = factor(
      variable,
      levels = c(
        "Fecundidad (hijos por mujer)",
        "Esperanza de vida (años)",
        "Población de 65+ (% del total)",
        "Crecimiento poblacional (% anual)"
      )
    ),
    etiqueta = format(round(valor, 1), decimal.mark = ",", nsmall = 1, trim = TRUE)
  )

# Chequeamos los números
antes_despues

# Claro = 1961, oscuro = 2024, con los mismos azul y naranja de los otros gráficos
colores_antes_despues <- c(
  "Perfil joven 1961" = "#8CC2E6",
  "Perfil joven 2024" = "#0072B2",
  "Perfil avanzado 1961" = "#F2CF80",
  "Perfil avanzado 2024" = "#E69F00"
)

grafico_antes_despues <- ggplot(antes_despues, aes(x = perfil, y = valor, fill = grupo)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.75
  ) +
  geom_text(
    aes(label = etiqueta),
    position = position_dodge(width = 0.8),
    vjust = -0.5, size = 3.6, fontface = "bold", color = "#19354D"
  ) +
  facet_wrap(~ variable, nrow = 2, scales = "free_y") +
  scale_fill_manual(values = colores_antes_despues, name = NULL) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "Comparación de las características de las trayectorias:\nsituación del perfil joven y del avanzado en 1961 contra 2024",
    subtitle = "Promedio de cada perfil en 1961 y en 2024",
    x = NULL,
    y = NULL,
    caption = "Fuente: Banco Mundial (WDI). Elaboración propia."
  ) +
  theme_tp() +
  theme(
    strip.text = element_text(
      face = "bold", color = "#19354D", size = 11.5,
      margin = margin(b = 6)
    ),
    panel.spacing.x = unit(20, "pt"),
    panel.spacing.y = unit(14, "pt"),
    axis.text.x = element_text(color = "#19354D", size = 11, face = "bold"),
    axis.text.y = element_text(color = "#677887", size = 9.5),
    legend.text = element_text(size = 10.5, color = "#38536B")
  )

grafico_antes_despues

# Lo guardamos
ggsave(
  filename = "output/graficos/comparacion_caracteristicas_de_las_trayectorias.jpg",
  plot = grafico_antes_despues,
  width = 24,
  height = 17,
  units = "cm",
  dpi = 300,
  bg = "white",
  quality = 95
)

# Gráfico 2: cambios en la pertenencia al grupo ####

# Contamos cuántos países hay en cada perfil en cada año y lo pasamos a porcentaje
composicion <- demo_cluster |>
  count(anio, perfil) |>
  mutate(
    pct = n / sum(n) * 100,
    .by = anio
  )

# Buscamos el año desde el cual los avanzados son mayoría y no dejan de serlo
mayoria_avanzada <- composicion |>
  filter(perfil == "Transición avanzada") |>
  arrange(anio) |>
  mutate(mayoria = pct > 50)

anio_cruce <- max(mayoria_avanzada$anio[!mayoria_avanzada$mayoria]) + 1

anio_cruce

colores_perfil <- c(
  "Joven y expansivo" = "#0072B2",
  "Transición avanzada" = "#E69F00"
)

grafico_evolucion_perfiles <- ggplot(composicion, aes(x = anio, y = pct, color = perfil)) +
  geom_hline(yintercept = 50, color = "#B9C6D0", linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept = anio_cruce, color = "#19354D", linewidth = 0.5) +
  geom_line(linewidth = 1.4) +
  annotate(
    "text", x = anio_cruce + 1, y = 95,
    label = paste0(anio_cruce, ": los avanzados\npasan a ser mayoría"),
    hjust = 0, vjust = 1, size = 3.8, fontface = "bold",
    color = "#19354D", lineheight = 1.05
  ) +
  scale_color_manual(
    values = colores_perfil,
    labels = c("Perfil demográfico joven", "Transición avanzada"),
    name = NULL
  ) +
  scale_x_continuous(breaks = c(1961, 1970, 1980, 1990, 2000, 2010, 2024)) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = \(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    title = paste0("Perfiles comparados: desde ", anio_cruce, " la mayoría de los países\nse asocia con el perfil de transición avanzada"),
    subtitle = "Porcentaje de países en cada perfil, 1961-2024",
    x = NULL,
    y = "Países (%)",
    caption = "Fuente: Banco Mundial (WDI). Elaboración propia. Perfiles: PCA y cluster jerárquico comunes a todo el período."
  ) +
  theme_tp() +
  theme(
    axis.text = element_text(color = "#677887", size = 10),
    axis.title.y = element_text(color = "#38536B", size = 10.5, margin = margin(r = 8)),
    axis.line.x = element_line(color = "#B9C6D0", linewidth = 0.4),
    legend.key.width = unit(1, "cm")
  )

grafico_evolucion_perfiles

ggsave(
  filename = "output/graficos/perfiles_clusters_comparados.jpg",
  plot = grafico_evolucion_perfiles,
  width = 24,
  height = 14,
  units = "cm",
  dpi = 300,
  bg = "white",
  quality = 95
)

# Tabla de la evolución de los perfiles con los seis momentos ####

tabla_evolucion <- evolucion_perfiles |>
  select(anio, clust, n, poblacion_65_mas, fecundidad, esperanza_vida, crecimiento_poblacional) |>
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

tabla_evolucion

# Guardamos
gtsave(
  tabla_evolucion,
  "output/tablas/tabla_evolucion_perfiles.png",
  zoom = 2
)

# Control final de archivos generados ####

archivos_generados <- c(
  "output/tablas/base_demo_interpolada.csv",
  "output/tablas/demo_cluster_continuo.csv",
  "output/tablas/evolucion_perfiles.csv",
  "output/graficos/comparacion_caracteristicas_de_las_trayectorias.jpg",
  "output/graficos/perfiles_clusters_comparados.jpg",
  "output/tablas/tabla_evolucion_perfiles.png"
)

stopifnot(all(file.exists(archivos_generados)))

print("10 finalizado: todos los archivos fueron generados.")