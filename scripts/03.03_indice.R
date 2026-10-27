# ==============================================================================
# Análisis de capacidad de respuesta
# ==============================================================================

#library(tidyverse)
#library(FactoMineR)
#library(factoextra)
#library(cluster)

# Podemos armar un indicador de "capacidad de respuesta sanitaria" de cada país?
# Qué datos tenemos en capacidad sanitaria y capacidad económica?
# Variables:
# UHC: como medida de capacidad sanitaria: que mide qué tan ampliamente 
# disponibles/cubiertos están los servicios esenciales de salud.
# Gasto bolsillo salud: como % del gasto corriente total en salud que es pagado 
# directamente por los hogares.
# PIB_per_capita: para ver qué recursos económicos tiene para sostener esa respuesta

wdi_limpia <- read_csv("repo_wdi/wdi_limpia.csv")

# Partimos de los 195 países trabajados anteriormente sin agregados ni territorios no
# soberanos. Creamos una base específica para este análisis. 

base_capacidad_respuesta <- wdi_limpia |>
  filter(anio >= 2000, anio <= 2023) |>
  select(
    pais,
    codigo_pais,
    anio,
    indice_cobertura_salud,
    gasto_bolsillo_salud,
    pib_per_capita_ppa,
    poblacion_65_mas
  ) |>
  arrange(pais, anio)

# Chequeamos que el universo de países sea el adecuado

base_capacidad_respuesta |>
  distinct(codigo_pais) |>
  nrow()

# Miramos la cobertura de las tres variables

cobertura_capacidad <- base_capacidad_respuesta |>
  group_by(anio) |>
  summarise(
    paises_cobertura_salud =
      sum(!is.na(indice_cobertura_salud)),
    
    paises_gasto_bolsillo =
      sum(!is.na(gasto_bolsillo_salud)),
    
    paises_pib =
      sum(!is.na(pib_per_capita_ppa)),
    
    paises_envejecimiento =
      sum(!is.na(poblacion_65_mas)),
    
    paises_capacidad_completos =
      sum(
        !is.na(indice_cobertura_salud) &
          !is.na(gasto_bolsillo_salud) &
          !is.na(pib_per_capita_ppa)
      ),
    
    paises_todo_completo =
      sum(
        !is.na(indice_cobertura_salud) &
          !is.na(gasto_bolsillo_salud) &
          !is.na(pib_per_capita_ppa) &
          !is.na(poblacion_65_mas)
      ),
    
    .groups = "drop"
  )

# Qué año nos deja más países completos?

cobertura_capacidad |>
  arrange(desc(paises_capacidad_completos))

# Si bien los más completos son del 2013-2021, buscamos datos actuales para responder
# la pregunta. Entonces nos quedamos con 2023.

faltantes_2023 <- base_capacidad_respuesta |>
  filter(anio == 2023) |>
  mutate(
    falta_salud = is.na(indice_cobertura_salud),
    falta_gasto_bolsillo = is.na(gasto_bolsillo_salud),
    falta_pib = is.na(pib_per_capita_ppa),
    falta_envejecimiento = is.na(poblacion_65_mas),
    
    completo_capacidad =
      !falta_salud &
      !falta_gasto_bolsillo &
      !falta_pib,
    
    completo_total =
      completo_capacidad &
      !falta_envejecimiento
  )

# Qué paises quedan afuera y cuáles son?

faltantes_2023 |>
  summarise(
    total_paises = n(),
    completos_capacidad = sum(completo_capacidad),
    completos_total = sum(completo_total),
    falta_salud = sum(falta_salud),
    falta_gasto_bolsillo = sum(falta_gasto_bolsillo),
    falta_pib = sum(falta_pib),
    falta_envejecimiento = sum(falta_envejecimiento)
  )


# Estos valores faltantes, qué tipo de missing son?
# Buscamos diagnosticar su tipo:

faltantes_2023 <- faltantes_2023 |>
  left_join(
    wdi_limpia |>
      filter(anio == 2023) |>
      select(
        codigo_pais,
        region,
        ingreso
      ),
    by = "codigo_pais"
  )

faltantes_2023 |>
  count(ingreso, completo_capacidad)

faltantes_2023 |>
  count(region, completo_capacidad)

# Analizando por %

faltantes_2023 |>
  group_by(ingreso) |>
  summarise(
    paises = n(),
    incompletos = sum(!completo_capacidad),
    porcentaje_incompletos = round(100 * mean(!completo_capacidad), 1)
  )

faltantes_2023 |>
  group_by(region) |>
  summarise(
    paises = n(),
    incompletos = sum(!completo_capacidad),
    porcentaje_incompletos = round(100 * mean(!completo_capacidad), 1)
  )

# El 20% de los países de ingreso bajo tienen un 20% de datos incompletos. le sigue 
# los de ingreso medio-alto, alto y medio bajo. Segñun región, Medio Oriente tiene un
# 8,7%, seguido por Europa con un 7,5%, América Latina y Caribe, África Subsahariana
# y Asia Oriental y Pacífico.

# Identificamos que los faltantes no están perfectamente distribuidos segun región o ingreso, por ende
# podemos identifcarlos como MAR. 
# Como representan solo 11 de 195 países de análisis, optaremos por trabajar con casos completos.
# (aceptamos que podríamos tener algo de sesgo en los países de ingreso bajo)

# Construimos la base para 2023:

capacidad_2023 <- base_capacidad_respuesta |>
  filter(anio == 2023) |>
  drop_na(
    indice_cobertura_salud,
    gasto_bolsillo_salud,
    pib_per_capita_ppa
  )

# Diagnóstico de las variables:
# Distribución:

capacidad_2023 |>
  select(
    indice_cobertura_salud,
    gasto_bolsillo_salud,
    pib_per_capita_ppa
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Distribución de las variables de capacidad de respuesta",
    x = NULL,
    y = "Cantidad de países"
  ) +
  theme_minimal()

# Outliers:

capacidad_2023 |>
  select(
    indice_cobertura_salud,
    gasto_bolsillo_salud,
    pib_per_capita_ppa
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) |>
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Valores atípicos en las variables de capacidad de respuesta",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

# gasto_bolsillo_salud tiene una distribución aprox. normal. indice_cobertura_salud
# Tiene una cola izquierda, pero lo dejamos en su forma original para no meternos con 
# la lógica del índice.En cambio pib_per_capita_ppa merece transformación

# Identificamos los países con PIB ppa más alto:

capacidad_2023 |>
  arrange(desc(pib_per_capita_ppa)) |>
  select(
    pais,
    pib_per_capita_ppa
  ) |>
  head(15)

# Detectamos los outliers del PIB con IQR:

q1_pib <- quantile(capacidad_2023$pib_per_capita_ppa, 0.25)
q3_pib <- quantile(capacidad_2023$pib_per_capita_ppa, 0.75)

iqr_pib <- IQR(capacidad_2023$pib_per_capita_ppa)

limite_superior_pib <- q3_pib + 1.5 * iqr_pib

capacidad_2023 |>
  filter(pib_per_capita_ppa > limite_superior_pib) |>
  arrange(desc(pib_per_capita_ppa)) |>
  select(
    pais,
    pib_per_capita_ppa
  )

# Transformamos pib a log:

capacidad_2023 <- capacidad_2023 |>
  mutate(
    log_pib_per_capita = log(pib_per_capita_ppa)
  )

# Comparamos antes y después:

capacidad_2023 |>
  select(
    pib_per_capita_ppa,
    log_pib_per_capita
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "PIB per cápita PPA: nivel y logaritmo",
    x = NULL,
    y = "Cantidad de países"
  ) +
  theme_minimal()

# Logramos normalizar
# No eliminamos los outliers, es coherente que sean ellos los de PIB genuinamente alto.

# Definimos que nos quedamos con las variables:

vars <- c(
  "indice_cobertura_salud",
  "gasto_bolsillo_salud",
  "log_pib_per_capita"
)

# Chequeamos correlación de las variables: si no había correlación no tenía sentido agrupar.

X <- capacidad_2023 |>
  select(all_of(vars)) |>
  as.data.frame()

rownames(X) <- capacidad_2023$pais
round(cor(X), 2)

cor(X) |>
  as_tibble(rownames = "v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "r") |>
  ggplot(aes(v1, v2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(
    low = "firebrick",
    mid = "white",
    high = "steelblue",
    limits = c(-1, 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Correlaciones entre las variables de capacidad de respuesta"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ahora PCA:

res <- PCA(
  X,
  scale.unit = TRUE,
  graph = FALSE
)

# Para ver información de los componentes principales:

res$eig

# Hacemos el gráfico de scree plot

fviz_eig(
  res,
  addlabels = TRUE
)

# Notamos que PC1 y PC2 resumen el 95,7% de toda la información de las tres variables.
# Qué variables construyen PC1 y PC2? Chequeamos correlación entre cada variable y componente.

# Correlación de cada variable con PC1 y PC2

res$var$coord[, 1:2]

# Calidad de representación de cada variable en PC1 y PC2

res$var$cos2[, 1:2]

# Cuánto aporta cada variable a construir PC1 y PC2

res$var$contrib[, 1:2]

# Gráfico de variables y contribuciones por componente:

fviz_pca_var(
  res,
  repel = TRUE,
  col.var = "contrib",
  gradient.cols = c("gray70", "steelblue", "firebrick")
)

fviz_contrib(
  res,
  choice = "var",
  axes = 1
)

fviz_contrib(
  res,
  choice = "var",
  axes = 2
)

# Para la interpretación de los componentes:

dimdesc(
  res,
  axes = 1:2
)

# PC1 resume mayor índice de cobertura, mayor capacidad económica y menor gasto directo
# de los hogares. Pero las contribuciones a PC1 son distintas (y gasto de bolsillo en menor medida).
# La información que PC1 no captura viene de PC2: 83,2% de contribución de Gasto de Bolsillo.

# PC1 explica el 69,7% de la variación total. PC2 el 26%. 

# Decimos que PC1 representa la capacidad general de respuesta, mientras que PC2, 
# como dimensaión adicional vinculada principalmente a cuánto recae el gasto sanitario
# directamente sobre los hogares. 

# Aplicamos a países:

fviz_pca_ind(
  res,
  repel = TRUE,
  labelsize = 3,
  col.ind = "cos2",
  gradient.cols = c("gray80", "steelblue", "firebrick")
)

# Armamos los scores:

scores <- as_tibble(
  res$ind$coord[, 1:3],
  rownames = "pais"
) |>
  rename(
    PC1 = Dim.1,
    PC2 = Dim.2,
    PC3 = Dim.3
  )

scores |>
  arrange(desc(PC1)) |>
  head(15)

scores |>
  arrange(PC1) |>
  head(15)

# Armamos biplot (Para juntar países y variables que constituyen PCA)

fviz_pca_biplot(
  res,
  repel = TRUE,
  labelsize = 3,
  col.var = "firebrick",
  col.ind = "gray50",
  select.ind = list(cos2 = 40)
)

# Prueba de sensibilidad:
# Guardamos el PC1 original

pc1 <- res$ind$coord[, 1]

# Repetimos el PCA sacando una variable por vez

sensibilidad <- purrr::map_dfr(vars, function(v) {
  
  r <- PCA(
    X[, setdiff(vars, v), drop = FALSE],
    scale.unit = TRUE,
    graph = FALSE
  )
  
  tibble(
    sin = v,
    var_pc1 = r$eig[1, 2],
    cor_pc1 = abs(
      cor(
        r$ind$coord[, 1],
        pc1,
        method = "spearman"
      )
    )
  )
})

sensibilidad

# Rubustez: en los tres casos está por encima de 0,92. Esto quiere decir que, 
# aunque saquemos cualquiera de las tres variables, el orden relativo de los países 
# cambia muy poco.

#===============================================================================
# ¿Los países enfrentan niveles similares de envejecimiento con las mismas capacidades
# de respuesta sanitaria y económica?
#===============================================================================

# Esperamos encontrar que existan países con niveles similares de envejecimiento
# pero que presenten capacidades de respuesta diferentes.

# Buscamos si existen perfiles de capacidad de respuesta --> para luego ir hacia si
# los distintos perfiles enfrentan envejecimientos distintos

# Cluster jerárquico sobre componentes principales ####

pca_capacidad <- PCA(
  X,
  scale.unit = TRUE,
  ncp = 2,
  graph = FALSE
)

# Utilizamos HCPC para ver si podemos resumir los puntos en grupos

hcpc <- HCPC(
  pca_capacidad,
  nb.clust = -1,
  consol = TRUE,
  graph = TRUE
)

# El método sugiere 4 grupos: analizamos los grupos:

table(hcpc$data.clust$clust)
hcpc$desc.var$quanti

# Chequeamos silhouette 
# Coordenadas de los países en PC1 y PC2

coords <- pca_capacidad$ind$coord[, 1:2]

# Cluster asignado por HCPC

grupos <- as.integer(hcpc$data.clust$clust)

# Silhouette

sil <- silhouette(
  grupos,
  dist(coords)
)

# Silhouette promedio global

mean(sil[, "sil_width"])

sil_resumen <- as.data.frame(sil) |>
  as_tibble() |>
  group_by(cluster) |>
  summarise(
    n = n(),
    media = mean(sil_width),
    mediana = median(sil_width),
    minimo = min(sil_width),
    negativos = sum(sil_width < 0)
  )

sil_resumen

# Para ver gráficamente:

plot(sil)

# Chequeamos si k=4 es la mejor opción

sil_k <- map_dfr(2:6, function(k) {
  
  hcpc_k <- HCPC(
    pca_capacidad,
    nb.clust = k,
    consol = TRUE,
    graph = FALSE
  )
  
  grupos_k <- as.integer(hcpc_k$data.clust$clust)
  
  sil_k <- silhouette(
    grupos_k,
    dist(coords)
  )
  
  tibble(
    k = k,
    silhouette = mean(sil_k[, "sil_width"])
  )
})

sil_k

# Nos quedamos con k=4, porque logra representar mejor la heterogeneidad de los grupos.
# Para ver qué países quedaron en los clusters

# Guardar la pertenencia de cada país a su cluster

perfiles <- hcpc$data.clust |>
  as.data.frame() |>
  tibble::rownames_to_column("pais") |>
  select(pais, clust)

capacidad_2023 <- capacidad_2023 |>
  left_join(perfiles, by = "pais") |>
  rename(cluster_capacidad = clust)

# Verlos separados por grupo

split(perfiles$pais, perfiles$clust)

# Qué niveles de envejecimiento enfrentan los distintos perfiles?

# Cómo se distribuye el porcentaje de poblacion +65 dentro de cada perfil?

capacidad_2023 |>
  group_by(cluster_capacidad) |>
  summarise(
    n = n(),
    media = mean(poblacion_65_mas, na.rm = TRUE),
    mediana = median(poblacion_65_mas, na.rm = TRUE),
    sd = sd(poblacion_65_mas, na.rm = TRUE),
    minimo = min(poblacion_65_mas, na.rm = TRUE),
    maximo = max(poblacion_65_mas, na.rm = TRUE)
  )

ggplot(
  capacidad_2023,
  aes(
    x = factor(cluster_capacidad),
    y = poblacion_65_mas
  )
) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(
    title = "Nivel de envejecimiento según perfil de capacidad",
    subtitle = "Países, 2023",
    x = "Perfil de capacidad",
    y = "Población de 65 años o más (%)"
  ) +
  theme_minimal()

# Kruskal-wallis test

kruskal.test(
  poblacion_65_mas ~ cluster_capacidad,
  data = capacidad_2023
)

kw <- kruskal.test(
  poblacion_65_mas ~ cluster_capacidad,
  data = capacidad_2023
)

# Tamaño del efecto

unname(kw$statistic) / (nrow(capacidad_2023) - 1)

# Confirmamos con el test que algún perfil difiere de otro, pero precisamos saber cuáles son pares:

pairwise.wilcox.test(
  capacidad_2023$poblacion_65_mas,
  capacidad_2023$cluster_capacidad,
  p.adjust.method = "holm"
)

# Kruskal-Wallis muestra diferencias significativas en el nivel de envejecimiento
# entre los perfiles de capacidad (p < 2.2e-16).

# Las comparaciones de a pares muestran que todos los perfiles presentan diferencias
# significativas entre sí, excepto los perfiles 1 y 2 (p ajustado = 0.21).

# Los perfiles 1 y 2 presentan niveles de envejecimiento similares:
# sus medianas de población de 65 años o más son 3.21% y 3.73%, respectivamente.
# Sin embargo, representan perfiles diferentes de capacidad sanitaria y económica.

# Esto aporta evidencia a nuestra hipótesis:
# países con niveles similares de envejecimiento no necesariamente cuentan
# con las mismas capacidades de respuesta.

# Gráficos para PPT ####

# Perfiles de capacidad de respuesta

mapa_pca_clusters <- res$ind$coord[, 1:2] |>
  as.data.frame() |>
  rownames_to_column("pais") |>
  rename(
    PC1 = Dim.1,
    PC2 = Dim.2
  ) |>
  left_join(
    capacidad_2023 |>
      select(pais, cluster_capacidad),
    by = "pais"
  )

grafico_pca_clusters <- ggplot(
  mapa_pca_clusters,
  aes(
    x = PC1,
    y = PC2,
    color = factor(cluster_capacidad)
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    alpha = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    alpha = 0.4
  ) +
  geom_point(
    size = 2.2,
    alpha = 0.8
  ) +
  labs(
    title = "Perfiles de capacidad de respuesta",
    subtitle = "Países según los dos primeros componentes principales",
    x = "PC1 · Capacidad general",
    y = "PC2 · Dimensión asociada al gasto de bolsillo",
    color = "Perfil"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

grafico_pca_clusters

ggsave("pca_clusters.png",
       grafico_pca_clusters,
       width = 5, height = 3.5,
       dpi = 300, bg = "white")

# Diferencias entre países 

grafico_envejecimiento_12 <- capacidad_2023 |>
  filter(cluster_capacidad %in% c(1, 2)) |>
  ggplot(
    aes(
      x = factor(cluster_capacidad),
      y = poblacion_65_mas
    )
  ) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.35,
    size = 1.5
  ) +
  labs(
    title = "Envejecimiento similar, distintos perfiles",
    subtitle = "Medianas: 3,21% vs. 3,73% · p ajustado = 0,21",
    x = "Perfil de capacidad",
    y = "Población de 65 años o más (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold")
  )

grafico_envejecimiento_12

ggsave("envejecimiento_similar.png",
       grafico_envejecimiento_12,
       width = 5, height = 3.2,
       dpi = 300, bg = "white")
