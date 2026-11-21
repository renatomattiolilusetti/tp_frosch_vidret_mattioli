# ==============================================================================
# Visualizaciones: gráfico exploratorio y comunicativo
# ==============================================================================

# 1. Preparación del análisis ####

# Cargar paquetes
library(tidyverse)
library(FactoMineR)
library(ggrepel)
library(scales)
library(sf)
library(rnaturalearth)
library(ggtext)
library(patchwork)

# Funciones propias
source("utils/clasificar_anio.R")
source("utils/crear_mapa.R")

# Crear la carpeta de resultados si no existe
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

# Variables y períodos utilizados en los gráficos
anios_analisis <- c(1980, 1995, 2010, 2024)
vars_demograficas <- c("poblacion_65_mas", "fecundidad", "esperanza_vida",
                       "crecimiento_poblacional")

# Lectura de datos necesarios
archivos_necesarios <- c(
  "input/wdi_limpia.csv",
  "auxiliar/base_cluster_demografico_cortes.csv",
  "output/tablas/trayectorias_paises_demograficos.csv"
)

# Verificar que existan los archivos
archivos_faltantes <- archivos_necesarios[!file.exists(archivos_necesarios)]
if (length(archivos_faltantes) > 0) {
  stop("Faltan archivos necesarios: ", paste(archivos_faltantes, collapse = ", "))
}

# Leer la base limpia
wdi_limpia <- read_csv(
  "input/wdi_limpia.csv",
  show_col_types = FALSE
)

# Leer la muestra utilizada en el clustering
base_cluster_demografico <- read_csv(
  "auxiliar/base_cluster_demografico_cortes.csv",
  show_col_types = FALSE
)

# Leer las clasificaciones de los países
trayectorias_paises <- read_csv(
  "output/tablas/trayectorias_paises_demograficos.csv",
  show_col_types = FALSE
)

# Verificar que no haya duplicados
stopifnot(
  !anyDuplicated(
    base_cluster_demografico[c("codigo_pais", "anio")]
  ),
  !anyDuplicated(trayectorias_paises["pais"])
)

print("Datos cargados correctamente.")

# 2. Gráfico exploratorio: perfiles demográficos en cuatro períodos ####

# 2.1. Preparación de los datos para el PCA común

clusters_largo <- trayectorias_paises |>
  pivot_longer(starts_with("cluster_"), names_to = "anio", values_to = "clust",
               names_prefix = "cluster_", names_transform = list(anio = as.integer)) |>
  mutate(perfil = if_else(as.character(clust) == "1", "joven", "avanzado"))

apilado <- base_cluster_demografico |>
  filter(anio %in% anios_analisis) |>
  drop_na(all_of(vars_demograficas)) |>
  inner_join(clusters_largo, by = c("pais", "anio"))

# Control: distribución de perfiles por período (sin cantidades fijas)
apilado |> count(anio, perfil) |> pivot_wider(names_from = perfil, values_from = n)


# 2.2. PCA común para representar los cuatro períodos en los mismos ejes

X_apilado <- apilado |> select(all_of(vars_demograficas)) |> as.data.frame()
rownames(X_apilado) <- paste(apilado$pais, apilado$anio)

pca_apilado <- PCA(X_apilado, scale.unit = TRUE, ncp = 2, graph = FALSE)
pca_apilado$eig[1:2, ]
pca_apilado$var$coord[, 1:2]

# PC1 orientado para que crezca con el % de 65+ (a la derecha = más envejecido)
signo1 <- sign(pca_apilado$var$coord["poblacion_65_mas", "Dim.1"])

# PC2 orientado para que su variable más importante crezca hacia arriba
carga2_bruta <- pca_apilado$var$coord[, "Dim.2"]
signo2 <- sign(carga2_bruta[which.max(abs(carga2_bruta))])

puntos <- apilado |>
  mutate(PC1 = signo1 * pca_apilado$ind$coord[, 1],
         PC2 = signo2 * pca_apilado$ind$coord[, 2])


# 2.3. Medianas y etiquetas de los países

resumen <- puntos |>
  group_by(anio, perfil) |>
  summarise(x = median(PC1), y = median(PC2),
            sd_y = sd(PC2), n = n(), .groups = "drop")
resumen   # cuánto se corre cada grupo (x) y cuánto se dispersa en y (sd_y)

# Nombres en castellano de países conocidos (se usan para los extremos de cada año)
conocidos <- c(
  ARG = "Argentina", BRA = "Brasil", MEX = "México", CHL = "Chile", COL = "Colombia",
  PER = "Perú", URY = "Uruguay", BOL = "Bolivia", CUB = "Cuba", USA = "Estados Unidos",
  CAN = "Canadá", ESP = "España", ITA = "Italia", FRA = "Francia", DEU = "Alemania",
  GBR = "Reino Unido", PRT = "Portugal", POL = "Polonia", RUS = "Rusia", JPN = "Japón",
  KOR = "Corea del Sur", CHN = "China", IND = "India", IDN = "Indonesia", VNM = "Vietnam",
  THA = "Tailandia", PHL = "Filipinas", BGD = "Bangladés", PAK = "Pakistán", IRN = "Irán",
  TUR = "Turquía", EGY = "Egipto", MAR = "Marruecos", ZAF = "Sudáfrica", NGA = "Nigeria",
  ETH = "Etiopía", KEN = "Kenia", GHA = "Ghana", SEN = "Senegal", AUS = "Australia"
)

# Destacar únicamente a Argentina
etiquetas <- puntos |>
  filter(codigo_pais == "ARG") |>
  mutate(nombre = "Argentina")

# Rótulo de cada panel con los conteos
rotulos <- resumen |>
  select(anio, perfil, n) |>
  pivot_wider(names_from = perfil, values_from = n) |>
  mutate(rotulo = paste0(anio, "   ·   ", joven, " jóvenes, ", avanzado, " avanzados"))
rotulos <- select(rotulos, anio, rotulo)
puntos    <- left_join(puntos, rotulos, by = "anio")
resumen   <- left_join(resumen, rotulos, by = "anio")
etiquetas <- left_join(etiquetas, rotulos, by = "anio")

# Rango visible: en el eje vertical recortamos el 0,5% más extremo de cada lado,
# para que unos pocos países no estiren el gráfico. En la nota se informa cuántas
# observaciones quedan afuera.

# Eje x: se muestran todos los países y se deja aire a los dos costados, para
# que se vea cómo las nubes se corren hacia la derecha.
rango_x <- range(puntos$PC1)
lim_x   <- rango_x + c(-1, 1) * 0.06 * diff(rango_x)
# Eje y: se recorta el 0,5% más extremo de cada lado.
lim_y   <- quantile(puntos$PC2, c(0.005, 0.995))
n_fuera <- sum(puntos$PC2 < lim_y[1] | puntos$PC2 > lim_y[2])

# Extremos de cada año: el país más joven (más a la izquierda en el componente 1)
# y el más envejecido (más a la derecha), entre los que se ven en el gráfico
mostrar_extremos <- TRUE
nombres_extra <- c(
  NER = "Níger", MLI = "Malí", TCD = "Chad", SOM = "Somalia", UGA = "Uganda", YEM = "Yemen",
  AGO = "Angola", COD = "RD del Congo", BFA = "Burkina Faso", AFG = "Afganistán", RWA = "Ruanda",
  MWI = "Malaui", BDI = "Burundi", GMB = "Gambia", MOZ = "Mozambique", SWE = "Suecia",
  NOR = "Noruega", DNK = "Dinamarca", FIN = "Finlandia", AUT = "Austria", BEL = "Bélgica",
  CHE = "Suiza", GRC = "Grecia", BGR = "Bulgaria", HRV = "Croacia", LVA = "Letonia",
  EST = "Estonia", LTU = "Lituania", UKR = "Ucrania", HUN = "Hungría", MLT = "Malta",
  PRI = "Puerto Rico", HKG = "Hong Kong", MCO = "Mónaco", SMR = "San Marino",
  LIE = "Liechtenstein", AND = "Andorra", DEU = "Alemania", ITA = "Italia", PRT = "Portugal",
  ZMB = "Zambia", TZA = "Tanzania", SLE = "Sierra Leona", GIN = "Guinea", LBR = "Liberia",
  CAF = "República Centroafricana", SDN = "Sudán", SSD = "Sudán del Sur", CIV = "Costa de Marfil",
  BEN = "Benín", TGO = "Togo", CMR = "Camerún", GNQ = "Guinea Ecuatorial", SYR = "Siria",
  JOR = "Jordania", LBY = "Libia", OMN = "Omán", SAU = "Arabia Saudita", ARE = "Emiratos Árabes Unidos",
  QAT = "Catar", KWT = "Kuwait", BHR = "Baréin", ZWE = "Zimbabue", SRB = "Serbia", SVN = "Eslovenia",
  CZE = "República Checa", ROU = "Rumania", GBR = "Reino Unido", JPN = "Japón", KOR = "Corea del Sur"
)
extremos <- puntos |>
  filter(PC2 >= lim_y[1], PC2 <= lim_y[2]) |>
  group_by(anio) |>
  filter(PC1 == min(PC1) | PC1 == max(PC1)) |>
  ungroup() |>
  mutate(tipo = if_else(PC1 < 0, "Más joven: ", "Más envejecido: "),
         nombre = coalesce(unname(conocidos[codigo_pais]), unname(nombres_extra[codigo_pais]), pais),
         texto = paste0(tipo, nombre))
extremos |> select(anio, texto)

# Control: avisa si algún extremo no tiene nombre en castellano cargado
sin_traducir <- extremos |>
  filter(!codigo_pais %in% c(names(conocidos), names(nombres_extra))) |>
  distinct(codigo_pais, pais)
if (nrow(sin_traducir) > 0) {
  message("Agregar a `nombres_extra` el nombre en castellano de: ",
          paste0(sin_traducir$codigo_pais, " (", sin_traducir$pais, ")", collapse = ", "))
}
if (!mostrar_extremos) extremos <- extremos[0, ]

# Extremos cuyo nombre va abajo del punto (por defecto va arriba)
extremos_abajo <- c("SOM")
extremos <- extremos |> mutate(abajo = codigo_pais %in% extremos_abajo)


# Posicionar la etiqueta de Argentina en relación con la mediana de su perfil
etiquetas <- etiquetas |>
  left_join(select(resumen, anio, perfil, my = y), by = c("anio", "perfil")) |>
  mutate(abajo = PC2 < my)

# Etiqueta de Argentina en 1980: posición fija a la derecha del eje vertical.
# Los otros tres años mantienen sus etiquetas automáticas originales
etiquetas_1980 <- etiquetas |> filter(anio == 1980)
etiquetas_otras <- etiquetas |> filter(anio != 1980)

# Cantidad de países representados: se calcula desde los datos, no lo fijamos a mano
n_paises_graficados <- n_distinct(puntos$codigo_pais)


# 2.4. Estilo visual

fuente_titulo <- "sans"
fuente_texto  <- "sans"

if ("showtext" %in% loadedNamespaces()) showtext::showtext_auto(FALSE)

# Paleta (común: Okabe-Ito)
col_perfil <- c(joven = "#0072B2", avanzado = "#E69F00")

azul_titulo <- "#19354D"
azul_texto  <- "#38536B"
azul_nota   <- "#4F6478"
gris_ejes   <- "#677887"


# 2.5. Construcción del gráfico exploratorio

grafico_exploratorio <- ggplot(puntos, aes(PC1, PC2)) +
  
  # Referencias centrales, sin grilla excesiva
  geom_hline(yintercept = 0, color = "#AAB6C0", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "#8795A3", linewidth = 0.55) +
  
  # Un punto = un país en cada año (transparencia para ver superposiciones)
  geom_point(
    aes(fill = perfil), shape = 21, size = 2.6, alpha = 0.42,
    color = "white", stroke = 0.2
  ) +
  
  # Anillos para los países extremos de cada año
  geom_point(
    data = extremos, shape = 21, fill = NA,
    color = azul_titulo, size = 4.4, stroke = 0.8,
    show.legend = FALSE
  ) +
  
  # Medianas: triángulos (no forman parte de la leyenda de perfiles)
  geom_point(
    data = resumen, aes(x, y, fill = perfil), inherit.aes = FALSE,
    shape = 24, size = 4.2, color = azul_titulo, stroke = 1.0,
    show.legend = FALSE
  ) +
  
  # Argentina: un único círculo, sin halo blanco, con su perfil correspondiente.
  # Se dibuja sobre los triángulos para que siga siendo visible.
  geom_point(
    data = etiquetas, aes(x = PC1, y = PC2, fill = perfil),
    inherit.aes = FALSE, shape = 21,
    size = 3.8, color = azul_titulo, stroke = 0.75,
    show.legend = FALSE
  ) +
  
  # Argentina en 1980: etiqueta cercana al punto, ligeramente arriba y a la derecha.
  # pmax() evita que el texto comience sobre la línea vertical x = 0.
  geom_text(
    data = etiquetas_1980,
    aes(x = pmax(PC1 + 0.08, 0.15), y = PC2 + 0.30, label = nombre),
    inherit.aes = FALSE,
    family = fuente_texto, fontface = "bold", size = 3.4,
    color = azul_titulo, hjust = 0, show.legend = FALSE
  ) +
  
  # Argentina en 1995, 2010 y 2024: conservar ubicación automática.
  geom_text_repel(
    data = filter(etiquetas_otras, !abajo), aes(label = nombre),
    family = fuente_texto, fontface = "bold", size = 3.4,
    color = azul_titulo, nudge_y = 0.65,
    box.padding = 0.4, point.padding = 0.3,
    segment.color = "#9CAEBB", min.segment.length = 0,
    seed = 1, max.overlaps = Inf, show.legend = FALSE
  ) +
  geom_text_repel(
    data = filter(etiquetas_otras, abajo), aes(label = nombre),
    family = fuente_texto, fontface = "bold", size = 3.4,
    color = azul_titulo, nudge_y = -0.65,
    box.padding = 0.4, point.padding = 0.3,
    segment.color = "#9CAEBB", min.segment.length = 0,
    seed = 1, max.overlaps = Inf, show.legend = FALSE
  ) +
  
  # Nombres de los países extremos
  geom_text_repel(
    data = filter(extremos, !abajo), aes(label = texto),
    family = fuente_texto, fontface = "italic", size = 3.05,
    color = azul_texto, nudge_y = 0.75,
    box.padding = 0.24, point.padding = 0.2,
    segment.color = "#B9C6D0", min.segment.length = 0,
    seed = 2, max.overlaps = Inf, show.legend = FALSE
  ) +
  geom_text_repel(
    data = filter(extremos, abajo), aes(label = texto),
    family = fuente_texto, fontface = "italic", size = 3.05,
    color = azul_texto, nudge_y = -0.75,
    box.padding = 0.24, point.padding = 0.2,
    segment.color = "#B9C6D0", min.segment.length = 0,
    seed = 2, max.overlaps = Inf, show.legend = FALSE
  ) +
  
  facet_wrap(~ rotulo, ncol = 1) +
  scale_fill_manual(
    values = col_perfil,
    breaks = c("joven", "avanzado"),
    labels = c("Perfil demográfico joven", "Transición avanzada"),
    name = NULL
  ) +
  # La leyenda muestra círculos (los países), no triángulos (las medianas)
  guides(fill = guide_legend(
    override.aes = list(
      shape = 21, size = 4, alpha = 1,
      color = azul_titulo, stroke = 0.4
    )
  )) +
  scale_x_continuous(
    breaks = seq(-6, 6, 2),
    labels = label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = 0.02)
  ) +
  scale_y_continuous(breaks = c(-2, 0, 2)) +
  coord_cartesian(xlim = lim_x, ylim = lim_y) +
  
  labs(
    title = paste0(
      "Distintas trayectorias del envejecimiento:\n",
      "perfiles jóvenes y avanzados en ", n_paises_graficados, " países"
    ),
    subtitle = paste0(
      "El perfil joven tiene más fecundidad y crecimiento poblacional que el avanzado,\n",
      "y menos población de 65+ y esperanza de vida. Los perfiles se definen en cada año\n",
      "en relación con el resto de los países, mientras la posición usa una escala común.\n",
      "El triángulo representa la mediana del grupo. Se señala el caso de Argentina\n",
      "y, con un anillo, el más joven y el más envejecido de cada año."
    ),
    x = paste0(
      "Componente 1: perfiles más envejecidos\n",
      "(hacia la derecha: más población de 65+ y esperanza de vida; menos fecundidad y crecimiento)"
    ),
    y = "Componente 2: crecimiento poblacional\n(arriba, mayor crecimiento)",
    caption = paste0(
      "Fuente: Banco Mundial (WDI). Elaboración propia.\n",
      "Método: PCA conjunto de los cuatro años; clusters jerárquicos por año. ",
      "Triángulos: medianas.\n",
      "Anillos: extremos del primer componente; Argentina destacada.\n",
      "Eje vertical: ", n_fuera, " observaciones fuera de vista."
    )
  ) +
  theme_minimal(base_size = 12, base_family = fuente_texto) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      family = fuente_titulo, size = 18, face = "bold",
      color = azul_titulo, hjust = 0.5, lineheight = 1.08,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      family = fuente_texto, size = 11.7,
      color = azul_texto, hjust = 0.5, lineheight = 1.2,
      margin = margin(b = 14)
    ),
    plot.caption = element_text(
      family = fuente_texto, size = 9.8,
      color = azul_nota, hjust = 0.5, lineheight = 1.2,
      margin = margin(t = 12)
    ),
    
    strip.text = element_text(
      face = "bold", hjust = 0, color = azul_titulo,
      size = 11.7, margin = margin(t = 3, b = 6)
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E5EBEF", linewidth = 0.3),
    panel.spacing.y = unit(14, "pt"),
    
    axis.title = element_text(color = azul_texto, size = 10),
    axis.title.x = element_text(margin = margin(t = 7)),
    axis.title.y = element_text(margin = margin(r = 7)),
    axis.text = element_text(color = gris_ejes, size = 9.5),
    
    # La leyenda conserva la asociación de colores sin depender del título
    legend.position = "bottom",
    legend.text = element_text(size = 10.5, color = azul_texto),
    legend.key.width = unit(1.0, "cm"),
    legend.margin = margin(t = 4, b = 2),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(16, 18, 14, 16)
  )


# 2.6. Exportación del gráfico exploratorio

archivo_exploratorio <- "output/graficos/grupo_10_exploratorio.jpg"
ggsave(
  filename = archivo_exploratorio,
  plot = grafico_exploratorio,
  width = 24,
  height = 29,
  units = "cm",
  dpi = 300,
  bg = "white",
  quality = 95
)


# 3. Gráfico comunicativo: mapas demográficos de 1980 y 2024 ####

# 3.1. Preparación de los datos para ambos mapas
columnas_necesarias <- c("pais", "codigo_pais", "anio", vars_demograficas)

if (!all(columnas_necesarias %in% names(wdi_limpia))) {
  stop(paste("Faltan columnas en wdi_limpia:",
             paste(setdiff(columnas_necesarias, names(wdi_limpia)),
                   collapse = ", ")))
}

base_mapas <- wdi_limpia |>
  filter(anio %in% c(1980, 2024),
         !is.na(codigo_pais), codigo_pais != "PSE") |>
  select(all_of(columnas_necesarias))

# La unidad de observacion debe ser pais-año
if (anyDuplicated(base_mapas[c("codigo_pais", "anio")]) > 0) {
  stop("Se encontraron filas duplicadas para un mismo país y año.")
}

# No se reemplazan NA por cero ni se imputan valores faltantes
base_mapas <- base_mapas |>
  filter(if_all(all_of(vars_demograficas), ~ !is.na(.x) & is.finite(.x)))

paises_comunes <- base_mapas |>
  count(codigo_pais, name = "n_anios") |>
  filter(n_anios == 2) |>
  pull(codigo_pais)

base_mapas <- base_mapas |>
  filter(codigo_pais %in% paises_comunes)

n_comparables <- length(paises_comunes)
if (n_comparables < 3) {
  stop("Hay menos de tres países con datos completos en ambos años.")
}

message("Países comparables para ambos años: ", n_comparables)


# 3.2. PCA y clasificación demográfica por período

perfiles <- bind_rows(
  clasificar_anio(base_mapas, vars_demograficas, 1980),
  clasificar_anio(base_mapas, vars_demograficas, 2024)
)

stopifnot(anyDuplicated(perfiles[c("codigo_pais", "anio")]) == 0)
stopifnot(nrow(perfiles) == 2 * n_comparables)

resumen_mapa <- perfiles |>
  count(anio, perfil, name = "paises") |>
  arrange(anio, perfil)

print(resumen_mapa)

n_1980 <- sum(perfiles$anio == 1980 &
                perfiles$perfil == "Transición avanzada")
n_2024 <- sum(perfiles$anio == 2024 &
                perfiles$perfil == "Transición avanzada")
cambio_neto <- n_2024 - n_1980


# 3.3. Cartografía y uniones controladas

# Natural Earth: la escala 50 requiere a veces una descarga inicial; 110
# funciona como respaldo. No cambia la clasificacion de los paises
mundo <- tryCatch(
  rnaturalearth::ne_countries(scale = 50, returnclass = "sf"),
  error = function(e) {
    message("Se utilizará la cartografía Natural Earth escala 110.")
    rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
  }
)

mundo <- mundo |>
  filter(admin != "Antarctica") |>
  mutate(
    codigo_pais = coalesce(na_if(iso_a3, "-99"), adm0_a3),
    codigo_pais = if_else(codigo_pais == "KOS", "XKX", codigo_pais)
  ) |>
  select(codigo_pais, geometry)

sin_geometria <- perfiles |>
  distinct(codigo_pais, pais) |>
  anti_join(mundo |>
              st_drop_geometry() |>
              distinct(codigo_pais), by = "codigo_pais")

if (nrow(sin_geometria) > 0) {
  message("Países sin polígono en Natural Earth: ",
          paste(sin_geometria$pais, collapse = ", "))
}

mapa_1980 <- mundo |>
  left_join(perfiles |>
              filter(anio == 1980) |>
              select(codigo_pais, perfil),
            by = "codigo_pais", relationship = "many-to-one") |>
  mutate(anio = 1980)

mapa_2024 <- mundo |>
  left_join(perfiles |>
              filter(anio == 2024) |>
              select(codigo_pais, perfil),
            by = "codigo_pais", relationship = "many-to-one") |>
  mutate(anio = 2024)


# 3.4. Paleta de colores y textos

# Paleta Okabe-Ito
paleta <- c(
  "Perfil demográfico joven" = "#0072B2",
  "Transición avanzada"     = "#E69F00",
  "Sin clasificación"       = "#D9D2E5"
)

mapas <- bind_rows(mapa_1980, mapa_2024) |>
  mutate(
    perfil_mapa = if_else(is.na(perfil), "Sin clasificación",
                          as.character(perfil)),
    perfil_mapa = factor(perfil_mapa, levels = names(paleta))
  )

stopifnot(!any(is.na(mapas$perfil_mapa)))

if (cambio_neto > 0) {
  mensaje_cambio <- paste0(cambio_neto, " países más")
} else if (cambio_neto < 0) {
  mensaje_cambio <- paste0(abs(cambio_neto), " países menos")
} else {
  mensaje_cambio <- "la misma cantidad de países"
}

titulo_principal <- paste0(
  "El perfil demográfico avanzado reúne<br>",
  "<span style='color:#A66A00'><b>", mensaje_cambio,
  "</b></span> que en 1980"
)

subtitulo_principal <- paste0(
  "En 2024 hay más países con baja fecundidad, mayor esperanza de vida\n",
  "y mayor peso de la población de 65+ que en 1980.\n",
  "Se comparan ", n_comparables,
  " países con datos completos en 1980 y 2024."
)

nota_fuente <- paste(
  "Fuente: Banco Mundial (WDI). Cartografía: Natural Earth. Elaboración propia.",
  "Método: PCA y HCPC; población 65+, fecundidad, esperanza de vida y crecimiento.",
  "Nota: perfiles relativos estimados por año, sin umbral fijo; lavanda: sin clasificación.",
  sep = "\n"
)

color_titulo <- "#18344D"
color_texto  <- "#36536B"
color_nota   <- "#52677A"


# 3.5. Construcción de los mapas de 1980 y 2024

mapa_grafico_1980 <- crear_mapa(mapas, 1980, n_1980, paleta, color_titulo)
mapa_grafico_2024 <- crear_mapa(mapas, 2024, n_2024, paleta, color_titulo)


# 3.6. Composición del gráfico comunicativo

# Alineacion hjust = 0.5: titulo, subtitulo, encabezados y notas centrados
comparacion <- (mapa_grafico_1980 / mapa_grafico_2024) +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(1, 1),
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    title = titulo_principal,
    subtitle = subtitulo_principal,
    caption = nota_fuente,
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggtext::element_markdown(
        face = "bold", size = 18.5, color = color_titulo,
        hjust = 0.5, halign = 0.5, lineheight = 1.14, margin = margin(b = 9)
      ),
      # Subtítulo con las tres líneas centradas.
      plot.subtitle = element_text(
        size = 13.2, color = color_texto, hjust = 0.5,
        lineheight = 1.22, margin = margin(b = 19)
      ),
      # Fuente y nota para la exportación a 300 dpi.
      plot.caption = element_text(
        size = 10.5, color = color_nota, hjust = 0.5,
        lineheight = 1.22, margin = margin(t = 14)
      ),
      plot.margin = margin(t = 14, r = 18, b = 14, l = 18)
    )
  ) &
  theme(legend.position = "bottom")


# 3.7. Exportación del gráfico comunicativo

archivo_comunicativo <- "output/graficos/grupo_10_comunicativo.jpg"

ggsave(
  filename = archivo_comunicativo,
  plot = comparacion,
  width = 24.5,
  height = 27,
  units = "cm",
  dpi = 300,
  bg = "white"
)

# 4. Control final de archivos generados ####

archivos_generados <- c(
  "output/graficos/grupo_10_exploratorio.jpg",
  "output/graficos/grupo_10_comunicativo.jpg"
)

stopifnot(
  all(file.exists(archivos_generados))
)

print("11 finalizado: todos los gráficos fueron generados.")
