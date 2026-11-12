# =============================================================================
# Grupo 10 · Instancia 3 · Gráfico EXPLORATORIO
# Los perfiles demográficos en 1980, 1995, 2010 y 2024 sobre UN MISMO plano
# Versión final: halo para Argentina, leyenda con círculos y eje cero suave.
# Exportación robusta sin cajas de texto de ggtext.
#
# Se corre DESPUÉS de 03.02_clustering.R: usa `base_cluster_demografico`
# (ya sin PSE) y `trayectorias_paises` (cluster de cada país en cada año).
#
# En 03.02 el PCA se estimó por separado en cada año, así que sus ejes no son
# comparables entre años. Acá estimamos UN PCA con los cuatro años apilados
# (194 países × 4) para que los paneles compartan ejes. Los colores siguen
# siendo los clusters de 03.02: no se re-estiman.
# =============================================================================

paquetes <- c("tidyverse", "FactoMineR", "ggrepel", "scales")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) install.packages(faltan)

suppressWarnings(suppressPackageStartupMessages({
  library(tidyverse)
  library(FactoMineR)
  library(ggrepel)
  library(scales)
}))


# -----------------------------------------------------------------------------
# 1. Datos: los cuatro momentos apilados + el cluster de cada país en cada año
# -----------------------------------------------------------------------------

anios <- c(1980, 1995, 2010, 2024)
vars_demograficas <- c("poblacion_65_mas", "fecundidad", "esperanza_vida",
                       "crecimiento_poblacional")

clusters_largo <- trayectorias_paises |>
  pivot_longer(starts_with("cluster_"), names_to = "anio", values_to = "clust",
               names_prefix = "cluster_", names_transform = list(anio = as.integer)) |>
  mutate(perfil = if_else(as.character(clust) == "1", "joven", "avanzado"))

apilado <- base_cluster_demografico |>
  filter(anio %in% anios) |>
  drop_na(all_of(vars_demograficas)) |>
  inner_join(clusters_largo, by = c("pais", "anio"))

# Control: 117/77 en 1980 ... 84/110 en 2024
apilado |> count(anio, perfil) |> pivot_wider(names_from = perfil, values_from = n)


# -----------------------------------------------------------------------------
# 2. PCA apilado
# -----------------------------------------------------------------------------

X_apilado <- apilado |> select(all_of(vars_demograficas)) |> as.data.frame()
rownames(X_apilado) <- paste(apilado$pais, apilado$anio)

pca_apilado <- PCA(X_apilado, scale.unit = TRUE, ncp = 2, graph = FALSE)
pca_apilado$eig[1:2, ]
pca_apilado$var$coord[, 1:2]    # MIRAR para escribir los títulos de los ejes

# PC1 orientado para que crezca con el % de 65+ (a la derecha = más envejecido)
signo1 <- sign(pca_apilado$var$coord["poblacion_65_mas", "Dim.1"])
# PC2 orientado para que su variable más importante crezca hacia arriba
carga2_bruta <- pca_apilado$var$coord[, "Dim.2"]
signo2 <- sign(carga2_bruta[which.max(abs(carga2_bruta))])

puntos <- apilado |>
  mutate(PC1 = signo1 * pca_apilado$ind$coord[, 1],
         PC2 = signo2 * pca_apilado$ind$coord[, 2])

var_pc1 <- pca_apilado$eig[1, 2]
var_pc2 <- pca_apilado$eig[2, 2]

# Explicación breve del eje vertical, armada con las cargas del PC2:
# se nombran las variables con carga de 0,3 o más (en valor absoluto),
# de la más importante a la menos importante.
nombres_var <- c(poblacion_65_mas = "población de 65+", fecundidad = "fecundidad",
                 esperanza_vida = "esperanza de vida", crecimiento_poblacional = "crecimiento poblacional")
carga2  <- signo2 * carga2_bruta
fuertes <- carga2[abs(carga2) >= 0.3]
fuertes <- fuertes[order(-abs(fuertes))]
texto_pc2 <- paste0(if_else(fuertes > 0, "más ", "menos "), nombres_var[names(fuertes)],
                    collapse = " y ")
round(carga2, 2)   # MIRAR: confirmar que la frase del eje y tenga sentido
texto_pc2


# -----------------------------------------------------------------------------
# 3. Resumen de cada grupo: mediana en cada año
# -----------------------------------------------------------------------------

resumen <- puntos |>
  group_by(anio, perfil) |>
  summarise(x = median(PC1), y = median(PC2),
            sd_y = sd(PC2), n = n(), .groups = "drop")
resumen   # cuánto se corre cada grupo (x) y cuánto se dispersa en y (sd_y)

# País típico de cada grupo: el que, sumando los cuatro años, queda más cerca
# de la mediana de su perfil. Tomamos dos trayectorias: siempre joven y
# siempre avanzado.
trayectoria_tipo <- trayectorias_paises |>
  mutate(patron = paste(cluster_1980, cluster_1995, cluster_2010, cluster_2024, sep = "-")) |>
  select(pais, patron) |>
  filter(patron %in% c("1-1-1-1", "2-2-2-2"))

# Solo buscamos entre países conocidos, con su nombre en castellano.
# Se pueden agregar o sacar países de esta lista.
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

# Para que el país no quede tapado por el triángulo, medimos la distancia como
# se ve en el gráfico (en cm, cada panel mide unos 19 x 4 cm) y exigimos que en
# los cuatro años esté al menos a `separacion_cm` de la mediana. Entre los que
# cumplen, elegimos el más cercano a la mediana (el más típico).
ancho_panel_cm <- 19
alto_panel_cm  <- 4
separacion_cm  <- 0.6
escala_x <- ancho_panel_cm / (1.12 * diff(range(puntos$PC1)))
escala_y <- alto_panel_cm  / diff(quantile(puntos$PC2, c(0.005, 0.995)))

# Destacar únicamente a Argentina
etiquetas <- puntos |>
  filter(codigo_pais == "ARG") |>
  mutate(nombre = "Argentina")

# Rótulo de cada panel con los conteos
rotulos <- resumen |>
  select(anio, perfil, n) |>
  pivot_wider(names_from = perfil, values_from = n) |>
  mutate(rotulo = paste0(anio, "   ·   ", joven, " jóvenes, ", avanzado, " avanzados"))
con_rotulo <- \(d) left_join(d, select(rotulos, anio, rotulo), by = "anio")
puntos  <- con_rotulo(puntos)
resumen <- con_rotulo(resumen)
etiquetas <- con_rotulo(etiquetas)

# Rango visible: recortamos el 0,5% más extremo de cada lado en los dos ejes,
# para que unos pocos países no estiren el gráfico (sobraba espacio a la
# izquierda). Se informa en la nota cuántas observaciones quedan afuera.
# Eje x: se muestran todos los países y se deja aire a los dos costados, para
# que se vea cómo las nubes se corren hacia la derecha.
rango_x <- range(puntos$PC1)
lim_x   <- rango_x + c(-1, 1) * 0.06 * diff(rango_x)
# Eje y: se recorta el 0,5% más extremo de cada lado.
lim_y   <- quantile(puntos$PC2, c(0.005, 0.995))
n_fuera <- sum(puntos$PC2 < lim_y[1] | puntos$PC2 > lim_y[2])

# Extremos de cada año: el país más joven (más a la izquierda en el componente 1)
# y el más envejecido (más a la derecha), entre los que se ven en el gráfico.
# Poner FALSE para sacarlos.
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

# Extremos cuyo nombre va abajo del punto (por defecto va arriba). Agregar
# códigos si algún otro nombre se superpone con un círculo.
extremos_abajo <- c("SOM")
extremos <- extremos |> mutate(abajo = codigo_pais %in% extremos_abajo)


# Etiquetas de países típicos: el nombre va del lado opuesto al triángulo
# (si el país está por encima de la mediana, el nombre va arriba; si está por
# debajo, abajo), así nunca cae sobre el triángulo.
etiquetas <- etiquetas |>
  left_join(select(resumen, anio, perfil, my = y), by = c("anio", "perfil")) |>
  mutate(abajo = PC2 < my)

# Etiqueta de Argentina en 1980: posición fija a la derecha del eje vertical.
# Los otros tres años mantienen sus etiquetas automáticas originales.
etiquetas_1980 <- etiquetas |> filter(anio == 1980)
etiquetas_otras <- etiquetas |> filter(anio != 1980)


# Cantidad de países representados: se calcula desde los datos, no se fija a mano.
n_paises_graficados <- n_distinct(puntos$codigo_pais)

# -----------------------------------------------------------------------------
# 4. Estilo visual: misma identidad que el mapa comunicativo
# -----------------------------------------------------------------------------

# Conservamos la tipografía sans (como en el mapa); evitamos descargas de fuentes.
fuente_titulo <- "sans"
fuente_texto  <- "sans"

# Evitamos diferencias de tamaño entre la vista de RStudio y el JPG exportado
# si showtext quedó activado por otro script de la materia.
if ("showtext" %in% loadedNamespaces()) showtext::showtext_auto(FALSE)

# Colores idénticos al mapa comunicativo (paleta Okabe-Ito).
# La transparencia se aplica solo a los puntos de países, no a las medianas.
col_perfil <- c(joven = "#0072B2", avanzado = "#E69F00")

# Colores de textos iguales a los del mapa comunicativo.
azul_titulo <- "#19354D"
azul_texto  <- "#38536B"
azul_nota   <- "#4F6478"
gris_ejes   <- "#677887"

# Para prevenir errores de viewport, usamos textos normales de ggplot2
# en vez de cajas de texto de ggtext (element_textbox_simple).
# La leyenda identifica las categorías con los mismos colores de los mapas.


# -----------------------------------------------------------------------------
# 5. Gráfico exploratorio (cuatro momentos, un solo PCA común)
# -----------------------------------------------------------------------------

# Las figuras y datos de los cuatro paneles siguen siendo los originales.
# Cambios: paleta, transparencia, jerarquía textual, subtítulo y notas.
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
    # Recuperamos el subtítulo del exploratorio original. Los saltos de línea
    # evitan que el texto se corte en el JPG exportado.
    subtitle = paste0(
      "El perfil joven tiene más fecundidad y crecimiento poblacional que el avanzado,\n",
      "y menos población de 65+ y esperanza de vida. Los perfiles se definen en cada año\n",
      "en relación con el resto de los países, mientras la posición usa una escala común.\n",
      "El triángulo representa la mediana del grupo. Se señalan países de ejemplo\n",
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
    
    # Textos de ggplot2 con saltos de línea explícitos.
    # A diferencia de ggtext, no necesitan una caja con ancho relativo
    # al panel de RStudio: evita el error "Viewport has zero dimension(s)".
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
    
    # Cada panel sigue teniendo su encabezado propio
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

# No usamos print() acá: source() puede ejecutarse con el panel Plots
# minimizado, y el dispositivo puede tener dimensiones cero.
# El archivo JPG se genera directamente con ggsave() a tamaño fijo.

# Guardado para instancia 3: entre 20 y 25 cm de ancho, mínimo 300 dpi.
# No alteramos ningún dato ni re-estimamos los clusters.
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)

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

message("Gráfico exploratorio guardado en: ", archivo_exploratorio)
# Abrir el JPG exportado: el panel Plots puede mostrar proporciones distintas.
