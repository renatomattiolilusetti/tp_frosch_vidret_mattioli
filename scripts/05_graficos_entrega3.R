# =============================================================================
# Grupo 10 · Instancia 3 · Gráfico EXPLORATORIO
# Los perfiles demográficos en 1980, 1995, 2010 y 2024 sobre UN MISMO plano
#
# Se corre DESPUÉS de 03.02_clustering.R: usa `base_cluster_demografico`
# (ya sin PSE) y `trayectorias_paises` (cluster de cada país en cada año).
#
# En 03.02 el PCA se estimó por separado en cada año, así que sus ejes no son
# comparables entre años. Acá estimamos UN PCA con los cuatro años apilados
# (194 países × 4) para que los paneles compartan ejes. Los colores siguen
# siendo los clusters de 03.02: no se re-estiman.
# =============================================================================

paquetes <- c("tidyverse", "FactoMineR", "ggtext", "ggrepel", "scales")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) install.packages(faltan)

suppressWarnings(suppressPackageStartupMessages({
  library(tidyverse)
  library(FactoMineR)
  library(ggtext)
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

tipicos <- puntos |>
  filter(codigo_pais %in% names(conocidos)) |>
  inner_join(trayectoria_tipo, by = "pais") |>
  left_join(select(resumen, anio, perfil, mx = x, my = y), by = c("anio", "perfil")) |>
  mutate(dist_cm = sqrt(((PC1 - mx) * escala_x)^2 + ((PC2 - my) * escala_y)^2)) |>
  group_by(patron, pais, codigo_pais) |>
  summarise(distancia = sum(dist_cm), minima = min(dist_cm),
            anios = n(), .groups = "drop") |>
  filter(anios == 4, minima >= separacion_cm) |>
  group_by(patron) |>
  slice_min(distancia, n = 1) |>
  ungroup()
tipicos   # MIRAR: qué país quedó en cada trayectoria
if (nrow(tipicos) < 2) message("Algún grupo se quedó sin país: bajar `separacion_cm` (por ejemplo, a 0.4).")
# Para el perfil avanzado se señala Argentina (siempre en el cluster 2) en lugar
# del elegido automáticamente; el perfil joven sigue con el elegido por el criterio.
# Se suma Perú como ejemplo de un país que cambia de perfil.
tipicos <- bind_rows(
  tipicos |> filter(patron == "1-1-1-1") |> select(codigo_pais),
  tibble(codigo_pais = c("ARG", "PER"))   # Perú: pasó de joven a avanzado entre 1995 y 2010
)
tipicos

etiquetas <- puntos |>
  filter(codigo_pais %in% tipicos$codigo_pais) |>
  mutate(nombre = unname(conocidos[codigo_pais]))

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


# -----------------------------------------------------------------------------
# 4. Estilo
# -----------------------------------------------------------------------------

# Tipografías: las de R por defecto (serif para el título, sans para el resto).
# Sin showtext, que pegaba las palabras del título con colores, y sin instalar
# nada extra.
# Misma tipografía (sans, Arial en Windows) para el título y para el resto del texto
fuente_titulo <- "sans"
fuente_texto  <- "sans"

# Si showtext quedó activado de una corrida anterior (o del script de la clase),
# lo apagamos: si no, el texto sale diminuto en el JPG.
if ("showtext" %in% loadedNamespaces()) showtext::showtext_auto(FALSE)

col_perfil  <- c(joven = "#8FA6CC", avanzado = "#A05961")   # azul claro / terracota: se distinguen en grises
gris_titulo <- "#363636"; gris_texto <- "#5B5B5B"; gris_suave <- "#858585"
pintar <- function(texto, color) sprintf("<span style='color:%s'><b>%s</b></span>", color, texto)
coma   <- \(x, d = 0) formatC(x, format = "f", digits = d, decimal.mark = ",")



# -----------------------------------------------------------------------------
# 5. Gráfico
# -----------------------------------------------------------------------------

grafico_exploratorio <- ggplot(puntos, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, color = "#9A9A9A", linewidth = 0.4) +
  # el 0 del eje x, bien marcado: ayuda a ver cómo todo se corre a la derecha
  geom_vline(xintercept = 0, color = "#3A3A3A", linewidth = 0.8) +
  # países: círculos con un borde blanco finito, para que se separen cuando se pisan
  geom_point(aes(fill = perfil), shape = 21, size = 2.6, alpha = 0.55,
             color = "white", stroke = 0.25) +
  # países típicos: más grandes, borde negro y nombre en cada panel
  geom_point(data = etiquetas, aes(fill = perfil), shape = 21, size = 3.8,
             color = "black", stroke = 0.8) +
  geom_text_repel(data = filter(etiquetas, !abajo), aes(label = nombre), family = fuente_texto,
                  fontface = "bold", size = 3.3, color = gris_titulo,
                  segment.color = NA, min.segment.length = Inf, nudge_y = 0.6, box.padding = 0.2, seed = 1) +
  geom_text_repel(data = filter(etiquetas, abajo), aes(label = nombre), family = fuente_texto,
                  fontface = "bold", size = 3.3, color = gris_titulo,
                  segment.color = NA, min.segment.length = Inf, nudge_y = -0.6, box.padding = 0.2, seed = 1) +
  # extremos de cada año: anillo gris oscuro y texto en cursiva, distinto de los ejemplos
  geom_point(data = extremos, shape = 21, fill = NA, color = gris_titulo,
             size = 4.4, stroke = 0.8) +
  geom_text_repel(data = filter(extremos, !abajo), aes(label = texto), family = fuente_texto,
                  fontface = "italic", size = 3, color = gris_texto,
                  segment.color = NA, min.segment.length = Inf, nudge_y = 0.7,
                  box.padding = 0.2, seed = 2) +
  geom_text_repel(data = filter(extremos, abajo), aes(label = texto), family = fuente_texto,
                  fontface = "italic", size = 3, color = gris_texto,
                  segment.color = NA, min.segment.length = Inf, nudge_y = -0.7,
                  box.padding = 0.2, seed = 2) +
  # mediana del grupo: triángulo 1,5 veces el tamaño de los círculos, borde negro grueso
  geom_point(data = resumen, aes(x, y, fill = perfil), inherit.aes = FALSE,
             shape = 24, size = 3.9, color = "black", stroke = 1.1) +
  facet_wrap(~ rotulo, ncol = 1) +
  scale_color_manual(values = col_perfil) +
  scale_fill_manual(values = col_perfil) +
  scale_x_continuous(breaks = seq(-6, 6, 2),
                     labels = label_number(decimal.mark = ",", big.mark = "."),
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = c(-2, 0, 2)) +
  coord_cartesian(xlim = lim_x, ylim = lim_y) +
  labs(
    title = paste0("Distintas trayectorias del envejecimiento: dinámica de perfiles ",
                   pintar("jóvenes", col_perfil[["joven"]]), " y ",
                   pintar("avanzados", col_perfil[["avanzado"]]),
                   " de 194 países en 1980, 1995, 2010 y 2024"),
    # Subtítulo y nota se escriben como un solo párrafo: element_textbox_simple()
    # (en el tema) los corta solo, llenando cada renglón hasta el borde derecho.
    subtitle = paste(
      "El perfil joven tiene más fecundidad y crecimiento poblacional que el avanzado, y menos",
      "población de 65+ y esperanza de vida. Los perfiles se definen en cada año en relación con el",
      "resto de los países, mientras la posición usa una escala común. El triángulo representa la",
      "mediana del grupo. Se señalan países de ejemplo y, con un anillo, el más joven y el más envejecido de cada año."
    ),
    x = paste0("Componente 1: perfiles más envejecidos\n",
               "(hacia la derecha, más población de 65+ y esperanza de vida, menos fecundidad y crecimiento)"),
    y = "Componente 2: crecimiento poblacional\n(arriba, mayor crecimiento)",
    caption = paste0(
      "Fuente: elaboración propia en base a World Development Indicators, Banco Mundial. ",
      "Posición: PCA conjunto de los cuatro años sobre población de 65+, fecundidad, esperanza de vida ",
      "y crecimiento poblacional. Perfiles: clusters jerárquicos estimados en cada año. ",
      "Para facilitar la lectura, el eje vertical deja fuera de vista ", n_fuera,
      " observaciones extremas."
    )
  ) +
  theme_minimal(base_size = 11, base_family = fuente_texto) +
  theme(
    plot.title.position = "plot", plot.caption.position = "plot",
    # element_textbox_simple corta el título solo, llenando el ancho del gráfico
    # (igual que el subtítulo y la nota), y mantiene los colores
    plot.title    = element_textbox_simple(family = fuente_titulo, size = rel(1.55), color = gris_titulo,
                                           lineheight = 1.1, width = unit(1, "npc"),
                                           margin = margin(b = 6)),
    plot.subtitle = element_textbox_simple(color = gris_texto, size = rel(0.9), lineheight = 1.2,
                                           width = unit(1, "npc"), margin = margin(b = 10)),
    plot.caption  = element_textbox_simple(color = gris_suave, size = rel(0.8), lineheight = 1.1,
                                           width = unit(1, "npc"), halign = 0,
                                           margin = margin(t = 10)),
    strip.text    = element_text(face = "bold", hjust = 0, color = gris_titulo,
                                 size = rel(0.95), margin = margin(t = 2, b = 3)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#EDEDED", linewidth = 0.3),
    panel.spacing.y    = unit(12, "pt"),
    axis.title    = element_text(color = gris_texto, size = rel(0.85)),
    axis.title.x  = element_text(margin = margin(t = 6)),
    axis.title.y  = element_text(margin = margin(r = 6)),
    axis.text     = element_text(color = "#777777", size = rel(0.85)),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(14, 18, 10, 14)
  )

grafico_exploratorio

# 22 cm de ancho y 300 dpi, como pide la consigna (20-25 cm, mínimo 300 dpi).
# quality = 95 evita que la compresión del JPG ensucie las letras.
ggsave("grupo_10_exploratorio.jpg", grafico_exploratorio,
       width = 22, height = 24, units = "cm", dpi = 300, bg = "white", quality = 95)

# Revisar en el JPG al 100%:
# - si los paneles quedan muy bajos, subir height (26-28) o bajar el texto;
# - si las etiquetas de países molestan, sacarlas (son opcionales).