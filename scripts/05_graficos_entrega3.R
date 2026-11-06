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
    plot.subtitle = element_textbox_simple(
      color = gris_texto,
      size = rel(0.9),
      lineheight = 1.2,
      width = unit(1, "npc"),
      margin = margin(b = 10)
    ),
    
    plot.caption = element_textbox_simple(
      color = gris_suave,
      size = rel(0.8),
      lineheight = 1.1,
      width = unit(1, "npc"),
      halign = 0,
      margin = margin(t = 10)
    ),
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

library(tidyverse)

library(tidyverse)

# ============================================================
# 1. PREPARAR LOS DATOS
# ============================================================

# Recuperar la posición y el perfil de cada país en 1980 y 2024

cambios_pca <- puntos |>
  filter(anio %in% c(1980, 2024)) |>
  select(codigo_pais, pais, anio, PC1, perfil) |>
  pivot_wider(
    names_from = anio,
    values_from = c(PC1, perfil),
    names_sep = "_"
  ) |>
  mutate(
    # Cambio en la posición del PCA
    cambio_pc1 = PC1_2024 - PC1_1980,
    
    # Clasificación de las transiciones
    transicion = case_when(
      perfil_1980 == "joven" &
        perfil_2024 == "joven" ~ "Joven → Joven",
      
      perfil_1980 == "joven" &
        perfil_2024 == "avanzado" ~ "Joven → Avanzado",
      
      perfil_1980 == "avanzado" &
        perfil_2024 == "avanzado" ~ "Avanzado → Avanzado",
      
      perfil_1980 == "avanzado" &
        perfil_2024 == "joven" ~ "Avanzado → Joven"
    )
  )


# Preparar las categorías del gráfico

datos_grafico <- cambios_pca |>
  mutate(
    grupo = factor(
      transicion,
      levels = c(
        "Avanzado → Joven",
        "Avanzado → Avanzado",
        "Joven → Avanzado",
        "Joven → Joven"
      ),
      labels = c(
        "Avanzado → Joven (1)",
        "Avanzado → Avanzado (76)",
        "Joven → Avanzado (34)",
        "Joven → Joven (83)"
      )
    ),
    
    destacado = case_when(
      cambio_pc1 < 0 ~ "Disminución del PC1",
      transicion == "Joven → Avanzado" ~ "Joven a avanzado",
      TRUE ~ "Otros países"
    )
  )

# 2. Definir los colores

colores <- c(
  "Otros países" = "#AEB9C5",
  "Joven a avanzado" = "#D96B3B",
  "Disminución del PC1" = "#305D83"
)


# 3. Construir el gráfico

grafico_cambio_pca <- ggplot(
  datos_grafico,
  aes(x = cambio_pc1, y = grupo)
) +
  
  # Línea de referencia: ausencia de cambio
  geom_vline(
    xintercept = 0,
    color = "#444444",
    linetype = "dashed",
    linewidth = 0.6
  ) +
  
  # Un punto por país
  geom_jitter(
    aes(color = destacado),
    width = 0,
    height = 0.17,
    size = 2.4,
    alpha = 0.75
  ) +
  
  # Etiquetar los dos países con cambio negativo
  ggrepel::geom_text_repel(
    data = filter(datos_grafico, cambio_pc1 < 0),
    aes(label = pais),
    size = 3.4,
    color = "#305D83",
    fontface = "bold",
    max.overlaps = Inf,
    seed = 10,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = colores,
    name = NULL
  ) +
  
  scale_x_continuous(
    labels = scales::label_number(
      decimal.mark = ",",
      accuracy = 0.1
    )
  ) +
  
  labs(
    title = paste(
      "192 de 194 países se desplazaron hacia",
      "perfiles demográficos más envejecidos"
    ),
    
    subtitle = paste(
      "Cambio en el primer componente del PCA entre 1980 y 2024.",
      "Cada punto es un país, agrupado según su clasificación",
      "demográfica en ambos años."
    ),
    
    x = "Cambio en el componente 1 del PCA (2024 − 1980)",
    y = NULL,
    
    caption = paste(
      "Fuente: elaboración propia con World Development Indicators, Banco Mundial.",
      "Valores positivos del PC1 indican un desplazamiento hacia características",
      "asociadas con mayor envejecimiento. Los clusters son relativos a cada año."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      color = "#363636",
      margin = margin(b = 8)
    ),
    
    plot.subtitle = element_text(
      size = 11,
      color = "#5B5B5B",
      margin = margin(b = 15)
    ),
    
    axis.text.y = element_text(
      size = 11,
      face = "bold",
      color = "#363636"
    ),
    
    axis.title.x = element_text(
      size = 11,
      color = "#5B5B5B",
      margin = margin(t = 10)
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    panel.grid.major.x = element_line(
      color = "#EAEAEA",
      linewidth = 0.3
    ),
    
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    
    plot.caption = element_text(
      size = 9,
      color = "#858585",
      hjust = 0,
      margin = margin(t = 15)
    ),
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(15, 20, 15, 15)
  )


# 4. Mostrar el gráfico

grafico_cambio_pca

install.packages("ggalluvial")

library(tidyverse)
library(ggalluvial)

# Seguimos a los países jóvenes de 1980

evolucion_jovenes <- trayectorias_paises |>
  filter(cluster_1980 == 1) |>
  select(pais, starts_with("cluster_")) |>
  pivot_longer(
    cols = starts_with("cluster_"),
    names_to = "anio",
    values_to = "cluster",
    names_prefix = "cluster_",
    names_transform = list(anio = as.integer)
  ) |>
  group_by(anio) |>
  summarise(
    jovenes = sum(cluster == 1, na.rm = TRUE),
    avanzados = sum(cluster == 2, na.rm = TRUE),
    faltantes = sum(is.na(cluster)),
    .groups = "drop"
  ) |>
  mutate(
    porcentaje_avanzados = round(
      avanzados / (jovenes + avanzados) * 100, 1
    )
  )

library(tidyverse)

# 1. Verificamos que la clave de unión sea única en ambas tablas

puntos |>
  count(codigo_pais, anio) |>
  filter(n > 1)

wdi_limpia |>
  filter(anio %in% c(1980, 1995, 2010, 2024)) |>
  count(codigo_pais, anio) |>
  filter(n > 1) 

mediana_65_perfil <- puntos |>
  select(codigo_pais, anio, perfil) |>
  left_join(
    wdi_limpia |>
      select(codigo_pais, anio, poblacion_65_mas),
    by = c("codigo_pais", "anio")
  ) |>
  filter(anio %in% c(1980, 1995, 2010, 2024)) |>
  group_by(anio, perfil) |>
  summarise(
    mediana_65 = median(poblacion_65_mas, na.rm = TRUE),
    cantidad_paises = n(),
    .groups = "drop"
  ) |>
  mutate(
    perfil = factor(
      perfil,
      levels = c("joven", "avanzado"),
      labels = c("Perfil joven", "Perfil avanzado")
    )
  ) |>
  arrange(perfil, anio)

mediana_65_perfil


library(ggrepel)

colores_anio <- c(
  "1980" = "#9DB4D3",
  "1995" = "#C7D3E2",
  "2010" = "#E8A06F",
  "2024" = "#D96B3B"
)

grafico_65_perfil <- ggplot(
  mediana_65_perfil,
  aes(
    x = mediana_65,
    y = perfil,
    group = perfil
  )
) +
  
  # línea que conecta los cuatro períodos dentro de cada perfil
  geom_line(
    color = "#8A8A8A",
    linewidth = 1
  ) +
  
  # puntos de cada año
  geom_point(
    aes(fill = factor(anio)),
    shape = 21,
    size = 5,
    color = "white",
    stroke = 0.8
  ) +
  
  # etiquetas con año y valor
  ggrepel::geom_text_repel(
    aes(
      label = paste0(
        anio, ": ",
        round(mediana_65, 1), "%"
      ),
      color = factor(anio)
    ),
    size = 4,
    direction = "both",
    box.padding = 0.25,
    point.padding = 0.2,
    segment.color = "#B0B0B0",
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = colores_anio,
    name = "Año"
  ) +
  
  scale_color_manual(
    values = colores_anio,
    guide = "none"
  ) +
  
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  labs(
    title = "La proporción de población de 65+ aumentó en ambos perfiles demográficos",
    subtitle = paste(
      "Mediana de población de 65+ dentro de los perfiles joven y avanzado",
      "en 1980, 1995, 2010 y 2024."
    ),
    x = "Mediana de población de 65+",
    y = NULL,
    caption = paste(
      "Fuente: elaboración propia con WDI, Banco Mundial.",
      "Los perfiles se obtuvieron mediante clustering sobre variables demográficas",
      "y son relativos a cada año."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      color = "#363636",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#5B5B5B",
      margin = margin(b = 18)
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      color = "#363636"
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#5B5B5B",
      margin = margin(t = 10)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.caption = element_text(
      size = 9,
      color = "#858585",
      hjust = 0,
      margin = margin(t = 15)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

###

library(tidyverse)

# ============================================================
# GRÁFICO COMUNICATIVO: BRECHA ENTRE PERFILES DEMOGRÁFICOS
# ============================================================

# 1. Preparar las medianas para las líneas

medianas_brechas <- mediana_65_perfil |>
  mutate(
    tipo = if_else(
      perfil == "Perfil joven",
      "joven",
      "avanzado"
    )
  ) |>
  select(anio, tipo, mediana_65) |>
  pivot_wider(
    names_from = tipo,
    values_from = mediana_65
  ) |>
  mutate(
    anio_grafico = factor(
      anio,
      levels = c(2024, 2010, 1995, 1980)
    ),
    brecha = avanzado - joven
  )

# 2. Preparar los puntos y sus etiquetas

puntos_brechas <- mediana_65_perfil |>
  mutate(
    anio_grafico = factor(
      anio,
      levels = c(2024, 2010, 1995, 1980)
    ),
    etiqueta = paste0(
      formatC(
        mediana_65,
        format = "f",
        digits = 1,
        decimal.mark = ","
      ),
      "%"
    )
  )

# 3. Colores de los perfiles

colores_perfiles <- c(
  "Perfil joven" = "#8FA6CC",
  "Perfil avanzado" = "#A05961"
)

# 4. Construir el gráfico

library(tidyverse)

# ============================================================
# RESUMEN DE POBLACIÓN 65+ POR PERFIL Y AÑO
# ============================================================

resumen_65_perfil <- puntos |>
  select(codigo_pais, anio, perfil) |>
  left_join(
    wdi_limpia |>
      select(codigo_pais, anio, poblacion_65_mas),
    by = c("codigo_pais", "anio")
  ) |>
  filter(anio %in% c(1980, 1995, 2010, 2024)) |>
  group_by(anio, perfil) |>
  summarise(
    mediana_65 = median(poblacion_65_mas, na.rm = TRUE),
    q1_65 = quantile(poblacion_65_mas, 0.25, na.rm = TRUE),
    q3_65 = quantile(poblacion_65_mas, 0.75, na.rm = TRUE),
    cantidad_paises = n(),
    .groups = "drop"
  ) |>
  mutate(
    perfil = factor(
      perfil,
      levels = c("joven", "avanzado"),
      labels = c("Perfil joven", "Perfil avanzado")
    )
  ) |>
  arrange(anio, perfil)

resumen_65_perfil

# ============================================================
# PREPARAR TABLAS PARA EL GRÁFICO
# ============================================================

# Tabla ancha para dibujar la brecha entre medianas
brechas_iqr <- resumen_65_perfil |>
  mutate(
    tipo = if_else(
      perfil == "Perfil joven",
      "joven",
      "avanzado"
    )
  ) |>
  select(anio, tipo, mediana_65, q1_65, q3_65) |>
  pivot_wider(
    names_from = tipo,
    values_from = c(mediana_65, q1_65, q3_65)
  ) |>
  mutate(
    anio_grafico = factor(
      anio,
      levels = c(2024, 2010, 1995, 1980)
    ),
    brecha = mediana_65_avanzado - mediana_65_joven
  )

# Etiquetas de brecha para 1980 y 2024
etiquetas_brecha <- brechas_iqr |>
  filter(anio %in% c(1980, 2024)) |>
  mutate(
    punto_medio = (mediana_65_joven + mediana_65_avanzado) / 2,
    etiqueta = paste0(
      "+",
      formatC(
        brecha,
        format = "f",
        digits = 1,
        decimal.mark = ","
      ),
      " pp"
    )
  )

brechas_iqr

# ============================================================
# GRÁFICO COMUNICATIVO FINAL CON RANGO INTERCUARTÍLICO
# ============================================================

azul <- "#7399C5"
bordo <- "#A2565F"
gris <- "#929292"

grafico_brechas_iqr <- ggplot() +
  
  # ----------------------------------------------------------
# 1. Rango intercuartílico del perfil joven
# ----------------------------------------------------------
geom_segment(
  data = brechas_iqr,
  aes(
    x = q1_65_joven,
    xend = q3_65_joven,
    y = anio_grafico,
    yend = anio_grafico
  ),
  color = azul,
  linewidth = 6,
  alpha = 0.25,
  lineend = "round"
) +
  
  # ----------------------------------------------------------
# 2. Rango intercuartílico del perfil avanzado
# ----------------------------------------------------------
geom_segment(
  data = brechas_iqr,
  aes(
    x = q1_65_avanzado,
    xend = q3_65_avanzado,
    y = anio_grafico,
    yend = anio_grafico
  ),
  color = bordo,
  linewidth = 6,
  alpha = 0.25,
  lineend = "round"
) +
  
  # ----------------------------------------------------------
# 3. Línea que une las medianas de ambos perfiles
# ----------------------------------------------------------
geom_segment(
  data = brechas_iqr,
  aes(
    x = mediana_65_joven,
    xend = mediana_65_avanzado,
    y = anio_grafico,
    yend = anio_grafico
  ),
  color = gris,
  linewidth = 1.05
) +
  
  # ----------------------------------------------------------
# 4. Puntos de las medianas
# ----------------------------------------------------------
geom_point(
  data = brechas_iqr,
  aes(x = mediana_65_joven, y = anio_grafico),
  color = azul,
  size = 4.8
) +
  
  geom_point(
    data = brechas_iqr,
    aes(x = mediana_65_avanzado, y = anio_grafico),
    color = bordo,
    size = 4.8
  ) +
  
  # ----------------------------------------------------------
# 5. Etiquetas de las medianas
# ----------------------------------------------------------
geom_text(
  data = brechas_iqr,
  aes(
    x = mediana_65_joven,
    y = anio_grafico,
    label = paste0(
      formatC(
        mediana_65_joven,
        format = "f",
        digits = 1,
        decimal.mark = ","
      ),
      "%"
    )
  ),
  nudge_x = -0.30,
  hjust = 1,
  color = azul,
  fontface = "bold",
  size = 4.6
) +
  
  geom_text(
    data = brechas_iqr,
    aes(
      x = mediana_65_avanzado,
      y = anio_grafico,
      label = paste0(
        formatC(
          mediana_65_avanzado,
          format = "f",
          digits = 1,
          decimal.mark = ","
        ),
        "%"
      )
    ),
    nudge_x = 0.30,
    hjust = 0,
    color = bordo,
    fontface = "bold",
    size = 4.6
  ) +
  
  # ----------------------------------------------------------
# 6. Etiquetas de brecha
# ----------------------------------------------------------
geom_text(
  data = filter(etiquetas_brecha, anio == 1980),
  aes(
    x = punto_medio,
    y = anio_grafico,
    label = etiqueta
  ),
  nudge_y = 0.23,
  color = "#777777",
  fontface = "bold",
  size = 4
) +
  
  geom_text(
    data = filter(etiquetas_brecha, anio == 2024),
    aes(
      x = punto_medio,
      y = anio_grafico,
      label = etiqueta
    ),
    nudge_y = 0.23,
    color = bordo,
    fontface = "bold",
    size = 5
  ) +
  
  # ----------------------------------------------------------
# 7. Escalas
# ----------------------------------------------------------
scale_x_continuous(
  breaks = c(0, 4, 8, 12, 16),
  labels = function(x) paste0(x, "%"),
  limits = c(0, 17.5),
  expand = c(0, 0)
) +
  
  scale_y_discrete(
    expand = expansion(add = c(0.35, 0.35))
  ) +
  
  # ----------------------------------------------------------
# 8. Textos
# ----------------------------------------------------------
labs(
  title = paste0(
    "La brecha en población de 65+ entre perfiles ",
    "pasó de 6,5 a 11 puntos porcentuales"
  ),
  
  subtitle = paste0(
    "En el perfil avanzado (bordó), la mediana pasó de 9,7% a 14,6%; ",
    "en el joven (azul), se mantuvo cerca del 3%.\n",
    "Las bandas muestran el rango intercuartílico ",
    "(50% central de países) en cada perfil. ",
    "194 países por año, 1980–2024."
  ),
  
  x = "Población de 65 años o más (% del total)",
  y = "Año",
  
  caption = paste0(
    "Fuente: elaboración propia con World Development Indicators, Banco Mundial.\n",
    "Nota: medianas y cuartiles entre países, sin ponderar. ",
    "Perfiles estimados por separado en cada año, con composición variable. ",
    "Brechas calculadas sin redondear."
  )
) +
  
  # ----------------------------------------------------------
# 9. Tema
# ----------------------------------------------------------
theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      color = "#303030",
      lineheight = 1.1,
      margin = margin(b = 9)
    ),
    
    plot.subtitle = element_text(
      size = 11.2,
      color = "#666666",
      lineheight = 1.2,
      margin = margin(b = 16)
    ),
    
    axis.text.y = element_text(
      size = 13,
      face = "bold",
      color = "#363636",
      margin = margin(r = 12)
    ),
    
    axis.title.y = element_text(
      size = 11,
      color = "#666666",
      margin = margin(r = 10)
    ),
    
    axis.text.x = element_text(
      size = 10.5,
      color = "#777777",
      margin = margin(t = 7)
    ),
    
    axis.title.x = element_text(
      size = 11,
      color = "#666666",
      margin = margin(t = 12)
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_line(
      color = "#F0F0F0",
      linewidth = 0.25
    ),
    
    legend.position = "none",
    
    plot.caption = element_text(
      size = 9,
      color = "#858585",
      hjust = 0,
      lineheight = 1.2,
      margin = margin(t = 16)
    ),
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(18, 20, 15, 18)
  )

grafico_brechas_iqr

ggsave(
  filename = "grupo_10_comunicativo_brechas_iqr.jpg",
  plot = grafico_brechas_iqr,
  width = 24,
  height = 13,
  units = "cm",
  dpi = 300,
  bg = "white",
  quality = 95
)