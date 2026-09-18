
wdi_limpia <- read_csv("C:/Users/renam/OneDrive/Escritorio/tp_frosch_vidret_mattioli/repo_wdi/wdi_limpia.csv")

################################################################################
# Para el conocimiento general de la base
################################################################################

#Contamos dimension
dim(wdi_limpia)  


#Contamos si tenemos filas duplicadas
wdi_limpia |>
  count(codigo_pais, anio) |>
  filter(n > 1)


#Contamos países, primer año, último año y cantidad de años de las observaciones
wdi_limpia |>
  summarise(
    cantidad_paises = n_distinct(codigo_pais),
    primer_anio = min(anio),
    ultimo_anio = max(anio),
    cantidad_anios = n_distinct(anio)
  )


#Contamos columnas (como las variables que precisamos para las pirámides explican lo mismo pero en diferentes rangos,
#nos quedamos solo con 2, por eso sumamos despues del ncol())
wdi_limpia |>
  select(-(35:68), -(1:4)) |>
  ncol() + 2 


################################################################################
# Para el análisis de los NA
################################################################################


#Contamos los faltantes por variable
faltantes_variables <- tibble(
  variable = names(wdi_limpia),
  cantidad_na = colSums(is.na(wdi_limpia)),
  total_observaciones = nrow(wdi_limpia)
) |>
  mutate(
    porcentaje_na = cantidad_na / total_observaciones * 100
  ) |>
  filter(porcentaje_na >= 5) |> 
  arrange(porcentaje_na, variable) |>
  mutate(
    porcentaje_na = round(porcentaje_na, 2)
  )


#Para hacer la tabla_na_region (contamos solo esas columnas porque las otras arrojaban NA=0)
variables_analisis <- names(wdi_limpia)[1:34]

variables_analisis <- setdiff(
  variables_analisis,
  "region"
)

tabla_na_region <- wdi_limpia |>
  group_by(region) |>
  summarise(
    across(
      all_of(variables_analisis),
      ~ round(mean(is.na(.)) * 100, 2)
    ) 
  ) |> 
  select(region,
    where(~ is.numeric(.) && any(. >= 5, na.rm = TRUE))
    ) #Porque aca es obvio que va a tirar NA de 0 (por características de la base)


#Vamos a armar mapas de calor para ver cuándo se concentran los NA a lo largo de las décadas, 
#para cada región. 


variables_candidatas <- setdiff(
  names(tabla_na_region),
  c("region", "pais", "codigo_pais", "anio")
)

variables_mapa <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  group_by(region) |>
  summarise(
    across(
      all_of(variables_candidatas),
      ~ mean(is.na(.)) * 100
    ),
    .groups = "drop"
  ) |>
  select(all_of(variables_candidatas)) |>
  select(where(~ any(. >= 5))) |>
  names()

na_decadas <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  mutate(
    decada = cut(
      anio,
      breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2026),
      labels = c(
        "1960–1969", "1970–1979", "1980–1989",
        "1990–1999", "2000–2009", "2010–2019",
        "2020–2025"
      ),
      right = FALSE
    )
  ) |>
  group_by(region, decada) |>
  summarise(
    across(
      all_of(variables_mapa),
      ~ mean(is.na(.)) * 100
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = all_of(variables_mapa),
    names_to = "variable",
    values_to = "porcentaje_na"
  )

# MAPA DE CALOR PARA PRESENTACIONES ###########################################

# MAPAS DE LAS SIETE REGIONES EN UNA SOLA IMAGEN ###############################

# Etiquetas más cortas para aprovechar el espacio
etiquetas_variables <- c(
  participacion_laboral = "Participación laboral",
  tasa_empleo = "Empleo",
  tasa_desempleo = "Desempleo",
  participacion_femenina = "Participación femenina",
  pib_por_ocupado = "PIB por ocupado",
  pib_per_capita_ppa = "PIB per cápita PPA",
  gasto_salud_pib = "Gasto en salud",
  gasto_publico_salud_pib = "Gasto público en salud",
  gasto_bolsillo_salud = "Gasto de bolsillo",
  gasto_educacion_pib = "Gasto en educación",
  gasto_por_alumno_primaria = "Gasto/alumno primaria",
  gasto_por_alumno_secundaria = "Gasto/alumno secundaria",
  cobertura_proteccion_social = "Protección social",
  cobertura_seguros_sociales = "Seguros sociales",
  mortalidad_menores_5 = "Mortalidad <5 años"
)

etiquetar_variables <- function(x) {
  etiquetas <- unname(etiquetas_variables[x])
  
  # Para otras variables, usar su nombre sin guiones bajos
  faltan <- is.na(etiquetas)
  etiquetas[faltan] <- gsub("_", " ", x[faltan])
  
  stringr::str_wrap(etiquetas, width = 23)
}

grafico_mapas_na <- ggplot(
  na_decadas,
  aes(x = decada, y = variable, fill = porcentaje_na)
) +
  geom_tile(
    width = 0.96,
    height = 0.90,
    color = "white",
    linewidth = 0.2
  ) +
  facet_wrap(
    ~region,
    ncol = 4,
    labeller = label_wrap_gen(width = 22),
    axes = "all_x",
    axis.labels = "all_x"
  ) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    name = "Datos faltantes (%)"
  ) +
  scale_x_discrete(
    # Mostrar el año de inicio de cada período
    labels = function(x) substr(x, 1, 4),
    drop = FALSE,
    expand = expansion(add = 0.05)
  ) +
  scale_y_discrete(
    limits = rev(variables_mapa),
    labels = etiquetar_variables,
    expand = expansion(add = 0.05)
  ) +
  labs(
    title = "Disponibilidad de datos por región y período",
    x = "Inicio del período",
    y = "Variable",
    caption = paste(
      "Porcentaje de observaciones país-año sin datos.",
      "Períodos de diez años, excepto 2020-2025 (seis años)."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    
    plot.title = element_text(
      size = 21,
      face = "bold",
      margin = margin(b = 10)
    ),
    
    strip.text = element_text(
      size = 11,
      face = "bold",
      margin = margin(b = 6)
    ),
    
    axis.text.x = element_text(
      size = 9,
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      color = "gray20"
    ),
    axis.text.y = element_text(
      size = 10,
      color = "gray20",
      lineheight = 0.9
    ),
    axis.title = element_text(size = 12),
    
    panel.spacing.x = grid::unit(0.5, "cm"),
    panel.spacing.y = grid::unit(0.5, "cm"),
    
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    
    plot.caption = element_text(size = 10, hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(7, "cm"),
      barheight = grid::unit(0.3, "cm")
    )
  )

print(grafico_mapas_na)

# Guardar una sola imagen horizontal 16:9 en Descargas
ggsave(
  filename = path.expand("~/Downloads/mapas_na_todas_las_regiones.png"),
  plot = grafico_mapas_na,
  width = 16,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

#Regiones para copiar y pegar y reproducir todos los gráficos:
# Oriente Medio, Norte de África, Afganistán y Pakistán
# África Subsahariana
# América del Norte
# América Latina y el Caribe
# Asia Meridional
# Asia Oriental y Pacífico 
# Europa y Asia Central

################################################################################
# Gráficos y análisis de variables
################################################################################

# Gráfico con poblacion_total ##################################################

# Población total por región y año
poblacion_region <- wdi_limpia |>
  filter(
    between(anio, 1960, 2025),
    !is.na(region),
    region != ""
  ) |>
  group_by(anio, region) |>
  summarise(
    # Si todos los datos faltan, conservar NA en lugar de generar un cero
    poblacion = if (all(is.na(poblacion_total))) {
      NA_real_
    } else {
      sum(poblacion_total, na.rm = TRUE)
    },
    .groups = "drop"
  )

# Gráfico de líneas por región
grafico_poblacion_region <- ggplot(
  poblacion_region,
  aes(
    x = anio,
    y = poblacion / 1e9,
    color = region
  )
) +
  geom_line(linewidth = 1.1) +
  facet_wrap(
    ~region,
    ncol = 3,
    scales = "free_y",
    labeller = label_wrap_gen(width = 32),
    axes = "all",
    axis.labels = "all"
  ) +
  scale_x_continuous(
    breaks = c(seq(1960, 2020, by = 10), 2025),
    limits = c(1960, 2025),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      decimal.mark = ",",
      big.mark = "."
    )
  ) +
  labs(
    title = "Evolución de la población por región",
    subtitle = "1960–2025 | Escala vertical diferente por región",
    x = "Año",
    y = "Población (miles de millones)"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

print(grafico_poblacion_region)

# Grafico sobre crecimiento natural ############################################

#Calcular pesos poblacionales por región y año
pesos_crecimiento_natural <- wdi_limpia |>
  filter(
    !is.na(region),
    region != "",
    region != "Aggregates",
    between(anio, 1960, 2025),
    !is.na(poblacion_total),
    poblacion_total > 0,
    !is.na(natalidad),
    !is.na(mortalidad)
  ) |>
  group_by(region, anio) |>
  mutate(
    poblacion_region_con_datos = sum(poblacion_total),
    peso_poblacional = poblacion_total / poblacion_region_con_datos
  ) |>
  ungroup()

#Calcular las tasas regionales ponderadas
crecimiento_natural_regional <- pesos_crecimiento_natural |>
  group_by(region, anio) |>
  summarise(
    natalidad_pct = sum(natalidad * peso_poblacional) / 10,
    mortalidad_pct = sum(mortalidad * peso_poblacional) / 10,
    .groups = "drop"
  ) |>
  mutate(
    crecimiento_natural_pct = natalidad_pct - mortalidad_pct
  )

#Pasar las dos tasas a formato largo para graficarlas
df_brecha_regional <- crecimiento_natural_regional |>
  select(region, anio, natalidad_pct, mortalidad_pct) |>
  pivot_longer(
    cols = c(natalidad_pct, mortalidad_pct),
    names_to = "tasa",
    values_to = "valor"
  )

#Graficar paneles por región
grafico_crecimiento_natural <- ggplot(
  df_brecha_regional,
  aes(x = anio, y = valor, color = tasa)
) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~region,
    ncol = 3,
    labeller = label_wrap_gen(width = 32),
    axes = "all_x",
    axis.labels = "all_x"
  ) +
  scale_x_continuous(
    breaks = c(1960, 1980, 2000, 2025),
    limits = c(1960, 2025),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_color_manual(
    values = c(
      natalidad_pct = "#2b5c8f",
      mortalidad_pct = "#d95f02"
    ),
    breaks = c("natalidad_pct", "mortalidad_pct"),
    labels = c("Natalidad (%)", "Mortalidad (%)")
  ) +
  labs(
    title = "Natalidad y mortalidad por región (1960–2025)",
    subtitle = "Tasas ponderadas por población de cada año",
    x = "Año",
    y = "Tasa (%)",
    color = "Componente",
    caption = paste(
      "La diferencia entre natalidad y mortalidad es el crecimiento natural.",
      "Se incluyen países con datos disponibles de ambas tasas y población."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

print(grafico_crecimiento_natural)

# Gráfico sobre tasa de crecimiento anual por regiones #########################

# Resume el crecimiento poblacional de cada país por década
crecimiento_por_decada <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  mutate(
    decada = cut(
      anio,
      breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2026),
      labels = c(
        "1960–1969", "1970–1979", "1980–1989",
        "1990–1999", "2000–2009", "2010–2019",
        "2020–2025"
      ),
      right = FALSE
    )
  ) |>
  group_by(pais, codigo_pais, region, decada) |>
  summarise(
    anios_con_dato = sum(!is.na(crecimiento_poblacional)),
    media = if (anios_con_dato > 0) {
      mean(crecimiento_poblacional, na.rm = TRUE)
    } else {
      NA_real_
    },
    mediana = if (anios_con_dato > 0) {
      median(crecimiento_poblacional, na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |>
  arrange(pais, decada)

##Media de la tasa de crecimiento poblacional ponderada por población##

# Calcular el peso poblacional de cada país en cada año
pesos_poblacionales <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  select(
    pais, codigo_pais, region, anio,
    poblacion_total, crecimiento_poblacional
  ) |>
  filter(
    !is.na(poblacion_total),
    poblacion_total > 0,
    !is.na(crecimiento_poblacional)
  ) |>
  group_by(region, anio) |>
  mutate(
    poblacion_region_con_datos = sum(poblacion_total),
    peso_poblacional = poblacion_total / poblacion_region_con_datos,
    porcentaje_poblacion = peso_poblacional * 100,
    aporte_crecimiento = peso_poblacional * crecimiento_poblacional
  ) |>
  ungroup()

# Sumar los aportes para obtener la media ponderada regional
crecimiento_regional_ponderado <- pesos_poblacionales |>
  group_by(region, anio) |>
  summarise(
    economias_con_dato = n(),
    suma_pesos = sum(peso_poblacional),
    crecimiento_ponderado = sum(aporte_crecimiento),
    .groups = "drop"
  )

# Graficar la evolución anual
grafico_crecimiento_ponderado <- ggplot(
  crecimiento_regional_ponderado,
  aes(
    x = anio,
    y = crecimiento_ponderado,
    color = region,
    group = region
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray60"
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(1960, 2020, by = 10),
    limits = c(1960, 2025)
  ) +
  labs(
    title = "Crecimiento poblacional por región ponderado",
    subtitle = "Media de las tasas anuales ponderada por población de cada año",
    x = "Año",
    y = "Crecimiento anual ponderado (%)",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(ncol = 1))

print(grafico_crecimiento_ponderado)

cobertura_crecimiento <- wdi_limpia |>
  filter(
    anio >= 1960, anio <= 2025,
    !is.na(poblacion_total),
    poblacion_total > 0
  ) |>
  group_by(region, anio) |>
  summarise(
    economias_con_poblacion = n(),
    economias_con_ambos_datos = sum(!is.na(crecimiento_poblacional)),
    cobertura_poblacion = 100 *
      sum(poblacion_total[!is.na(crecimiento_poblacional)]) /
      sum(poblacion_total),
    .groups = "drop"
  )

# Comparar la tasa de crecimiento entre el período inicial y el reciente
comparacion_crecimiento <- crecimiento_regional_ponderado |>
  filter(
    between(anio, 1961, 1970) |
      between(anio, 2016, 2025)
  ) |>
  mutate(
    periodo = if_else(
      anio <= 1970,
      "1961_1970",
      "2016_2025"
    )
  ) |>
  group_by(region, periodo) |>
  summarise(
    tasa_promedio = mean(crecimiento_ponderado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = periodo,
    values_from = tasa_promedio,
    names_prefix = "tasa_"
  ) |>
  mutate(
    cambio_pp = tasa_2016_2025 - tasa_1961_1970,
    
  # Calcular el cambio relativo cuando la tasa inicial es positiva
    cambio_porcentual = if_else(
      tasa_1961_1970 > 0,
      (cambio_pp / tasa_1961_1970) * 100,
      NA_real_
    )
  ) |>
  arrange(cambio_porcentual) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

View(comparacion_crecimiento)

################################################################################
# Explicación de las variables
################################################################################

# Diccionario de variables

diccionario <- tribble(
  ~variable, ~descripcion, ~tipo, ~unidad, ~rango_teorico,
  
  # Identificación y tiempo
  "pais",
  "Nombre del país",
  "Categórica", "-", "-",
  
  "codigo_pais",
  "Código de tres letras utilizado para identificar al país",
  "Categórica", "-", "-",
  
  "anio",
  "Año al que corresponde la observación",
  "Numérica discreta", "Año calendario", "1960 a 2025",
  
  # Tamaño y dinámica demográfica
  "poblacion_total",
  "Población total del país",
  "Numérica", "Personas", "> 0",
  
  "poblacion_mill",
  "Población total del país dividida por un millón",
  "Numérica", "Millones de personas", "> 0",
  
  "crecimiento_poblacional",
  "Tasa de crecimiento exponencial anual de la población del país",
  "Numérica", "% anual", "-Inf a Inf",
  
  "natalidad",
  "Nacimientos vivos durante el año respecto de la población a mitad de año",
  "Numérica", "Nacimientos por cada 1.000 habitantes", ">= 0",
  
  "mortalidad",
  "Defunciones durante el año respecto de la población a mitad de año",
  "Numérica", "Defunciones por cada 1.000 habitantes", ">= 0",
  
  "migracion_neta",
  "Cantidad de inmigrantes menos emigrantes durante el año",
  "Numérica", "Personas", "-Inf a Inf",
  
  "fecundidad",
  "Número de hijos que tendría una mujer bajo las tasas de fecundidad por edad del período",
  "Numérica", "Hijos por mujer", ">= 0",
  
  "esperanza_vida",
  "Años que viviría un recién nacido bajo las tasas de mortalidad por edad del período",
  "Numérica", "Años", ">= 0",
  
  "mortalidad_menores_5",
  "Probabilidad de morir antes de cumplir cinco años, expresada por cada 1.000 nacidos vivos",
  "Numérica", "Por cada 1.000 nacidos vivos", "0 a 1.000",
  
  # Estructura etaria
  "poblacion_0_14",
  "Personas de 0 a 14 años como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100",
  
  "poblacion_15_64",
  "Personas de 15 a 64 años como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100",
  
  "poblacion_65_mas",
  "Personas de 65 años o más como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100",
  
  # Dependencia demográfica
  "dependencia_total",
  "Población de 0 a 14 años y de 65 años o más, dividida por la población de 15 a 64 años, por 100",
  "Numérica", "Personas por cada 100 personas de 15 a 64 años", ">= 0",
  
  "dependencia_joven",
  "Población de 0 a 14 años dividida por la población de 15 a 64 años, por 100",
  "Numérica", "Personas por cada 100 personas de 15 a 64 años", ">= 0",
  
  "dependencia_mayor",
  "Población de 65 años o más dividida por la población de 15 a 64 años, por 100",
  "Numérica", "Personas por cada 100 personas de 15 a 64 años", ">= 0",
  
  # Mercado laboral: estimaciones modeladas de la OIT
  "participacion_laboral",
  "Personas ocupadas o desocupadas como proporción de la población de 15 años o más",
  "Numérica", "% de la población de 15 años o más", "0 a 100",
  
  "tasa_empleo",
  "Personas ocupadas como proporción de la población de 15 años o más",
  "Numérica", "% de la población de 15 años o más", "0 a 100",
  
  "tasa_desempleo",
  "Personas sin empleo, disponibles y que buscan trabajo, como proporción de la fuerza laboral",
  "Numérica", "% de la fuerza laboral", "0 a 100",
  
  "participacion_femenina",
  "Mujeres ocupadas o desocupadas como proporción de las mujeres de 15 años o más",
  "Numérica", "% de las mujeres de 15 años o más", "0 a 100",
  
  # Condiciones económicas
  "pib_por_ocupado",
  "PIB por persona ocupada, ajustado por paridad de poder adquisitivo",
  "Numérica", "Dólares internacionales constantes de 2021 por ocupado", "> 0",
  
  "pib_per_capita_ppa",
  "PIB por habitante, ajustado por paridad de poder adquisitivo",
  "Numérica", "Dólares internacionales constantes de 2021 por habitante", "> 0",
  
  # Salud
  "gasto_salud_pib",
  "Gasto corriente en salud, público y privado, en relación con el PIB",
  "Numérica", "% del PIB", ">= 0",
  
  "gasto_publico_salud_pib",
  "Gasto corriente del gobierno general en salud financiado con fuentes internas, en relación con el PIB",
  "Numérica", "% del PIB", ">= 0",
  
  "gasto_bolsillo_salud",
  "Pagos directos de los hogares por salud respecto del gasto corriente total en salud",
  "Numérica", "% del gasto corriente en salud", "0 a 100",
  
  # Educación
  "gasto_educacion_pib",
  "Gasto del gobierno general en educación, corriente, de capital y transferencias, en relación con el PIB",
  "Numérica", "% del PIB", ">= 0",
  
  "gasto_por_alumno_primaria",
  "Gasto público promedio por estudiante de primaria en relación con el PIB per cápita",
  "Numérica", "% del PIB per cápita", ">= 0",
  
  "gasto_por_alumno_secundaria",
  "Gasto público promedio por estudiante de secundaria en relación con el PIB per cápita",
  "Numérica", "% del PIB per cápita", ">= 0",
  
  # Protección social
  "cobertura_proteccion_social",
  "Población beneficiaria directa o indirecta de programas de protección social y laborales",
  "Numérica", "% de la población", "0 a 100",
  
  "cobertura_seguros_sociales",
  "Población beneficiaria directa o indirecta de seguros sociales, incluidas pensiones contributivas",
  "Numérica", "% de la población", "0 a 100",
  
  # Pirámide poblacional: mujeres
  # El denominador es la población femenina de todas las edades.
  "mujeres_0_4_pct",
  "Mujeres de 0 a 4 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_5_9_pct",
  "Mujeres de 5 a 9 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_10_14_pct",
  "Mujeres de 10 a 14 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_15_19_pct",
  "Mujeres de 15 a 19 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_20_24_pct",
  "Mujeres de 20 a 24 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_25_29_pct",
  "Mujeres de 25 a 29 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_30_34_pct",
  "Mujeres de 30 a 34 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_35_39_pct",
  "Mujeres de 35 a 39 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_40_44_pct",
  "Mujeres de 40 a 44 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_45_49_pct",
  "Mujeres de 45 a 49 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_50_54_pct",
  "Mujeres de 50 a 54 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_55_59_pct",
  "Mujeres de 55 a 59 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_60_64_pct",
  "Mujeres de 60 a 64 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_65_69_pct",
  "Mujeres de 65 a 69 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_70_74_pct",
  "Mujeres de 70 a 74 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_75_79_pct",
  "Mujeres de 75 a 79 años",
  "Numérica", "% de la población femenina", "0 a 100",
  
  "mujeres_80_mas_pct",
  "Mujeres de 80 años o más",
  "Numérica", "% de la población femenina", "0 a 100",
  
  # Pirámide poblacional: varones
  # El denominador es la población masculina de todas las edades.
  "varones_0_4_pct",
  "Varones de 0 a 4 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_5_9_pct",
  "Varones de 5 a 9 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_10_14_pct",
  "Varones de 10 a 14 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_15_19_pct",
  "Varones de 15 a 19 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_20_24_pct",
  "Varones de 20 a 24 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_25_29_pct",
  "Varones de 25 a 29 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_30_34_pct",
  "Varones de 30 a 34 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_35_39_pct",
  "Varones de 35 a 39 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_40_44_pct",
  "Varones de 40 a 44 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_45_49_pct",
  "Varones de 45 a 49 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_50_54_pct",
  "Varones de 50 a 54 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_55_59_pct",
  "Varones de 55 a 59 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_60_64_pct",
  "Varones de 60 a 64 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_65_69_pct",
  "Varones de 65 a 69 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_70_74_pct",
  "Varones de 70 a 74 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_75_79_pct",
  "Varones de 75 a 79 años",
  "Numérica", "% de la población masculina", "0 a 100",
  
  "varones_80_mas_pct",
  "Varones de 80 años o más",
  "Numérica", "% de la población masculina", "0 a 100",
  
  # Composición por sexo
  "mujeres_total_pct",
  "Mujeres como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100",
  
  "varones_total_pct",
  "Varones como proporción de la población total",
  "Numérica", "% de la población total", "0 a 100",
  
  # Clasificaciones del Banco Mundial
  "region",
  "Región del Banco Mundial asociada al país en la descarga; no representa una clasificación histórica anual",
  "Categórica", "-", "7 categorías",
  
  "ingreso",
  "Grupo de ingreso del Banco Mundial en la descarga; no necesariamente corresponde al grupo de cada año histórico",
  "Ordinal", "-", "4 categorías"
)

