
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

region_elegida <- "África Subsahariana"

na_decadas |>
  filter(region == region_elegida) |>
  ggplot(aes(x = decada, y = variable, fill = porcentaje_na)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B",
    limits = c(0, 100),
    name = "% de NA"
  ) +
  scale_y_discrete(limits = rev(variables_mapa)) +
  labs(
    title = "Faltantes por variable y década",
    subtitle = region_elegida,
    x = NULL,
    y = NULL,
    caption = "Porcentaje sobre las observaciones país-año de cada región y período."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
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

poblacion_region <- wdi_limpia |>
  group_by(anio, region) |>
  summarise(
    poblacion = sum(poblacion_total, na.rm = TRUE),
    .groups = "drop"
  )

# Gráfico de áreas apiladas
ggplot(
  poblacion_region,
  aes(
    x = anio,
    y = poblacion / 1e9,
    color = region
  )
) +
  geom_line(linewidth = 1.1) +
  facet_wrap(
    ~ region,
    scales = "free_y"
  ) +
  labs(
    title = "Evolución de la población por región",
    subtitle = "1960-2025",
    x = "Año",
    y = "Población (miles de millones)"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


# Grafico sobre crecimiento natural ############################################

# 1. Agrupar por región y año hasta 2025
df_brecha_regional <- wdi_limpia |> 
  filter(!is.na(region), region != "Aggregates", anio <= 2025) |> 
  group_by(region, anio) |> 
  summarise(
    natalidad_pct = mean(natalidad, na.rm = TRUE) / 10,
    mortalidad_pct = mean(mortalidad, na.rm = TRUE) / 10,
    .groups = "drop"
  ) |> 
  pivot_longer(
    cols = c(natalidad_pct, mortalidad_pct), 
    names_to = "tasa", 
    values_to = "valor"
  )

# 2. Graficar paneles por región (1960 - 2025)
ggplot(df_brecha_regional, aes(x = anio, y = valor, color = tasa)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region, ncol = 3) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 15)) +
  scale_color_manual(
    values = c("natalidad_pct" = "#2b5c8f", "mortalidad_pct" = "#d95f02"),
    labels = c("Mortalidad (%)", "Natalidad (%)")
  ) +
  labs(
    title = "Brecha entre Natalidad y Mortalidad por Región (1960 - 2025)",
    subtitle = "Evolución del balance vegetativo regional",
    x = "Año",
    y = "Tasa (%)",
    color = "Componente"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

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

# Calculo de la mediana entre países para cada región y año
crecimiento_region_anual <- wdi_limpia |>
  filter(anio >= 1960, anio <= 2025) |>
  group_by(region, anio) |>
  summarise(
    paises_con_dato = sum(!is.na(crecimiento_poblacional)),
    mediana_regional = if (paises_con_dato > 0) {
      median(crecimiento_poblacional, na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  )

# Gráfico de la mediana regional sin ponderar por poblacion
grafico_crecimiento_regional <- ggplot(
  crecimiento_region_anual,
  aes(
    x = anio,
    y = mediana_regional,
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
    title = "Evolución del crecimiento poblacional por región",
    subtitle = "Mediana del crecimiento anual entre países, 1960–2025",
    x = "Año",
    y = "Crecimiento anual (%)",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(ncol = 1))

##Mediana de tasa de crecimiento poblacional ponderada por población##

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
    title = "Crecimiento poblacional ponderado por región",
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

################################################################################
# Explicación de las variables
################################################################################

#Diccionario de variables 
diccionario <- tribble(
  ~variable, ~descripcion, ~tipo, ~unidad, ~rango_teorico,
  
  # Identificación y tiempo
  "pais",
  "Nombre del país o economía",
  "Categórica", "-", "-",
  
  "codigo_pais",
  "Código de tres letras que identifica al país o economía",
  "Categórica", "-", "-",
  
  "anio",
  "Año al que corresponde la observación",
  "Numérica discreta", "Año calendario", "1960 a 2025",
  
  # Tamaño y dinámica demográfica
  "poblacion_total",
  "Población total del país o economía",
  "Numérica", "Personas", "> 0",
  
  "poblacion_mill",
  "Población total dividida por un millón",
  "Numérica", "Millones de personas", "> 0",
  
  "crecimiento_poblacional",
  "Tasa de crecimiento exponencial anual de la población",
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
  "Población de 0 a 14 años y de 65 o más, dividida por la población de 15 a 64 años, por 100",
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
  "Región del Banco Mundial asociada al país en la descarga",
  "Categórica", "-", "7 categorías",
  
  "ingreso",
  "Grupo de ingreso del Banco Mundial en la descarga; no necesariamente corresponde al grupo de cada año histórico",
  "Ordinal", "-", "4 categorías + Sin clasificar"
)

