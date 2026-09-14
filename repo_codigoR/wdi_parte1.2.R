install.packages(c("tidyverse", "WDI", "skimr", "readr"))

# Activamos los paquetes 

library(tidyverse)
library(WDI)
library(skimr)
library(readr)
library(dplyr)

wdi_raw <- read_csv("raw/wdi_raw.csv")

# Limpiamos base:

wdi_limpia <- wdi_raw |>
  select(
    -iso2c,
    -latitude,
    -longitude,
    -capital,
    -lending,
    -status,
    -lastupdated
  ) |>
  filter(
    region != "Aggregates",
    !is.na(region),
    region != ""
  ) |>
  mutate(
    region = dplyr::recode(
      region,
      "East Asia & Pacific" = "Asia Oriental y Pacífico",
      "Europe & Central Asia" = "Europa y Asia Central",
      "Latin America & Caribbean" = "América Latina y el Caribe",
      "Middle East, North Africa, Afghanistan & Pakistan" = "Oriente Medio, Norte de África, Afganistán y Pakistán",
      "North America" = "América del Norte",
      "South Asia" = "Asia Meridional",
      "Sub-Saharan Africa" = "África Subsahariana"
    ),
    
    region = factor(region),
    
    income = factor(
      income,
      levels = c(
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income",
        "Not classified"
      ),
      labels = c(
        "Ingreso bajo",
        "Ingreso medio-bajo",
        "Ingreso medio-alto",
        "Ingreso alto",
        "Sin clasificar"
      )
    ),
    
    poblacion_mill = poblacion_total / 1e6
  ) |>
  
  relocate(poblacion_mill, .after = poblacion_total) |>
  
  rename(
    pais = country,
    ingreso = income,
    anio = year,
    codigo_pais = iso3c
  )

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

dim(wdi_limpia)
glimpse(wdi_limpia)

wdi_limpia |>
  count(codigo_pais, anio) |>
  filter(n > 1)

wdi_limpia |>
  summarise(
    cantidad_paises = n_distinct(codigo_pais),
    primer_anio = min(anio),
    ultimo_anio = max(anio),
    cantidad_anios = n_distinct(anio)
  )

colSums(is.na(wdi_limpia))

wdi_limpia |>
  group_by(anio) |>
  summarise(
    paises_con_datos = sum(!is.na(participacion_laboral)),
    paises_sin_datos = sum(is.na(participacion_laboral))
  ) |>
  print(n = Inf)

wdi_limpia |>
  filter(anio >= 1990) |>
  group_by(codigo_pais, pais) |>
  summarise(
    anios_con_datos = sum(!is.na(participacion_laboral)),
    .groups = "drop"
  ) |>
  count(anios_con_datos)

write_csv(wdi_raw, "raw/wdi_raw.csv")
write_csv(wdi_limpia, "raw/wdi_limpia.csv")


  