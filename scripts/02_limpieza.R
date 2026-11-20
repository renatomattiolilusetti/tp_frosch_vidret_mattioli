# ==============================================================================
# Limpieza de Base 
# ==============================================================================

library(tidyverse)

# 1. Lectura de base original

wdi_raw <- read_csv("raw/wdi_raw.csv")

# Identificar los indicadores de la descarga, excluyendo los metadatos
columnas_metadatos <- c(
  "country", "iso2c", "iso3c", "year",
  "status", "lastupdated", "region", "income",
  "lending", "capital", "longitude", "latitude"
)

indicadores_raw <- setdiff(
  names(wdi_raw),
  columnas_metadatos
)

# Comprobar que la base original tenga los 65 indicadores
stopifnot(
  all(columnas_metadatos %in% names(wdi_raw)),
  length(indicadores_raw) == 65
)

# 2. Definimos territorios a excluir

# Excluimos territorios no soberanos

territorios_excluir <- c(
  "ABW", # Aruba
  "ASM", # Samoa Americana
  "BMU", # Bermudas
  "VGB", # Islas Vírgenes Británicas
  "CYM", # Islas Caimán
  "CHI", # Islas del Canal
  "CUW", # Curazao
  "FRO", # Islas Feroe
  "PYF", # Polinesia Francesa
  "GIB", # Gibraltar
  "GRL", # Groenlandia
  "GUM", # Guam
  "HKG", # Hong Kong
  "IMN", # Isla de Man
  "MAC", # Macao
  "NCL", # Nueva Caledonia
  "MNP", # Islas Marianas del Norte
  "PRI", # Puerto Rico
  "SXM", # Sint Maarten
  "MAF", # Saint-Martin
  "TCA", # Islas Turcas y Caicos
  "VIR"  # Islas Vírgenes de Estados Unidos
)

# 3. Limpieza de base

# Eliminamos columnas que no utilizaremos y excluimos agregados regionales

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
    region != "",
    !(iso3c %in% territorios_excluir)
  ) |>

# Traducimos nombres de regiones y categorías de ingreso
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
    
  # Expresamos la población en millones
    poblacion_mill = poblacion_total / 1e6
  ) |>
  
# Renombramos columnas y las ordenamos
  rename(
    pais = country,
    ingreso = income,
    anio = year,
    codigo_pais = iso3c
  ) |>
  
  relocate(poblacion_mill, .after = poblacion_total) |>
  relocate(region, .after = codigo_pais) |>
  relocate(ingreso, .after = pib_per_capita_ppa) |>
  arrange(pais, anio)

# 4. Validamos limpieza
# Comprobar que no se perdió ningún indicador
variables_faltantes <- setdiff(
  indicadores_raw,
  names(wdi_limpia)
)

if (length(variables_faltantes) > 0) {
  stop(
    "Faltan indicadores después de la limpieza: ",
    paste(variables_faltantes, collapse = ", ")
  )
}

# Comprobar que no haya identificadores de país o año faltantes
stopifnot(
  !anyNA(wdi_limpia$codigo_pais),
  !anyNA(wdi_limpia$anio)
)

# Comprobar que cada país tenga una sola observación por año
stopifnot(
  !anyDuplicated(wdi_limpia[c("codigo_pais", "anio")])
)

# Comprobar la cantidad de columnas
stopifnot(
  ncol(wdi_limpia) == 71
)

# 5. Guardar la base limpia

# Crear la carpeta input si todavía no existe
dir.create("input", showWarnings = FALSE)

# Guardar la base procesada
write_csv(wdi_limpia, "input/wdi_limpia.csv")

# 6. Resumen de la base limpia

cat("Dimensiones de la base limpia:\n")
print(dim(wdi_limpia))

cat("\nCantidad de países y período disponible:\n")

wdi_limpia |>
  summarise(
    cantidad_paises = n_distinct(codigo_pais),
    primer_anio = min(anio),
    ultimo_anio = max(anio),
    cantidad_anios = n_distinct(anio)
  ) |>
  print()

# Para dejar una copia de esta base limpia, 
write_csv(wdi_limpia, "repo_wdi/wdi_limpia.csv")
