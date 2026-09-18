install.packages(c("tidyverse", "WDI", "skimr", "readr"))

# Activamos los paquetes 

library(tidyverse)
library(WDI)
library(skimr)
library(readr)
library(dplyr)

wdi_raw <- read_csv("C:/Users/renam/OneDrive/Escritorio/tp_frosch_vidret_mattioli/raw/wdi_raw.csv")
#wdi_raw <- read_csv(file.choose())

#El vector para excluir los territorios no soberanos y/o economías agregadas por fuera de países
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
    region != "",
    !(iso3c %in% territorios_excluir)
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
  ) |>  # Acá se cierra mutate()
  
  rename(
    pais = country,
    ingreso = income,
    anio = year,
    codigo_pais = iso3c
  ) |>
  
  relocate(poblacion_mill, .after = poblacion_total) |>
  relocate(region, .after = codigo_pais) |>
  relocate(ingreso, .after = pib_per_capita_ppa) 

wdi_limpia <- wdi_limpia |>
  arrange(pais, anio)

#No ejecutar
#write_csv(wdi_raw, "raw/wdi_raw.csv")
#write_csv(wdi_limpia, "C:/Users/renam/OneDrive/Escritorio/tp_frosch_vidret_mattioli/repo_wdi/wdi_limpia.csv")
