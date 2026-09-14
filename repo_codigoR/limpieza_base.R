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
#write_csv(wdi_limpia, "raw/wdi_limpia.csv")
