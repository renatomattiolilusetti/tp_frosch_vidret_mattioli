################################################################################
# Parte 1: preparamos la base de datos para ésta sección. 
################################################################################

# Instalar
install.packages("WDI")

# Cargar paquete
library(WDI)

# Países seleccionados: 
paises <- c(
  "NER", # Níger
  "NGA", # Nigeria
  "AFG", # Afganistán
  "TCD", # Chad
  "ARG", # Argentina
  "BRA", # Brasil
  "IND", # India
  "MEX", # México
  "JPN", # Japón
  "ITA", # Italia
  "DEU", # Alemania
  "ESP", # España
  "KOR", # Corea del Sur
  "URY", # Uruguay
  "ARE", # Emiratos Árabes Unidos
  "CHN"  # China
)

# Indicadores
indicadores_1 <- c(
  poblacion_total = "SP.POP.TOTL",
  crecimiento_poblacion = "SP.POP.GROW",
  tasa_natalidad = "SP.DYN.CBRT.IN",
  tasa_mortalidad = "SP.DYN.CDRT.IN",
  migracion_neta = "SM.POP.NETM",
  tasa_fecundidad = "SP.DYN.TFRT.IN",
  esperanza_vida = "SP.DYN.LE00.IN",
  mortalidad_niniez = "SH.DYN.MORT",
  poblacion_0_14 = "SP.POP.0014.TO.ZS",
  poblacion_15_64 = "SP.POP.1564.TO.ZS",
  poblacion_65_mas = "SP.POP.65UP.TO.ZS",
  dependencia_total = "SP.POP.DPND",
  dependencia_joven = "SP.POP.DPND.YG",
  dependencia_mayores = "SP.POP.DPND.OL"
)

# Para descargar TODO junto:
wdi_parte1 <- WDI(
  country = paises,
  indicator = indicadores_1,
  start = 1960,
  end = 2025,
  extra = FALSE
)
