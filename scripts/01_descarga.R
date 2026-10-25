# ==============================================================================
# Descarga
# ==============================================================================

install.packages(c("tidyverse", "skimr", "readr", "mice", "naniar", "WDI", "car", "lmtest", "sandwich"))

library(tidyverse)
library(skimr)
library(dplyr)
library(mice)
library(naniar)
library(WDI)
library(car)
library(lmtest)
library(sandwich)

# Para generar una primer base cruda, a partir de las variables seleccionadas, 
# durante el lapso de tiempo seleccionado

wdi_raw <- WDI(
  indicator =  c(
    poblacion_total              = "SP.POP.TOTL",
    crecimiento_poblacional      = "SP.POP.GROW",
    natalidad                    = "SP.DYN.CBRT.IN",
    mortalidad                   = "SP.DYN.CDRT.IN",
    migracion_neta               = "SM.POP.NETM",
    fecundidad                   = "SP.DYN.TFRT.IN",
    esperanza_vida               = "SP.DYN.LE00.IN",
    mortalidad_menores_5         = "SH.DYN.MORT",
    poblacion_0_14               = "SP.POP.0014.TO.ZS",
    poblacion_15_64              = "SP.POP.1564.TO.ZS",
    poblacion_65_mas             = "SP.POP.65UP.TO.ZS",
    dependencia_total            = "SP.POP.DPND",
    dependencia_joven            = "SP.POP.DPND.YG",
    dependencia_mayor            = "SP.POP.DPND.OL",
    participacion_laboral        = "SL.TLF.CACT.ZS",
    tasa_empleo                  = "SL.EMP.TOTL.SP.ZS",
    tasa_desempleo               = "SL.UEM.TOTL.ZS",
    participacion_femenina       = "SL.TLF.CACT.FE.ZS",
    pib_por_ocupado              = "SL.GDP.PCAP.EM.KD",
    pib_per_capita_ppa           = "NY.GDP.PCAP.PP.KD",
    gasto_salud_pib              = "SH.XPD.CHEX.GD.ZS",
    gasto_publico_salud_pib      = "SH.XPD.GHED.GD.ZS",
    gasto_bolsillo_salud         = "SH.XPD.OOPC.CH.ZS",
    indice_cobertura_salud       = "SH_UHC_SCI", 
    gasto_educacion_pib          = "SE.XPD.TOTL.GD.ZS",
    gasto_por_alumno_primaria    = "SE.XPD.PRIM.PC.ZS",
    gasto_por_alumno_secundaria  = "SE.XPD.SECO.PC.ZS",
    cobertura_proteccion_social  = "per_allsp.cov_pop_tot",
    cobertura_seguros_sociales   = "per_si_allsi.cov_pop_tot",
    mujeres_0_4_pct              = "SP.POP.0004.FE.5Y", # Los datos para pirámide empiezan acá. 
    mujeres_5_9_pct              = "SP.POP.0509.FE.5Y",
    mujeres_10_14_pct            = "SP.POP.1014.FE.5Y",
    mujeres_15_19_pct            = "SP.POP.1519.FE.5Y",
    mujeres_20_24_pct            = "SP.POP.2024.FE.5Y",
    mujeres_25_29_pct            = "SP.POP.2529.FE.5Y",
    mujeres_30_34_pct            = "SP.POP.3034.FE.5Y",
    mujeres_35_39_pct            = "SP.POP.3539.FE.5Y",
    mujeres_40_44_pct            = "SP.POP.4044.FE.5Y",
    mujeres_45_49_pct            = "SP.POP.4549.FE.5Y",
    mujeres_50_54_pct            = "SP.POP.5054.FE.5Y",
    mujeres_55_59_pct            = "SP.POP.5559.FE.5Y",
    mujeres_60_64_pct            = "SP.POP.6064.FE.5Y",
    mujeres_65_69_pct            = "SP.POP.6569.FE.5Y",
    mujeres_70_74_pct            = "SP.POP.7074.FE.5Y",
    mujeres_75_79_pct            = "SP.POP.7579.FE.5Y",
    mujeres_80_mas_pct           = "SP.POP.80UP.FE.5Y",
    varones_0_4_pct              = "SP.POP.0004.MA.5Y",
    varones_5_9_pct              = "SP.POP.0509.MA.5Y",
    varones_10_14_pct            = "SP.POP.1014.MA.5Y",
    varones_15_19_pct            = "SP.POP.1519.MA.5Y",
    varones_20_24_pct            = "SP.POP.2024.MA.5Y",
    varones_25_29_pct            = "SP.POP.2529.MA.5Y",
    varones_30_34_pct            = "SP.POP.3034.MA.5Y",
    varones_35_39_pct            = "SP.POP.3539.MA.5Y",
    varones_40_44_pct            = "SP.POP.4044.MA.5Y",
    varones_45_49_pct            = "SP.POP.4549.MA.5Y",
    varones_50_54_pct            = "SP.POP.5054.MA.5Y",
    varones_55_59_pct            = "SP.POP.5559.MA.5Y",
    varones_60_64_pct            = "SP.POP.6064.MA.5Y",
    varones_65_69_pct            = "SP.POP.6569.MA.5Y",
    varones_70_74_pct            = "SP.POP.7074.MA.5Y",
    varones_75_79_pct            = "SP.POP.7579.MA.5Y",
    varones_80_mas_pct           = "SP.POP.80UP.MA.5Y",
    mujeres_total_pct            = "SP.POP.TOTL.FE.ZS",   # También para pirámide: "Composición por sexo (% de la población total)".
    varones_total_pct            = "SP.POP.TOTL.MA.ZS"
  ), 
  start = 1960, end = 2025,
  extra = TRUE)

# Guardar la base cruda ####

# Para dejar una copia de la base cruda en la carpeta raw utilizamos: 
# write_csv(wdi_raw, "raw/wdi_raw.csv") 