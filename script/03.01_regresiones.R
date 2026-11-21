# ==============================================================================
# Regresiones lineales: 2000, 2010 y 2019
# ==============================================================================

# Paquetes necesarios
library(tidyverse)
library(mice)
library(lattice)

# Leer la base limpia
wdi_limpia <- read_csv(
  "input/wdi_limpia.csv",
  show_col_types = FALSE
)

#### Tratemiento de outliers ###################################################

variables_atipicos <- c(
  "poblacion_65_mas",
  "fecundidad",
  "esperanza_vida",
  "migracion_relativa",
  "log_pib_pc",
  "gasto_bolsillo_salud",
  "gasto_publico_salud_pib"
)

# Cargar la función de diagnóstico de outliers
source("utils/diagnosticar_outliers.R")

diag_2000 <- diagnosticar_outliers(
  2000, wdi_limpia, variables_atipicos
)

diag_2010 <- diagnosticar_outliers(
  2010, wdi_limpia, variables_atipicos
)

diag_2019 <- diagnosticar_outliers(
  2019, wdi_limpia, variables_atipicos
)

# Tabla global comparable de outliers.
resumen_outliers <- bind_rows(
  diag_2000$outliers |> mutate(anio = 2000),
  diag_2010$outliers |> mutate(anio = 2010),
  diag_2019$outliers |> mutate(anio = 2019)
) |>
  count(anio, variable, name = "cantidad_outliers")
print(resumen_outliers, n = Inf)

# Mostrar tablas específicas cuando sea necesario (los 3 años).
print(diag_2000$disponibilidad, n = Inf)
print(diag_2010$disponibilidad, n = Inf)
print(diag_2019$disponibilidad, n = Inf)

print(diag_2000$outliers, n = Inf, width = Inf)
print(diag_2010$outliers, n = Inf, width = Inf)
print(diag_2019$outliers, n = Inf, width = Inf)

# Revisión temporal y MAD ordenados por desvío.
print(diag_2000$resumen_temporal, n = Inf, width = Inf)
print(diag_2010$resumen_temporal, n = Inf, width = Inf)
print(diag_2019$resumen_temporal, n = Inf, width = Inf)

# Resumen por categorías de revisión temporal.
print(diag_2000$resumen_temporal |> count(variable, revision_temporal), n = Inf)
print(diag_2010$resumen_temporal |> count(variable, revision_temporal), n = Inf)
print(diag_2019$resumen_temporal |> count(variable, revision_temporal), n = Inf)

# Gráficos: utilizar print() para verlos al ejecutar el script completo.
if (!is.null(diag_2000$grafico)) print(diag_2000$grafico)
if (!is.null(diag_2010$grafico)) print(diag_2010$grafico)
if (!is.null(diag_2019$grafico)) print(diag_2019$grafico)

# Control de referencia con la base que veníamos utilizando:
# se habían obtenido 26 (2000), 30 (2010) y 34 (2019) candidatos.
# Si las cantidades cambian, revisar la base y los valores infinitos.
print(tibble(
  anio = c(2000, 2010, 2019),
  valores_atipicos = c(nrow(diag_2000$outliers),
                       nrow(diag_2010$outliers),
                       nrow(diag_2019$outliers))
))


# Se convierte en NA la esperanza de vida de CAF en 2019.

# Verificar que la observación es única y conservar el valor original.
registro_tratamiento <- wdi_limpia |>
  filter(codigo_pais == "CAF", anio == 2019) |>
  transmute(
    codigo_pais, pais, anio,
    variable = "esperanza_vida",
    valor_original = esperanza_vida,
    tratamiento = "NA en base pre-MCO",
    motivo = "Discontinuidad temporal y discrepancia con OMS",
    imputacion = "MICE existente del modelo de 2019 (pmm, m=20)"
  )
stopifnot(nrow(registro_tratamiento) == 1)
stopifnot(!is.na(registro_tratamiento$valor_original))

wdi_pre_mco <- wdi_limpia |>
  mutate(
    esperanza_vida = if_else(
      codigo_pais == "CAF" & anio == 2019,
      NA_real_, esperanza_vida
    )
  )

# Auditoría: la base original no cambió y el resto de la base sigue intacto.
stopifnot(!is.na(
  wdi_limpia$esperanza_vida[
    wdi_limpia$codigo_pais == "CAF" & wdi_limpia$anio == 2019
  ]
))
stopifnot(all(is.na(
  wdi_pre_mco$esperanza_vida[
    wdi_pre_mco$codigo_pais == "CAF" & wdi_pre_mco$anio == 2019
  ]
)))
stopifnot(sum(is.na(wdi_pre_mco$esperanza_vida)) ==
            sum(is.na(wdi_limpia$esperanza_vida)) + 1)

print(registro_tratamiento)
print(wdi_pre_mco |>
        filter(codigo_pais == "CAF", anio %in% c(2018, 2019, 2020)) |>
        select(pais, anio, esperanza_vida))

# Medir y documentar el efecto de convertir un único valor en NA.
impacto_limpieza_2019 <- bind_rows(
  wdi_limpia |>
    filter(anio == 2019) |>
    summarise(
      version = "Original",
      valores_disponibles = sum(!is.na(esperanza_vida)),
      faltantes = sum(is.na(esperanza_vida)),
      media = mean(esperanza_vida, na.rm = TRUE),
      mediana = median(esperanza_vida, na.rm = TRUE),
      desvio = sd(esperanza_vida, na.rm = TRUE)
    ),
  wdi_pre_mco |>
    filter(anio == 2019) |>
    summarise(
      version = "Procesada",
      valores_disponibles = sum(!is.na(esperanza_vida)),
      faltantes = sum(is.na(esperanza_vida)),
      media = mean(esperanza_vida, na.rm = TRUE),
      mediana = median(esperanza_vida, na.rm = TRUE),
      desvio = sd(esperanza_vida, na.rm = TRUE)
    )
)
print(impacto_limpieza_2019)

wdi_pre_mco |>
  filter(codigo_pais == "CAF", anio == 2019) |>
  mutate(
    migracion_relativa = 100 * migracion_neta / poblacion_total,
    log_pib_pc = log(pib_per_capita_ppa)
  ) |>
  select(
    pais,
    poblacion_65_mas,
    fecundidad,
    esperanza_vida,
    migracion_relativa,
    log_pib_pc,
    gasto_bolsillo_salud,
    gasto_publico_salud_pib
  )

#### Regresion 2000 ############################################################
#### Preparación de las variables ##############################################
base_2000 <- wdi_pre_mco |>
  filter(anio == 2000) |>
  mutate(
    migracion_relativa = 100 * migracion_neta / poblacion_total,
    log_pib_pc = log(pib_per_capita_ppa),
    log_poblacion = log(poblacion_total),
    region = factor(region)
  ) |>
  select(
    pais, region, poblacion_65_mas, fecundidad, esperanza_vida,
    migracion_relativa, log_pib_pc, gasto_bolsillo_salud,
    gasto_publico_salud_pib, log_poblacion
  )

# Cada pais debe aparecer una sola vez y los logaritmos deben ser validos.
stopifnot(!anyDuplicated(base_2000$pais))
stopifnot(all(is.finite(base_2000$log_poblacion)))
stopifnot(all(is.finite(base_2000$log_pib_pc[!is.na(base_2000$log_pib_pc)])))

# El tamano poblacional se usara para IMPUTAR, no en la regresion final.
base_2000_modelo <- base_2000 |> select(-log_poblacion)

### Diagnóstico NA #############################################################
# Cantidad de NA por variable.
faltantes_2000 <- colSums(is.na(base_2000_modelo))
print(faltantes_2000)

# Comparacion de paises incluidos y excluidos por falta de datos.
diagnostico_2000 <- base_2000_modelo |>
  mutate(excluido = !complete.cases(base_2000_modelo))

exclusion_region_2000 <- diagnostico_2000 |>
  group_by(region) |>
  summarise(
    total_paises = n(),
    excluidos = sum(excluido),
    porcentaje_excluidos = round(100 * mean(excluido), 1),
    .groups = "drop"
  )
print(exclusion_region_2000)

perfil_faltantes_2000 <- diagnostico_2000 |>
  group_by(excluido) |>
  summarise(
    cantidad_paises = n(),
    mediana_envejecimiento = median(poblacion_65_mas, na.rm = TRUE),
    mediana_fecundidad = median(fecundidad, na.rm = TRUE),
    mediana_esperanza_vida = median(esperanza_vida, na.rm = TRUE),
    .groups = "drop"
  )
print(perfil_faltantes_2000)

# Test de Little: H0 = faltantes MCAR
# Excluimos las variables identificadoras/categoricas
test_mcar_2000 <- naniar::mcar_test(
  base_2000 |> select(-pais, -region)
)
print(test_mcar_2000)

#### Regresion con casos completos############################################## 
base_2000_completa <- base_2000_modelo |> drop_na()
cat("Paises totales:", nrow(base_2000_modelo), "\n")
cat("Paises con datos completos:", nrow(base_2000_completa), "\n")

modelo_2000 <- lm(
  poblacion_65_mas ~ fecundidad + esperanza_vida +
    migracion_relativa + log_pib_pc + gasto_bolsillo_salud +
    gasto_publico_salud_pib + region,
  data = base_2000_completa
)
print(summary(modelo_2000))

### Imputacion multiple ########################################################
base_mice_2000_poblacion <- base_2000 |> select(-pais)

imp_2000_poblacion <- mice(
  base_mice_2000_poblacion,
  m = 20,
  maxit = 20,
  method = "pmm",
  seed = 42,
  printFlag = FALSE
)

# NULL indica que MICE no registro eventos; no garantiza imputaciones validas.
print(imp_2000_poblacion$loggedEvents)
print(imp_2000_poblacion$method)

# Diagnosticos graficos de estabilidad y distribuciones imputadas
plot(imp_2000_poblacion)
densityplot(
  imp_2000_poblacion,
  ~ log_pib_pc + gasto_bolsillo_salud + gasto_publico_salud_pib
)

# Resumen del gasto sanitario imputado por pais (umbral 10% solo exploratorio).
tabla_gasto_imputado_2000 <- imp_2000_poblacion$imp$gasto_publico_salud_pib |>
  tibble::rownames_to_column("fila") |>
  pivot_longer(
    cols = -fila,
    names_to = "imputacion",
    values_to = "valor"
  ) |>
  mutate(pais = base_2000$pais[as.integer(fila)]) |>
  group_by(pais) |>
  summarise(
    minimo = min(valor),
    mediana = median(valor),
    maximo = max(valor),
    veces_mayor_10 = sum(valor > 10),
    .groups = "drop"
  ) |>
  arrange(desc(maximo))
print(tabla_gasto_imputado_2000)

modelo_mice_poblacion_2000 <- with(
  imp_2000_poblacion,
  lm(
    poblacion_65_mas ~ fecundidad + esperanza_vida +
      migracion_relativa + log_pib_pc + gasto_bolsillo_salud +
      gasto_publico_salud_pib + region
  )
)
resultado_mice_poblacion_2000 <- pool(modelo_mice_poblacion_2000)
print(summary(resultado_mice_poblacion_2000, conf.int = TRUE))

# R2 y R2 ajustado de MICE
mice::pool.r.squared(
  modelo_mice_poblacion_2000
)

mice::pool.r.squared(
  modelo_mice_poblacion_2000,
  adjusted = TRUE
)

modelo_mice_hc1_2000 <- with(
  imp_2000_poblacion,
  {
    ajuste <- lm(
      poblacion_65_mas ~ fecundidad + esperanza_vida +
        migracion_relativa + log_pib_pc +
        gasto_bolsillo_salud + gasto_publico_salud_pib +
        region
    )
    
    lmtest::coeftest(
      ajuste,
      vcov. = sandwich::vcovHC(ajuste, type = "HC1"),
      save = TRUE
    )
  }
)

# Combinar los resultados con las reglas de Rubin
resultado_mice_hc1_2000 <- mice::pool(
  modelo_mice_hc1_2000,
  dfcom = df.residual(modelo_mice_poblacion_2000$analyses[[1]])
)

# Mostrar resultados
summary(resultado_mice_hc1_2000, conf.int = TRUE)

#### Regresion 2010 ############################################################

base_2010 <- wdi_pre_mco |>
  filter(anio == 2010) |>
  mutate(
    migracion_relativa = 100 * migracion_neta / poblacion_total,
    log_pib_pc = log(pib_per_capita_ppa),
    log_poblacion = log(poblacion_total),
    region = factor(region)
  ) |>
  select(
    pais,
    region,
    poblacion_65_mas,
    fecundidad,
    esperanza_vida,
    migracion_relativa,
    log_pib_pc,
    gasto_bolsillo_salud,
    gasto_publico_salud_pib,
    log_poblacion
  )

# Base con las variables de la regresión
base_2010_modelo <- base_2010 |>
  select(-log_poblacion)

# Verificar la cantidad de países y los duplicados
nrow(base_2010)
anyDuplicated(base_2010$pais)

# Verificar valores faltantes
colSums(is.na(base_2010_modelo))

# Contar países con todas las variables disponibles
sum(complete.cases(base_2010_modelo))

### Diagnóstico NA #############################################################

# Identificar países que quedarían excluidos
diagnostico_2010 <- base_2010_modelo |>
  mutate(
    excluido = !complete.cases(base_2010_modelo)
  )

# Cantidad y porcentaje de excluidos por región
exclusion_region_2010 <- diagnostico_2010 |>
  group_by(region) |>
  summarise(
    total_paises = n(),
    excluidos = sum(excluido),
    porcentaje_excluidos = round(100 * mean(excluido), 1),
    .groups = "drop"
  )

print(exclusion_region_2010)

# Comparar características de incluidos y excluidos
perfil_faltantes_2010 <- diagnostico_2010 |>
  group_by(excluido) |>
  summarise(
    cantidad_paises = n(),
    mediana_envejecimiento = median(poblacion_65_mas, na.rm = TRUE),
    mediana_fecundidad = median(fecundidad, na.rm = TRUE),
    mediana_esperanza_vida = median(esperanza_vida, na.rm = TRUE),
    .groups = "drop"
  )

print(perfil_faltantes_2010)

# Test de Little para el año 2010.  H0: los datos faltantes son MCAR

test_mcar_2010 <- naniar::mcar_test(
  base_2010 |>
    select(-pais, -region)
)

print(test_mcar_2010)

#### Regresion con casos completos############################################## 

# Seleccionar países con información completa
base_2010_completa <- base_2010_modelo |>
  drop_na()

# Comprobar la cantidad de observaciones
nrow(base_2010_completa)

# Estimar la regresión
modelo_2010 <- lm(
  poblacion_65_mas ~
    fecundidad +
    esperanza_vida +
    migracion_relativa +
    log_pib_pc +
    gasto_bolsillo_salud +
    gasto_publico_salud_pib +
    region,
  data = base_2010_completa
)

# Mostrar los resultados
summary(modelo_2010)

### Imputacion multiple ########################################################

# Crear la base de imputación
base_mice_2010_poblacion <- base_2010 |>
  select(-pais)

# Generar 20 bases imputadas
imp_2010_poblacion <- mice::mice(
  base_mice_2010_poblacion,
  m = 20,
  maxit = 20,
  method = "pmm",
  seed = 42,
  printFlag = FALSE
)

# Revisar si se registraron problemas
imp_2010_poblacion$loggedEvents

# Verificar qué variables se imputaron
imp_2010_poblacion$method

# Evaluar la estabilidad de las imputaciones de 2010
plot(imp_2010_poblacion)

# Comparar las distribuciones observadas e imputadas
# Año 2010

densityplot(
  imp_2010_poblacion,
  ~ log_pib_pc +
    gasto_bolsillo_salud +
    gasto_publico_salud_pib
)

# Revisar las imputaciones del gasto público en salud en 2010
tabla_gasto_imputado_2010 <- imp_2010_poblacion$imp$gasto_publico_salud_pib |>
  tibble::rownames_to_column("fila") |>
  pivot_longer(
    cols = -fila,
    names_to = "imputacion",
    values_to = "valor"
  ) |>
  mutate(
    pais = base_2010$pais[as.integer(fila)],
    region = base_2010$region[as.integer(fila)]
  ) |>
  group_by(pais, region) |>
  summarise(
    minimo = min(valor),
    mediana = median(valor),
    maximo = max(valor),
    veces_mayor_10 = sum(valor > 10),
    .groups = "drop"
  ) |>
  arrange(desc(maximo))

print(tabla_gasto_imputado_2010, n = Inf)

# Estimar la regresión en las 20 bases imputadas
modelo_mice_poblacion_2010 <- with(
  imp_2010_poblacion,
  lm(
    poblacion_65_mas ~
      fecundidad +
      esperanza_vida +
      migracion_relativa +
      log_pib_pc +
      gasto_bolsillo_salud +
      gasto_publico_salud_pib +
      region
  )
)

# Combinar los resultados mediante las reglas de Rubin
resultado_mice_poblacion_2010 <- mice::pool(
  modelo_mice_poblacion_2010
)

# Mostrar los coeficientes combinados
summary(
  resultado_mice_poblacion_2010,
  conf.int = TRUE
)

# R2 y R2 ajustado de MICE
mice::pool.r.squared(
  modelo_mice_poblacion_2010
)

mice::pool.r.squared(
  modelo_mice_poblacion_2010,
  adjusted = TRUE
)

# Calcular errores robustos en las 20 regresiones
modelo_mice_hc1_2010 <- with(
  imp_2010_poblacion,
  {
    ajuste <- lm(
      poblacion_65_mas ~ fecundidad + esperanza_vida +
        migracion_relativa + log_pib_pc +
        gasto_bolsillo_salud + gasto_publico_salud_pib +
        region
    )
    
    lmtest::coeftest(
      ajuste,
      vcov. = sandwich::vcovHC(ajuste, type = "HC1"),
      save = TRUE
    )
  }
)

# Combinar resultados mediante las reglas de Rubin
resultado_mice_hc1_2010 <- mice::pool(
  modelo_mice_hc1_2010,
  dfcom = df.residual(modelo_mice_poblacion_2010$analyses[[1]])
)

# Mostrar resultados combinados
summary(resultado_mice_hc1_2010, conf.int = TRUE)

#### Regresion 2019 ############################################################

# Preparar la base de 2019
base_2019 <- wdi_pre_mco |>
  filter(anio == 2019) |>
  mutate(
    migracion_relativa = 100 * migracion_neta / poblacion_total,
    log_pib_pc = log(pib_per_capita_ppa),
    log_poblacion = log(poblacion_total),
    region = factor(region)
  ) |>
  select(
    pais,
    region,
    poblacion_65_mas,
    fecundidad,
    esperanza_vida,
    migracion_relativa,
    log_pib_pc,
    gasto_bolsillo_salud,
    gasto_publico_salud_pib,
    log_poblacion
  )

# Base con las variables de la regresión
base_2019_modelo <- base_2019 |>
  select(-log_poblacion)

# Verificar que el tratamiento de CAF se mantuvo
base_2019 |>
  filter(pais == "Central African Republic") |>
  select(pais, esperanza_vida)

# Cantidad total de países
nrow(base_2019)

# Cantidad de países con información completa
sum(complete.cases(base_2019_modelo))

# Verificar si hay países duplicados
anyDuplicated(base_2019$pais)

# Contar los NA por variable
colSums(is.na(base_2019_modelo))

### Diagnóstico NA #############################################################

# Identificar países que quedarían excluidos
diagnostico_2019 <- base_2019_modelo |>
  mutate(
    excluido = !complete.cases(base_2019_modelo)
  )

# Distribución de exclusiones por región
exclusion_region_2019 <- diagnostico_2019 |>
  group_by(region) |>
  summarise(
    total_paises = n(),
    excluidos = sum(excluido),
    porcentaje_excluidos = round(100 * mean(excluido), 1),
    .groups = "drop"
  )

exclusion_region_2019

# Comparar características demográficas
perfil_faltantes_2019 <- diagnostico_2019 |>
  group_by(excluido) |>
  summarise(
    cantidad_paises = n(),
    mediana_envejecimiento = median(poblacion_65_mas, na.rm = TRUE),
    mediana_fecundidad = median(fecundidad, na.rm = TRUE),
    mediana_esperanza_vida = median(esperanza_vida, na.rm = TRUE),
    .groups = "drop"
  )

perfil_faltantes_2019

test_mcar_2019 <- naniar::mcar_test(
  base_2019 |>
    select(-pais, -region)
)

test_mcar_2019

#### Regresion 2019 ############################################################

# Conservar los países con información completa
base_2019_completa <- base_2019_modelo |>
  drop_na()

# Comprobar la cantidad de observaciones
nrow(base_2019_completa)

# Estimar la regresión
modelo_2019 <- lm(
  poblacion_65_mas ~
    fecundidad +
    esperanza_vida +
    migracion_relativa +
    log_pib_pc +
    gasto_bolsillo_salud +
    gasto_publico_salud_pib +
    region,
  data = base_2019_completa
)

# Mostrar los resultados
summary(modelo_2019)

### Imputacion multiple ########################################################

# Preparar base para MICE
base_mice_2019_poblacion <- base_2019 |>
  select(-pais)

# Generar 20 bases imputadas
imp_2019_poblacion <- mice::mice(
  base_mice_2019_poblacion,
  m = 20,
  maxit = 20,
  method = "pmm",
  seed = 42,
  printFlag = FALSE
)

# Revisar posibles problemas
imp_2019_poblacion$loggedEvents

# Verificar qué variables se imputaron
imp_2019_poblacion$method

# Evaluar la estabilidad de las cadenas
plot(imp_2019_poblacion)

# Recuperar los valores imputados
valores_bolsillo_2019 <- unlist(
  imp_2019_poblacion$imp$gasto_bolsillo_salud
)

valores_publico_2019 <- unlist(
  imp_2019_poblacion$imp$gasto_publico_salud_pib
)

# Resumen de las imputaciones
summary(valores_bolsillo_2019)

summary(valores_publico_2019)

# Contar imputaciones de gasto público superiores al 10% del PIB
sum(valores_publico_2019 > 10)

# Mostrar los cinco valores imputados más altos
head(sort(valores_publico_2019, decreasing = TRUE), 5)

# Estimar la regresión en las 20 bases imputadas
modelo_mice_poblacion_2019 <- with(
  imp_2019_poblacion,
  lm(
    poblacion_65_mas ~
      fecundidad +
      esperanza_vida +
      migracion_relativa +
      log_pib_pc +
      gasto_bolsillo_salud +
      gasto_publico_salud_pib +
      region
  )
)

# Combinar las 20 regresiones
resultado_mice_poblacion_2019 <- mice::pool(
  modelo_mice_poblacion_2019
)

# Mostrar los resultados combinados
summary(
  resultado_mice_poblacion_2019,
  conf.int = TRUE
)

# R² combinado de MICE - 2019
mice::pool.r.squared(
  modelo_mice_poblacion_2019
)

# R² ajustado combinado de MICE - 2019
mice::pool.r.squared(
  modelo_mice_poblacion_2019,
  adjusted = TRUE
)

# Estimar las 20 regresiones con errores robustos HC1
modelo_mice_hc1_2019 <- with(
  imp_2019_poblacion,
  {
    ajuste <- lm(
      poblacion_65_mas ~ fecundidad + esperanza_vida +
        migracion_relativa + log_pib_pc +
        gasto_bolsillo_salud + gasto_publico_salud_pib +
        region
    )
    
    lmtest::coeftest(
      ajuste,
      vcov. = sandwich::vcovHC(ajuste, type = "HC1"),
      save = TRUE
    )
  }
)

# Combinar los resultados mediante las reglas de Rubin
resultado_mice_hc1_2019 <- mice::pool(
  modelo_mice_hc1_2019,
  dfcom = df.residual(modelo_mice_poblacion_2019$analyses[[1]])
)

# Mostrar los resultados robustos combinados
summary(resultado_mice_hc1_2019, conf.int = TRUE)

# Identificar la fila de República Centroafricana
fila_caf <- which(
  base_2019$pais == "Central African Republic"
)

# Comprobar que existe una única observación
stopifnot(length(fila_caf) == 1)

# Recuperar las 20 imputaciones de esperanza de vida
imputaciones_caf_2019 <- as.numeric(
  unlist(
    imp_2019_poblacion$imp$esperanza_vida[
      as.character(fila_caf),
      ,
      drop = FALSE
    ]
  )
)

# Mostrar los 20 valores ordenados
sort(imputaciones_caf_2019)

# Resumir las imputaciones
summary(imputaciones_caf_2019)

# Comparar imputaciones de CAF con la referencia OMS de 2019
comparacion_caf_2019 <- tibble(
  imputacion = imputaciones_caf_2019
) |>
  summarise(
    cantidad = n(),
    mediana = median(imputacion),
    media = mean(imputacion),
    diferencia_mediana_oms = mediana - 53.1,
    imputaciones_mayores_oms = sum(imputacion > 53.1),
    imputaciones_mayores_60 = sum(imputacion > 60)
  )

print(comparacion_caf_2019)


#### Sensibilidad CAF 2019: interpolación lineal con HC1 ####################

# Calcular la esperanza de vida interpolada para CAF en 2019

vida_vecinos_caf <- wdi_limpia |>
  filter(
    codigo_pais == "CAF",
    anio %in% c(2018, 2020)
  ) |>
  arrange(anio) |>
  select(anio, esperanza_vida)

# Comprobar que tenemos los dos años y valores disponibles
stopifnot(
  nrow(vida_vecinos_caf) == 2,
  all(vida_vecinos_caf$anio == c(2018, 2020)),
  all(is.finite(vida_vecinos_caf$esperanza_vida))
)

# Interpolación lineal para 2019
vida_interpolada_caf <- mean(vida_vecinos_caf$esperanza_vida)

print(vida_interpolada_caf)


# Identificar la fila de CAF en la base de 2019

fila_caf <- which(
  base_2019$pais == "Central African Republic"
)

stopifnot(
  length(fila_caf) == 1,
  is.na(base_2019$esperanza_vida[fila_caf])
)


# Estimar las 20 regresiones con CAF interpolada y errores HC1

modelos_hc1_interpolados_2019 <- lapply(
  seq_len(imp_2019_poblacion$m),
function(i) {
    
    # Recuperar una base imputada
    datos <- mice::complete(imp_2019_poblacion, action = i)
    
    # Reemplazar solamente la esperanza de vida de CAF
    datos$esperanza_vida[fila_caf] <- vida_interpolada_caf
    
    # Estimar la regresión
    ajuste <- lm(
      poblacion_65_mas ~
        fecundidad +
        esperanza_vida +
        migracion_relativa +
        log_pib_pc +
        gasto_bolsillo_salud +
        gasto_publico_salud_pib +
        region,
      data = datos
    )
    
    # Calcular errores estándar robustos HC1
    lmtest::coeftest(
      ajuste,
      vcov. = sandwich::vcovHC(ajuste, type = "HC1"),
      save = TRUE
    )
  }
)


# Combinar las regresiones mediante las reglas de Rubin

# Calcular los grados de libertad del modelo completo
gl_2019 <- df.residual(
  modelo_mice_poblacion_2019$analyses[[1]]
)

resultado_hc1_interpolado_2019 <- mice::pool(
  mice::as.mira(modelos_hc1_interpolados_2019),
  dfcom = gl_2019
)

# Mostrar resultados con HC1
summary(resultado_hc1_interpolado_2019, conf.int = TRUE)


# Comparar MICE original vs. CAF interpolada (ambos con HC1)

tabla_original_hc1 <- summary(
  resultado_mice_hc1_2019
) |>
  select(term, estimate, std.error, p.value) |>
  rename(
    coef_original = estimate,
    error_original = std.error,
    p_original = p.value
  )

tabla_interpolada_hc1 <- summary(
  resultado_hc1_interpolado_2019
) |>
  select(term, estimate, std.error, p.value) |>
  rename(
    coef_interpolado = estimate,
    error_interpolado = std.error,
    p_interpolado = p.value
  )

# Unir los resultados y calcular las diferencias
tabla_sensibilidad_caf_hc1 <- tabla_original_hc1 |>
  left_join(
    tabla_interpolada_hc1,
    by = "term",
    relationship = "one-to-one"
  ) |>
  mutate(
    diferencia = coef_interpolado - coef_original
  )

# Mostrar resultados redondeados
tabla_sensibilidad_caf_hc1 |>
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  ) |>
  tibble::as_tibble() |>
  print(n = Inf, width = Inf)


##### Matrices de correlacion #################################################

### 2000
# Seleccionar las variables explicativas numéricas
variables_explicativas_2000 <- base_2000_completa |>
  select(
    fecundidad,
    esperanza_vida,
    migracion_relativa,
    log_pib_pc,
    gasto_bolsillo_salud,
    gasto_publico_salud_pib
  )

# Calcular la matriz de correlaciones
matriz_correlaciones_2000 <- cor(
  variables_explicativas_2000
)

# Mostrar resultados con dos decimales
round(matriz_correlaciones_2000, 2)

vif_2000 <- car::vif(modelo_2000)

vif_2000

### 2010
variables_explicativas_2010 <- base_2010_completa |>
  select(
    fecundidad,
    esperanza_vida,
    migracion_relativa,
    log_pib_pc,
    gasto_bolsillo_salud,
    gasto_publico_salud_pib
  )

# Calcular la matriz de correlaciones
matriz_correlaciones_2010 <- cor(
  variables_explicativas_2010
)

# Mostrar resultados con dos decimales
round(matriz_correlaciones_2010, 2)

vif_2010 <- car::vif(modelo_2010)

vif_2010

### 2019
# Seleccionar las variables explicativas numéricas
variables_explicativas_2019 <- base_2019_completa |>
  select(
    fecundidad,
    esperanza_vida,
    migracion_relativa,
    log_pib_pc,
    gasto_bolsillo_salud,
    gasto_publico_salud_pib
  )

# Calcular la matriz de correlaciones
matriz_correlaciones_2019 <- cor(
  variables_explicativas_2019
)

# Mostrar resultados con dos decimales
round(matriz_correlaciones_2019, 2)

vif_2019 <- car::vif(modelo_2019)

vif_2019

# MAPA DE CALOR: CORRELACIONES ENTRE REGRESORES (2019)

# Convertir la matriz en una tabla para ggplot
correlaciones_2019 <- as.data.frame(
  as.table(matriz_correlaciones_2019)
) |>
  filter(as.integer(Var2) > as.integer(Var1))

# Nombres más legibles para el gráfico
nombres_variables <- c(
  fecundidad = "Fecundidad",
  esperanza_vida = "Esperanza de vida",
  migracion_relativa = "Migración relativa",
  log_pib_pc = "Log PIB pc PPA",
  gasto_bolsillo_salud = "Gasto de bolsillo en salud",
  gasto_publico_salud_pib = "Gasto público en salud"
)

# Construir el mapa de calor
grafico_correlaciones_2019 <- ggplot(
  correlaciones_2019,
  aes(x = Var1, y = Var2, fill = Freq)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(Freq, 2)),
    size = 3.5
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlación"
  ) +
  scale_x_discrete(labels = nombres_variables) +
  scale_y_discrete(labels = nombres_variables) +
  coord_equal() +
  labs(
    title = "Correlaciones entre regresores (2019)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Mostrar el gráfico
grafico_correlaciones_2019

##### Homoscedasticidad #####################################################

# Test de Breusch-Pagan para 2000
bp_2000 <- lmtest::bptest(modelo_2000)

# Test de Breusch-Pagan para 2010
bp_2010 <- lmtest::bptest(modelo_2010)

# Test de Breusch-Pagan para 2019
bp_2019 <- lmtest::bptest(modelo_2019)

# Mostrar resultados
bp_2000
bp_2010
bp_2019

##### Modelos progresivos #####################################################

### 2000
# Modelo 1: demográfico
modelo_1_2000 <- lm(
  poblacion_65_mas ~ fecundidad + esperanza_vida + region,
  data = base_2000_completa
)

# Modelo 2: incorporar migración y PIB
modelo_2_2000 <- lm(
  poblacion_65_mas ~ fecundidad + esperanza_vida +
    migracion_relativa + log_pib_pc + region,
  data = base_2000_completa
)

### 2010
# Modelo 1: demográfico
modelo_1_2010 <- lm(
  poblacion_65_mas ~ fecundidad + esperanza_vida + region,
  data = base_2010_completa
)

# Modelo 2: incorporar migración y PIB
modelo_2_2010 <- lm(
  poblacion_65_mas ~ fecundidad + esperanza_vida +
    migracion_relativa + log_pib_pc + region,
  data = base_2010_completa
)

### 2019
# Modelo 1: variables demográficas
modelo_1_2019 <- lm(
  poblacion_65_mas ~ fecundidad + esperanza_vida + region,
  data = base_2019_completa
)

# Modelo 2: agregar migración y PIB per cápita
modelo_2_2019 <- lm(
  poblacion_65_mas ~ fecundidad + esperanza_vida +
    migracion_relativa + log_pib_pc + region,
  data = base_2019_completa
)

# Modelo 3: ya estimado anteriormente
# modelo_2019


# Función que calcula errores estándar robustos HC1

source("utils/tabla_hc1.R")

# Reunir todas las regresiones MCO ya estimadas

modelos_mco <- list(
  
  # Regresiones principales
  completo_2000 = modelo_2000,
  completo_2010 = modelo_2010,
  completo_2019 = modelo_2019,
  
  # Modelos progresivos
  demografico_2000 = modelo_1_2000,
  intermedio_2000 = modelo_2_2000,
  
  demografico_2010 = modelo_1_2010,
  intermedio_2010 = modelo_2_2010,
  
  demografico_2019 = modelo_1_2019,
  intermedio_2019 = modelo_2_2019
)


# Aplicar HC1 a todos los modelos de la lista
resultados_hc1 <- lapply(
  modelos_mco,
  tabla_hc1
)


# Mostrar las tres regresiones principales
resultados_hc1$completo_2000
resultados_hc1$completo_2010
resultados_hc1$completo_2019

# Función para comparar dos modelos con prueba de Wald robusta

source("utils/wald_hc1.R")

# Guardar los resultados de las seis comparaciones
pruebas_wald_hc1 <- list(
  
  "2000_demografico_vs_intermedio" =
    wald_hc1(modelo_1_2000, modelo_2_2000),
  
  "2000_intermedio_vs_completo" =
    wald_hc1(modelo_2_2000, modelo_2000),
  
  "2010_demografico_vs_intermedio" =
    wald_hc1(modelo_1_2010, modelo_2_2010),
  
  "2010_intermedio_vs_completo" =
    wald_hc1(modelo_2_2010, modelo_2010),
  
  "2019_demografico_vs_intermedio" =
    wald_hc1(modelo_1_2019, modelo_2_2019),
  
  "2019_intermedio_vs_completo" =
    wald_hc1(modelo_2_2019, modelo_2019)
  
)

# Reunir las seis pruebas en una tabla
tabla_wald_hc1 <- purrr::imap_dfr(
  pruebas_wald_hc1,
  
  function(resultado, comparacion) {
    
    tibble(
      anio = as.integer(substr(comparacion, 1, 4)),
      comparacion = comparacion,
      estadistico_F = as.numeric(resultado[2, "F"]),
      gl_numerador = abs(as.numeric(resultado[2, "Df"])),
      gl_denominador = as.numeric(resultado[2, "Res.Df"]),
      p_valor = as.numeric(resultado[2, "Pr(>F)"])
    )
    
  }
)


# Comprobar que tenemos las seis pruebas
stopifnot(
  nrow(tabla_wald_hc1) == 6
)

# Mostrar los resultados
print(tabla_wald_hc1, n = Inf)

##### Tablas de modelos progresivos con HC1 ####################

# Variables que queremos comparar
variables_tabla <- c(
  "fecundidad",
  "esperanza_vida",
  "migracion_relativa",
  "log_pib_pc",
  "gasto_bolsillo_salud",
  "gasto_publico_salud_pib"
)

# Mostrar los tres modelos de cada año
for (anio in c(2000, 2010, 2019)) {
  
  for (etapa in c("demografico", "intermedio", "completo")) {
    
    nombre <- paste0(etapa, "_", anio)
    
    tabla <- resultados_hc1[[nombre]]
    
    variables_presentes <- variables_tabla[
      variables_tabla %in% rownames(tabla)
    ]
    
    cat("\n\nMODELO:", nombre, "\n")
    
    print(signif(
      tabla[
        variables_presentes,
        c("Estimate", "Std. Error", "Pr(>|t|)"),
        drop = FALSE
      ],
      4
    ))
  }
}

# Comparar el ajuste de las nueve regresiones

comparacion_modelos <- tibble(
  modelo = names(modelos_mco),
  
  r2_ajustado = sapply(
    modelos_mco,
    function(modelo) summary(modelo)$adj.r.squared
  ),
  
  observaciones = sapply(
    modelos_mco,
    nobs
  )
)

# Mostrar los resultados
print(comparacion_modelos, n = Inf)

# Guardar resultados de las regresiones

# Crear las carpetas de resultados
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)


# 1. Comparación de los nueve modelos MCO
write_csv(
  comparacion_modelos,
  "output/tablas/comparacion_modelos.csv"
)


# 2. Coeficientes y errores estándar robustos HC1
# Reunimos los resultados de los nueve modelos en una sola tabla

coeficientes_hc1 <- purrr::imap_dfr(
  resultados_hc1,
  function(tabla, nombre_modelo) {
    tibble(
      modelo = nombre_modelo,
      variable = rownames(tabla),
      coeficiente = as.numeric(tabla[, 1]),
      error_estandar_hc1 = as.numeric(tabla[, 2]),
      p_valor = as.numeric(tabla[, 4])
    )
  }
)

write_csv(
  coeficientes_hc1,
  "output/tablas/coeficientes_modelos_hc1.csv"
)


# 3. Resultados MICE con errores robustos
tabla_mice_hc1 <- bind_rows(
  summary(resultado_mice_hc1_2000, conf.int = TRUE) |>
    mutate(anio = 2000),
  
  summary(resultado_mice_hc1_2010, conf.int = TRUE) |>
    mutate(anio = 2010),
  
  summary(resultado_mice_hc1_2019, conf.int = TRUE) |>
    mutate(anio = 2019)
) |>
  relocate(anio)

write_csv(
  tabla_mice_hc1,
  "output/tablas/regresiones_mice_hc1.csv"
)


# 4. Pruebas de Breusch-Pagan
tabla_breusch_pagan <- tibble(
  anio = c(2000, 2010, 2019),
  estadistico = c(
    unname(bp_2000$statistic),
    unname(bp_2010$statistic),
    unname(bp_2019$statistic)
  ),
  p_valor = c(
    bp_2000$p.value,
    bp_2010$p.value,
    bp_2019$p.value
  )
)

write_csv(
  tabla_breusch_pagan,
  "output/tablas/test_breusch_pagan.csv"
)


# 5. Sensibilidad del tratamiento de República Centroafricana
write_csv(
  tabla_sensibilidad_caf_hc1,
  "output/tablas/sensibilidad_caf_2019.csv"
)


# 6. Diagnóstico de outliers de las regresiones
write_csv(
  resumen_outliers,
  "output/tablas/outliers_regresiones.csv"
)


# 7. Gráfico de correlaciones entre regresores
ggsave(
  filename = "output/graficos/correlaciones_regresores_2019.png",
  plot = grafico_correlaciones_2019,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

# 8. Documentar el tratamiento de República Centroafricana
write_csv(
  registro_tratamiento,
  "output/tablas/tratamiento_caf_2019.csv"
)

# 9. Efecto del tratamiento sobre los estadísticos descriptivos
write_csv(
  impacto_limpieza_2019,
  "output/tablas/impacto_tratamiento_caf_2019.csv"
)

# 10. Pruebas de Wald robustas HC1
write_csv(
  tabla_wald_hc1,
  "output/tablas/pruebas_wald_hc1.csv"
)

# 11. Validación final de los resultados

# Archivos que debe generar el script
archivos_esperados <- c(
  "output/tablas/comparacion_modelos.csv",
  "output/tablas/coeficientes_modelos_hc1.csv",
  "output/tablas/regresiones_mice_hc1.csv",
  "output/tablas/test_breusch_pagan.csv",
  "output/tablas/sensibilidad_caf_2019.csv",
  "output/tablas/outliers_regresiones.csv",
  "output/tablas/pruebas_wald_hc1.csv",
  "output/tablas/tratamiento_caf_2019.csv",
  "output/tablas/impacto_tratamiento_caf_2019.csv",
  "output/graficos/correlaciones_regresores_2019.png"
)

# Identificar archivos faltantes
archivos_faltantes <- archivos_esperados[
  !file.exists(archivos_esperados)
]

# Detener la ejecución si falta alguno
if (length(archivos_faltantes) > 0) {
  stop(
    "Faltan archivos en output/: ",
    paste(archivos_faltantes, collapse = ", ")
  )
}

# Confirmar que existen todos
cat(
  "\nValidación final: se encontraron todos los resultados esperados en output/.\n"
)

