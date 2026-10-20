################################################################################
#### Regresión lineal en tres momentos #########################################

#### Regresion 2000 ############################################################
#### Preparación de las variables ##############################################
base_2000 <- wdi_limpia |>
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

### Tabla de comparación y resultados ##########################################
coeficientes_casos <- summary(modelo_2000)$coefficients

tabla_comparacion_2000 <- tibble(
  variable = rownames(coeficientes_casos),
  beta_casos = coeficientes_casos[, "Estimate"],
  p_casos = coeficientes_casos[, "Pr(>|t|)"]
) |>
  left_join(
    summary(resultado_mice_poblacion_2000) |>
      transmute(variable = term, beta_mice = estimate, p_mice = p.value),
    by = "variable"
  ) |>
  filter(variable %in% c(
    "fecundidad", "esperanza_vida", "migracion_relativa",
    "log_pib_pc", "gasto_bolsillo_salud", "gasto_publico_salud_pib"
  )) |>
  mutate(across(where(is.numeric), ~ signif(.x, 3)))

print(tabla_comparacion_2000)

#### Regresion 2010 ############################################################

base_2010 <- wdi_limpia |>
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

# Extraer los valores imputados de gasto público en salud
valores_salud_2010 <- unlist(
  imp_2010_poblacion$imp$gasto_publico_salud_pib
)

# Resumen de las imputaciones
summary(valores_salud_2010)

# Contar cuántas superan el 10% del PIB
sum(valores_salud_2010 > 10)

# Identificar los valores superiores al 10%
sort(unique(valores_salud_2010[valores_salud_2010 > 10]))

# Identificar el país que recibió la imputación elevada
imp_2010_poblacion$imp$gasto_publico_salud_pib |>
  tibble::rownames_to_column("fila") |>
  pivot_longer(
    cols = -fila,
    names_to = "numero_imputacion",
    values_to = "gasto_imputado"
  ) |>
  filter(gasto_imputado > 10) |>
  mutate(
    pais = base_2010$pais[as.integer(fila)],
    region = base_2010$region[as.integer(fila)]
  ) |>
  select(pais, region, numero_imputacion, gasto_imputado)


# Identificar de qué país procede el valor observado

base_2010 |>
  filter(
    !is.na(gasto_publico_salud_pib),
    abs(gasto_publico_salud_pib - 13.19274) < 0.001
  ) |>
  select(pais, region, gasto_publico_salud_pib)

# Identificar Montenegro
fila_montenegro <- which(base_2010$pais == "Montenegro")

# Recuperar sus 20 imputaciones
imputaciones_montenegro <- as.numeric(
  imp_2010_poblacion$imp$gasto_publico_salud_pib[
    as.character(fila_montenegro),
  ]
)

# Ver los valores imputados
imputaciones_montenegro

# Resumir sus imputaciones
summary(imputaciones_montenegro)

# Comparar con los valores observados de su región
base_2010 |>
  filter(
    region == base_2010$region[fila_montenegro],
    !is.na(gasto_publico_salud_pib)
  ) |>
  summarise(
    cantidad_paises = n(),
    minimo = min(gasto_publico_salud_pib),
    mediana = median(gasto_publico_salud_pib),
    maximo = max(gasto_publico_salud_pib)
  )

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

# Comparar cobertura de posibles años

cobertura_anios <- wdi_limpia |>
  filter(anio %in% c(2019, 2022, 2023)) |>
  mutate(
    migracion_relativa = 100 * migracion_neta / poblacion_total,
    log_pib_pc = if_else(
      pib_per_capita_ppa > 0,
      log(pib_per_capita_ppa),
      NA_real_
    )
  ) |>
  group_by(anio) |>
  summarise(
    paises = n(),
    completos = sum(complete.cases(
      poblacion_65_mas,
      fecundidad,
      esperanza_vida,
      migracion_relativa,
      log_pib_pc,
      gasto_bolsillo_salud,
      gasto_publico_salud_pib,
      region
    )),
    .groups = "drop"
  )

cobertura_anios

#### Regresion 2019 ############################################################

# Preparar la base de 2019
base_2019 <- wdi_limpia |>
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

#### Regresion 2010 ############################################################

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

# Comparar los modelos
summary(modelo_1_2000)
summary(modelo_2_2000)

# Contrastes F de variables adicionales
anova(modelo_1_2000, modelo_2_2000)
anova(modelo_2_2000, modelo_2000)

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

# Comparar los modelos
summary(modelo_1_2010)
summary(modelo_2_2010)

# Contrastes F de variables adicionales
anova(modelo_1_2010, modelo_2_2010)
anova(modelo_2_2010, modelo_2010)

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

# Ver resultados
summary(modelo_1_2019)
summary(modelo_2_2019)

# Comparación de los tres modelos progresivos
anova(modelo_1_2019, modelo_2_2019)
anova(modelo_2_2019, modelo_2019)

##### Comparacion entre periodos ###############################################

# Identificar países presentes en las tres regresiones
paises_comunes <- intersect(
  intersect(
    base_2000_completa$pais,
    base_2010_completa$pais
  ),
  base_2019_completa$pais
)

# Contar los países comunes
length(paises_comunes)

# Seleccionar los mismos 179 países en cada año

base_2010_comunes <- base_2010_completa |>
  filter(pais %in% paises_comunes)

base_2019_comunes <- base_2019_completa |>
  filter(pais %in% paises_comunes)

# Verificar cantidad de países

nrow(base_2010_comunes)
nrow(base_2019_comunes)

# Reestimar los modelos con la muestra común

modelo_2010_comunes <- update(
  modelo_2010,
  data = base_2010_comunes
)

modelo_2019_comunes <- update(
  modelo_2019,
  data = base_2019_comunes
)

# Consultar resultados

summary(modelo_2010_comunes)
summary(modelo_2019_comunes)