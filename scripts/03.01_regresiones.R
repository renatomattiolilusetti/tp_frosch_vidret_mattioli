################################################################################
#### Regresión lineal en tres momentos #########################################

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

diagnosticar_outliers <- function(anio_objetivo) {
    base_anio <- wdi_limpia |>
    filter(anio == anio_objetivo) |>
    mutate(
      migracion_relativa = 100 * migracion_neta / poblacion_total,
      log_pib_pc = log(pib_per_capita_ppa)
    ) |>
    select(codigo_pais, pais, all_of(variables_atipicos))
  
  # Identificar países por código evita problemas con nombres repetidos.
  stopifnot(!anyNA(base_anio$codigo_pais))
  stopifnot(!anyDuplicated(base_anio$codigo_pais))
  
  # Control igual para 2000, 2010 y 2019: datos válidos, faltantes, infinitos.
  disponibilidad <- base_anio |>
    pivot_longer(
      cols = all_of(variables_atipicos),
      names_to = "variable", values_to = "valor"
    ) |>
    group_by(variable) |>
    summarise(
      disponibles = sum(is.finite(valor)),
      faltantes = sum(is.na(valor)),
      infinitos = sum(is.infinite(valor)),
      .groups = "drop"
  ) |>
  mutate(
    porcentaje_faltantes = round(
      100 * faltantes /
        (disponibles + faltantes + infinitos),
      2
    )
  )
    if (any(disponibilidad$infinitos > 0)) {
    stop(paste("Hay infinitos en el año", anio_objetivo,
               ": revisar cocientes y logaritmos antes del IQR."))
  }
  
  # IQR por variable, utilizando las observaciones disponibles.
  outliers <- base_anio |>
    pivot_longer(
      cols = all_of(variables_atipicos),
      names_to = "variable", values_to = "valor",
      values_drop_na = TRUE
    ) |>
    group_by(variable) |>
    mutate(
      q1 = quantile(valor, 0.25),
      q3 = quantile(valor, 0.75),
      rango_iqr = q3 - q1,
      limite_inferior = q1 - 1.5 * rango_iqr,
      limite_superior = q3 + 1.5 * rango_iqr
    ) |>
    filter(valor < limite_inferior | valor > limite_superior) |>
    ungroup() |>
    select(codigo_pais, pais, variable, valor,
           limite_inferior, limite_superior) |>
    arrange(variable, valor)
  
  # Ventana temporal de 5 años a cada lado, sin incluir el central
  trayectorias <- wdi_limpia |>
    filter(
      codigo_pais %in% outliers$codigo_pais,
      anio >= anio_objetivo - 5,
      anio <= anio_objetivo + 5
    ) |>
    mutate(
      migracion_relativa = 100 * migracion_neta / poblacion_total,
      log_pib_pc = log(pib_per_capita_ppa)
    ) |>
    select(codigo_pais, pais, anio, all_of(variables_atipicos)) |>
    pivot_longer(
      cols = all_of(variables_atipicos),
      names_to = "variable", values_to = "valor"
    ) |>
    semi_join(
      outliers |> select(codigo_pais, variable),
      by = c("codigo_pais", "variable")
    )
  
  stopifnot(!anyDuplicated(
    trayectorias |> select(codigo_pais, variable, anio)
  ))
  
  # Comparación local y MAD 
  resumen_temporal <- trayectorias |>
    filter(anio != anio_objetivo) |>
    group_by(codigo_pais, pais, variable) |>
    summarise(
      anios_antes = sum(anio < anio_objetivo & !is.na(valor)),
      anios_despues = sum(anio > anio_objetivo & !is.na(valor)),
      mediana_vecinos = if (all(is.na(valor))) NA_real_
      else median(valor, na.rm = TRUE),
      minimo_vecinos = if (all(is.na(valor))) NA_real_
      else min(valor, na.rm = TRUE),
      maximo_vecinos = if (all(is.na(valor))) NA_real_
      else max(valor, na.rm = TRUE),
      mad_vecinos = if (all(is.na(valor))) NA_real_
      else mad(valor, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      outliers |> select(codigo_pais, variable, valor),
      by = c("codigo_pais", "variable"),
      relationship = "one-to-one"
    ) |>
    mutate(
      revision_temporal = case_when(
        anios_antes < 3 | anios_despues < 3 ~ "Cobertura insuficiente",
        valor < minimo_vecinos | valor > maximo_vecinos ~
          "Fuera del rango local",
        TRUE ~ "Dentro del rango local"
      ),
      desvio_robusto = case_when(
        is.na(mad_vecinos) | mad_vecinos == 0 ~ NA_real_,
        TRUE ~ abs(valor - mediana_vecinos) / mad_vecinos
      )
    ) |>
    arrange(desc(desvio_robusto))
  
  # Selección uniforme para inspección visual, NO para eliminación:
  # >3 MAD, fuera del rango local, cobertura insuficiente
  casos_prioritarios <- resumen_temporal |>
    filter(
      desvio_robusto > 3 |
        revision_temporal != "Dentro del rango local" |
        (anio_objetivo == 2019 & codigo_pais == "CAF" &
           variable == "esperanza_vida")
    ) |>
    select(codigo_pais, variable)
  
  trayectorias_prioritarias <- trayectorias |>
    semi_join(casos_prioritarios, by = c("codigo_pais", "variable"))
  
  grafico <- if (nrow(trayectorias_prioritarias) > 0) {
    ggplot(trayectorias_prioritarias, aes(x = anio, y = valor)) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = anio_objetivo, linetype = "dashed") +
      facet_wrap(~ pais + variable, scales = "free_y", ncol = 2) +
      labs(
        title = paste("Revisión temporal de outliers:", anio_objetivo),
        subtitle = paste(anio_objetivo - 5, "a", anio_objetivo + 5),
        x = "Año", y = "Valor de la variable"
      ) +
      theme_minimal()
  } else {
    NULL
  }
  
  list(
    disponibilidad = disponibilidad,
    outliers = outliers,
    trayectorias = trayectorias,
    resumen_temporal = resumen_temporal,
    casos_prioritarios = casos_prioritarios,
    trayectorias_prioritarias = trayectorias_prioritarias,
    grafico = grafico
  )
}

# Ejecutar el MISMO diagnóstico para los tres cortes.
diag_2000 <- diagnosticar_outliers(2000)
diag_2010 <- diagnosticar_outliers(2010)
diag_2019 <- diagnosticar_outliers(2019)

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
  dfcom = 182
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
  dfcom = 182
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
  dfcom = 182
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

tabla_hc1 <- function(modelo) {
  lmtest::coeftest(
    modelo,
    vcov. = sandwich::vcovHC(modelo, type = "HC1")
  )
}

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
wald_hc1 <- function(modelo_reducido, modelo_completo) {
  
  lmtest::waldtest(
    modelo_reducido,
    modelo_completo,
    vcov = function(modelo) {
      sandwich::vcovHC(modelo, type = "HC1")
    },
    test = "F"
  )
}

# Año 2000
wald_hc1(modelo_1_2000, modelo_2_2000)
wald_hc1(modelo_2_2000, modelo_2000)

# Año 2010
wald_hc1(modelo_1_2010, modelo_2_2010)
wald_hc1(modelo_2_2010, modelo_2010)

# Año 2019
wald_hc1(modelo_1_2019, modelo_2_2019)
wald_hc1(modelo_2_2019, modelo_2019)

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

