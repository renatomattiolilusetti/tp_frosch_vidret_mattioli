# ==============================================================================
# Función para diagnosticar outliers
# ==============================================================================

diagnosticar_outliers <- function(anio_objetivo, wdi_limpia, variables_atipicos) {
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
    ggplot(
      trayectorias_prioritarias |> filter(is.finite(valor)),
      aes(x = anio, y = valor)
    ) +
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