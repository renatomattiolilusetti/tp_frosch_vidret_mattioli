# ==============================================================================
# Función: mostrar_trayectoria
# ==============================================================================
# Recibe un caso candidato a outlier (una fila del diagnóstico vertical) y la
# base en formato largo. Muestra los valores de esa variable en ese país desde
# cinco años antes hasta cinco años después del candidato.
# ==============================================================================

mostrar_trayectoria <- function(caso, datos) {
  
  if (nrow(caso) == 0) {
    message("No hay candidatos en esta categoría.")
    return(invisible(NULL))
  }
  
  datos |>
    dplyr::filter(
      codigo_pais == caso$codigo_pais[[1]],
      variable == caso$variable[[1]],
      anio >= caso$anio_candidato[[1]] - 5,
      anio <= caso$anio_candidato[[1]] + 5
    ) |>
    dplyr::select(pais, variable, anio, valor) |>
    dplyr::arrange(anio) |>
    print(n = Inf)
}
