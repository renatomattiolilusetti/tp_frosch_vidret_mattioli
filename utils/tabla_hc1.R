# ==============================================================================
# Función para calcular errores estándar robustos HC1
# ==============================================================================

tabla_hc1 <- function(modelo) {
  
  lmtest::coeftest(
    modelo,
    vcov. = sandwich::vcovHC(modelo, type = "HC1")
  )
  
}