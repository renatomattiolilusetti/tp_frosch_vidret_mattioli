# ==============================================================================
# Función para comparar modelos mediante Wald robusto HC1
# ==============================================================================

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