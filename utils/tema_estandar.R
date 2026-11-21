# ==============================================================================
# Función: tema_estandar
# ==============================================================================
# Tema base de los gráficos de quintiles: tipografía sans, jerarquía visual
# clara, grilla horizontal suave y sin leyenda.
# ==============================================================================

tema_estandar <- function(base_size = 11) {
  
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.25), color = "#111111", margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.95), color = "#444444", margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.8), color = "#666666", hjust = 0, margin = ggplot2::margin(t = 10)),
      axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.95), color = "#222222"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.9), color = "#222222"),
      axis.text.x = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#E5E5E5", linewidth = 0.5),
      legend.position = "none",
      plot.margin = ggplot2::margin(14, 16, 12, 14)
    )
}
