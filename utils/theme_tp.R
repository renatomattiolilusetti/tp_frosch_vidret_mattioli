# ==============================================================================
# Función: theme_tp
# ==============================================================================

# Tema común para todos los gráficos del TP: tipografía, colores de los textos,
# grilla suave, leyenda abajo y fondo blanco. Cada gráfico le suma lo propio.

theme_tp <- function() {
  
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(
        size = 17, face = "bold", color = "#19354D",
        hjust = 0.5, lineheight = 1.08, margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 11.5, color = "#38536B",
        hjust = 0.5, margin = margin(b = 14)
      ),
      plot.caption = element_text(
        size = 9.5, color = "#4F6478",
        hjust = 0.5, margin = margin(t = 10)
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5EBEF", linewidth = 0.35),
      legend.position = "bottom",
      legend.text = element_text(size = 11, color = "#38536B"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(14, 16, 12, 16)
    )
}