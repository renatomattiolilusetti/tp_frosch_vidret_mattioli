# ==============================================================================
# Función: crear_mapa
# ==============================================================================
# Dibuja el mapa de perfiles de un año, con un encabezado que dice cuántos
# países tienen perfil avanzado.
# ==============================================================================

crear_mapa <- function(mapas, anio_elegido, cantidad_avanzados, paleta, color_titulo) {
  
  datos <- mapas |>
    dplyr::filter(anio == anio_elegido)
  
  ggplot2::ggplot(datos) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = perfil_mapa),
      color = "white", linewidth = 0.075
    ) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = paleta,
      breaks = names(paleta),
      limits = names(paleta),
      drop = FALSE,
      guide = ggplot2::guide_legend(nrow = 1)
    ) +
    ggplot2::coord_sf(crs = "+proj=robin", datum = NA, expand = FALSE) +
    ggplot2::labs(
      title = paste0(anio_elegido, "  ·  ", cantidad_avanzados,
                     " países con perfil avanzado")
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "#F2F8FD", color = NA),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold", size = 14.5, color = color_titulo,
        hjust = 0.5, margin = ggplot2::margin(b = 7)
      ),
      plot.margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 0),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 11.2, color = color_titulo),
      legend.key.width = grid::unit(0.75, "cm"),
      legend.key.height = grid::unit(0.36, "cm")
    )
}
