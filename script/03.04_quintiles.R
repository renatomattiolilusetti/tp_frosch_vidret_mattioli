# ==============================================================================
# ANÁLISIS GLOBAL: QUINTILES DE TASA DE DEPENDENCIA MAYOR Y GASTO EN SALUD (2019)
# ==============================================================================

library(tidyverse)

# 1. Cargar la base de datos limpia
wdi_limpia <- read_csv("input/wdi_limpia.csv")

# 2. Calcular la Tasa de Dependencia Mayor (DM) para el año 2019 de forma dinámica
datos_2019_demografia <- wdi_limpia %>% 
  filter(anio == 2019) %>% 
  mutate(dep_mayor_2019 = (poblacion_65_mas / poblacion_15_64) * 100) %>% 
  select(pais, dep_mayor_2019) %>% 
  drop_na(dep_mayor_2019)

# 3. Filtrar el Gasto en Salud (% del PIB) para el año 2019 (sin traer columnas con NAs innecesarios)
datos_salud_2019 <- wdi_limpia %>% 
  filter(anio == 2019) %>% 
  select(pais, gasto_salud_pib) %>%  # Nos quedamos solo con país y gasto
  drop_na(gasto_salud_pib)

# 4. Unir ambas fuentes de datos por país (Sincronizado en 2019)
df_quintiles <- datos_2019_demografia %>% 
  inner_join(datos_salud_2019, by = "pais")

# 5. Creación de los 5 Quintiles No Paramétricos (ntile)
df_quintiles <- df_quintiles %>% 
  mutate(
    quintil_DM = ntile(dep_mayor_2019, 5),
    quintil_DM = factor(
      quintil_DM, 
      labels = c("Q1 (Baja)", "Q2 (Media-Baja)", "Q3 (Media)", "Q4 (Media-Alta)", "Q5 (Alta)")
    )
  )

# 6. Construcción de la Tabla Resumen Descriptiva
tabla_resumen <- df_quintiles %>% 
  group_by(quintil_DM) %>% 
  summarise(
    n_paises = n(),
    min_DM = min(dep_mayor_2019),
    max_DM = max(dep_mayor_2019),
    promedio_DM = mean(dep_mayor_2019),
    promedio_salud_pib = mean(gasto_salud_pib),
    sd_salud_pib = sd(gasto_salud_pib),
    mediana_salud_pib = median(gasto_salud_pib),
    .groups = "drop"
  )

print("--- TABLA DESCRIPTIVA DE QUINTILES (2019) ---")
print(tabla_resumen)

# Identificar países excluidos de manera automática
paises_totales <- unique(wdi_limpia$pais)
paises_en_analisis <- unique(df_quintiles$pais)
paises_excluidos <- setdiff(paises_totales, paises_en_analisis)

print("Países excluidos:")
print(paises_excluidos)


# 7. Evaluación Inferencial: ANOVA de Un Factor
modelo_anova <- aov(gasto_salud_pib ~ quintil_DM, data = df_quintiles)
summary_anova <- summary(modelo_anova)

print("--- RESULTADOS DEL TEST ANOVA (2019) ---")
print(summary_anova)


# 8. Tamaño del Efecto (Eta Cuadrado - Eta^2)
ss_efecto <- summary_anova[[1]]["quintil_DM", "Sum Sq"]
ss_total <- sum(summary_anova[[1]][, "Sum Sq"])
eta_cuadrado <- ss_efecto / ss_total

cat(sprintf("Tamaño del efecto (Eta^2): %.4f (%.2f%% de la varianza explicada)\n", 
            eta_cuadrado, eta_cuadrado * 100))

# 9. Test Post-Hoc de Tukey HSD
tukey_res <- TukeyHSD(modelo_anova)
print("--- TEST POST-HOC DE TUKEY HSD (2019) ---")
print(tukey_res)


# 10. Detección de Outliers (Criterio IQR / Boxplot)
outliers_gasto <- df_quintiles %>%
  group_by(quintil_DM) %>%
  mutate(
    Q1_val = quantile(gasto_salud_pib, 0.25),
    Q3_val = quantile(gasto_salud_pib, 0.75),
    IQR_val = IQR(gasto_salud_pib),
    limite_superior = Q3_val + 1.5 * IQR_val,
    limite_inferior = Q1_val - 1.5 * IQR_val
  ) %>%
  filter(gasto_salud_pib > limite_superior | gasto_salud_pib < limite_inferior) %>%
  select(pais, quintil_DM, gasto_salud_pib, limite_superior)

print("--- OUTLIERS DETECTADOS (2019) ---")
print(outliers_gasto)


# 11. Visualización: Gráfico de Barras con Promedios y Jitter (Muestra dinámica)
promedios <- df_quintiles %>% 
  group_by(quintil_DM) %>% 
  summarise(prom_salud = mean(gasto_salud_pib), .groups = "drop")

grafico_barras <- ggplot() +
  geom_col(data = promedios, aes(x = quintil_DM, y = prom_salud, fill = quintil_DM), 
           width = 0.55, color = "black", alpha = 0.85) +
  geom_jitter(data = df_quintiles, aes(x = quintil_DM, y = gasto_salud_pib), 
              width = 0.15, alpha = 0.4, size = 1.8, color = "black") +
  geom_text(data = promedios, aes(x = quintil_DM, y = prom_salud, label = sprintf("%.2f%%", prom_salud)), 
            vjust = -0.5, fontface = "bold", size = 3.8) +
  scale_fill_manual(values = c("#2b5c8f", "#4682b4", "#6baed6", "#d95f02", "#e7298a")) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 2)) +
  labs(
    title = "Gasto Promedio en Salud (% del PIB) según Quintil de Dependencia Mayor (2019)",
    subtitle = paste0("Análisis comparativo global (N = ", nrow(df_quintiles), " países)"),
    x = "Quintiles de Tasa de Dependencia Mayor (2019)",
    y = "Gasto en Salud (% del PIB)",
    fill = "Quintil"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "none",
    axis.title = element_text(face = "bold")
  )

print(grafico_barras)


# 12. Generación del Boxplot actualizado para el año 2019
grafico_boxplot <- ggplot(df_quintiles, aes(x = quintil_DM, y = gasto_salud_pib, fill = quintil_DM)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, alpha = 0.8) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Gasto en Salud (% del PIB) según Quintiles de Dependencia Mayor (2019)",
    subtitle = paste0("Distribución internacional y dispersión por cuartiles (N = ", nrow(df_quintiles), ")"),
    x = "Quintiles de Carga Demográfica de Adultos Mayores (DM 2019)",
    y = "Gasto en Salud (% del PIB)",
    fill = "Quintil"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold")
  )

print(grafico_boxplot)


# 13. Tablas agrupadas por Quintil (con y sin PIB per cápita)
paises_agrupados <- df_quintiles %>%
  select(quintil_DM, pais, dep_mayor_2019, gasto_salud_pib) %>%
  arrange(quintil_DM, pais)

View(paises_agrupados)

# Extraer el PIB per cápita PPA de 2019 desde la base limpia
datos_pib_2019 <- wdi_limpia %>% 
  filter(anio == 2019) %>% 
  select(pais, pib_per_capita_ppa)

# Unir el PIB al dataframe de quintiles sin modificar el modelo principal
df_quintiles_con_pib <- df_quintiles %>% 
  left_join(datos_pib_2019, by = "pais")

# Armar la tabla agrupada por Quintil con País, Tasa DM, Gasto en Salud y PIB per cápita
paises_agrupados_pib <- df_quintiles_con_pib %>% 
  select(quintil_DM, pais, dep_mayor_2019, gasto_salud_pib, pib_per_capita_ppa) %>% 
  arrange(quintil_DM, desc(dep_mayor_2019))