# Trayectorias del envejecimiento poblacional

**Trabajo práctico final — Ciencia de Datos para Economía y Negocios, FCE-UBA (segundo cuatrimestre de 2026)**  
**Grupo 10**

## Integrantes

- Dolores Frosch
- Renato Mattioli
- Julieta Vidret
- 
## Objetivo e hipótesis de trabajo

El proyecto estudia la transición demográfica hacia estructuras poblacionales más envejecidas y su relación con las condiciones económicas y sanitarias de los países. Buscamos responder dos preguntas relacionadas: **¿se observan patrones comunes en las trayectorias de envejecimiento?** y **¿países con niveles similares de envejecimiento presentan distintas capacidades económicas y sanitarias para afrontarlo?**

La hipótesis de trabajo es que el envejecimiento se asocia con transformaciones demográficas comunes, por lo que es posible identificar perfiles y trayectorias comparables entre países. Sin embargo, esos procesos ocurren en momentos diferentes y en contextos económicos y sanitarios heterogéneos. El análisis es descriptivo y asociativo, no causal.

## Datos

- **Fuente:** [World Development Indicators (WDI), Banco Mundial](https://databank.worldbank.org/source/world-development-indicators).
- **Acceso:** descarga mediante el paquete `WDI` de R, que consulta los indicadores del Banco Mundial.
- **Período de la base general:** 1960–2025 (datos anuales).
- **Unidad de observación:** país-año; cada fila representa un país en un año determinado.
- **Cobertura del recorte documentado:** 195 países, 66 años y 12.870 observaciones país-año. Se excluyen los agregados del Banco Mundial y los territorios no soberanos definidos en el script de limpieza.
- **Variables:** se descargan 50 indicadores demográficos, económicos y sanitarios, incluidos indicadores por edad y sexo para construir pirámides poblacionales. La base procesada contiene 56 columnas, contando identificadores y variables auxiliares.

Entre las principales dimensiones estudiadas se encuentran la fecundidad, la esperanza de vida, el crecimiento poblacional, la proporción de personas de 65 años o más, el PIB per cápita en paridad de poder adquisitivo (PPA), el gasto en salud y la cobertura de servicios sanitarios.

La cobertura efectiva de países y años varía entre análisis, según la disponibilidad de los indicadores seleccionados. Los datos faltantes no se reemplazan automáticamente por cero.

## Análisis realizado

El trabajo combina distintas herramientas para pasar de la descripción de la transición demográfica a la comparación de sus trayectorias y de los contextos en los que ocurre:

1. **Descarga y limpieza (`01`–`02`):** selección de indicadores WDI, exclusión de agregados y territorios definidos, estandarización de nombres y validación de la clave país-año.
2. **Exploración de la base (`03`–`05`):** estadísticos descriptivos, diccionario de variables, diagnóstico de datos faltantes por región y período, identificación y revisión de valores atípicos, y gráficos de evolución poblacional y demográfica.
3. **Regresiones lineales (`06`):** modelos progresivos para 2000, 2010 y 2019 que relacionan el porcentaje de población de 65 años o más con indicadores demográficos, económicos y de financiamiento sanitario. Se incluyen diagnósticos, errores estándar robustos HC1 y análisis de sensibilidad al tratamiento de faltantes mediante imputación múltiple.
4. **Perfiles demográficos (`07`):** análisis de componentes principales (PCA) y clustering jerárquico para 1980, 1995, 2010 y 2024, con comparación de perfiles y trayectorias de países.
5. **Capacidad de respuesta sanitaria y económica (`08`):** PCA y clustering para 2023 a partir de cobertura de servicios esenciales de salud, gasto de bolsillo y PIB per cápita PPA.
6. **Dependencia mayor y gasto sanitario (`09`):** clasificación de países en quintiles de dependencia de personas mayores en 2019 y comparación del gasto en salud mediante estadísticas descriptivas, ANOVA y comparaciones de Tukey.
7. **Trayectorias continuas (`10`):** extensión del clustering demográfico al período 1961–2024, con diagnóstico y tratamiento documentado de faltantes, para analizar cambios de perfil a lo largo del tiempo.
8. **Visualizaciones exploratoria y comunicativa (`11`):** proyección de perfiles demográficos sobre componentes principales y mapas comparativos de 1980 y 2024.

Las decisiones sobre exclusiones, imputaciones y valores atípicos se documentan en los scripts correspondientes. 

## Estructura del repositorio

```text
proyecto/
├── README.md
├── raw/
│   └── wdi_raw.csv                         # Descarga original de WDI
├── input/
│   └── wdi_limpia.csv                      # Base preparada para el análisis
├── auxiliar/                               # Bases intermedias reutilizadas
├── output/
│   ├── tablas/                             # Tablas y diagnósticos exportados
│   └── graficos/                           # Gráficos exportados
├── script/
│   ├── 01_descarga.R
│   ├── 02_limpieza.R
│   ├── 03_analisis_descriptivo.R
│   ├── 04_analisis_outliers_y_na.R
│   ├── 05_graficos_descriptivos.R
│   ├── 06_regresiones.R
│   ├── 07_clustering_demografico.R
│   ├── 08_clustering_respuesta_sanitaria_y_economica.R
│   ├── 09_quintiles.R
│   ├── 10_clustering_continuo.R
│   └── 11_graficos_exploratorio_comunicativo.R
└── utils/                                  # Funciones auxiliares utilizadas por los scripts
```

Los scripts utilizan rutas relativas a la carpeta raíz del proyecto. Los archivos de `utils/` forman parte del código necesario para reproducir los análisis; no deben omitirse al subir el repositorio. La carpeta `auxiliar/` incluye, entre otros archivos generados, la muestra de países para los cortes del clustering demográfico.

## Reproducción del análisis

### Paquetes necesario

Instalar los paquetes requeridos:

```r
install.packages(c(
  "tidyverse", "WDI", "mice", "naniar", "car", "lmtest", "sandwich",
  "cluster", "FactoMineR", "factoextra", "gt", "webshot2",
  "scales", "patchwork", "ggrepel", "sf", "rnaturalearth",
  "rnaturalearthdata", "ggtext", "zoo", "lattice"
))
```

### Orden de ejecución

Con la carpeta raíz como directorio de trabajo, ejecutar en la consola de RStudio:

```r
source("script/01_descarga.R")
source("script/02_limpieza.R")
source("script/03_analisis_descriptivo.R")
source("script/04_analisis_outliers_y_na.R")
source("script/05_graficos_descriptivos.R")
source("script/06_regresiones.R")
source("script/07_clustering_demografico.R")
source("script/08_clustering_respuesta_sanitaria_y_economica.R")
source("script/09_quintiles.R")
source("script/10_clustering_continuo.R")
source("script/11_graficos_exploratorio_comunicativo.R")
```

**Dependencias entre etapas:** `01` genera `raw/wdi_raw.csv`; `02` genera `input/wdi_limpia.csv`, que utilizan los análisis siguientes. `04` exporta diagnósticos utilizados en `07`, mientras que `07` produce archivos que requieren `10` y `11`. Los scripts recurren además a funciones propias alojadas en `utils/`.

**Descarga y reproducibilidad:** el script `01` necesita conexión a Internet y puede obtener revisiones posteriores de los indicadores. Para reproducir exactamente una versión ya descargada, conservar su archivo `raw/wdi_raw.csv` y comenzar desde `02`. Antes de ejecutar `11`, verificar que esté resuelto cualquier conflicto de Git pendiente en ese archivo.

**Resultados:** los scripts guardan las tablas en `output/tablas/` y los gráficos que exportan en `output/graficos/`. Algunos gráficos descriptivos del script `05` se muestran en el panel *Plots* de RStudio sin guardarse automáticamente en un archivo.

## Conclusiones principales
