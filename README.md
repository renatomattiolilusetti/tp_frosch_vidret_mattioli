# Trayectorias del envejecimiento poblacional

**Trabajo práctico final — Ciencia de Datos para Economía y Negocios, FCE-UBA (segundo cuatrimestre de 2026) — Grupo 10**

## Integrantes

- Dolores Frosch
- Renato Mattioli
- Julieta Vidret

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

### Paquetes necesarios

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

**Descarga y reproducibilidad:** el script `01` necesita conexión a Internet y puede obtener revisiones posteriores de los indicadores. Para reproducir exactamente una versión ya descargada, conservar su archivo `raw/wdi_raw.csv` y comenzar desde `02`.

**Resultados:** los scripts guardan las tablas en `output/tablas/` y los gráficos que exportan en `output/graficos/`. Algunos gráficos descriptivos del script `05` se muestran en el panel *Plots* de RStudio sin guardarse automáticamente en un archivo.


## Conclusiones principales

Los resultados obtenidos respaldan la existencia de patrones demográficos comunes en las trayectorias de envejecimiento, aunque muestran que estas transiciones no ocurren simultáneamente ni bajo las mismas condiciones económicas y sanitarias. En este sentido, la evidencia sostiene la primera parte de nuestra hipótesis, con matices, y aporta respaldo parcial a la segunda.

### 1. Patrones comunes en las trayectorias de envejecimiento

El análisis de PCA y clustering permitió identificar dos perfiles demográficos comparables en los distintos cortes temporales: uno **joven**, caracterizado por mayor fecundidad y crecimiento poblacional, y otro **avanzado**, asociado con una mayor proporción de población de 65 años o más y una mayor esperanza de vida.

Entre 1980 y 2024, la cantidad de países clasificados en el perfil joven disminuyó de 117 a 84, mientras que los del perfil avanzado aumentaron de 77 a 110. Sin embargo, la transición no fue simultánea: los países del perfil avanzado pasaron a ser mayoría recién en 2010 y buena parte de África Subsahariana permanece en el perfil joven.

Además, los perfiles no permanecieron constantes a lo largo del tiempo. El perfil joven de 2024 presenta características cercanas a las del avanzado de 1961, lo que muestra que incluso los países relativamente menos envejecidos atravesaron importantes transformaciones demográficas.

Las regresiones lineales complementan estos resultados: en 2000, 2010 y 2019, la fecundidad presenta una asociación negativa con la proporción de población mayor de 65 años, mientras que la esperanza de vida muestra una asociación positiva. La persistencia de estos signos es consistente con la existencia de regularidades demográficas compartidas, aunque la robustez estadística de algunas asociaciones depende del tratamiento de los datos faltantes.

### 2. Heterogeneidad de los contextos económicos y sanitarios

La segunda parte de la hipótesis encuentra respaldo parcial. Los resultados sugieren que niveles similares de envejecimiento pueden coexistir con condiciones económicas y sanitarias diferentes.

El análisis de quintiles de dependencia mayor para 2019 muestra que el gasto en salud no aumenta de manera uniforme con el envejecimiento. Los primeros cuatro quintiles presentan niveles promedio de gasto relativamente similares, entre 5,37% y 6,87% del PIB, mientras que el quinto alcanza el 8,83%. Las comparaciones de Tukey identifican diferencias estadísticamente significativas entre el quintil de mayor dependencia y cada uno de los cuatro restantes.

Por su parte, las regresiones muestran que la incorporación de variables de financiamiento sanitario mejora el ajuste de los modelos, especialmente en 2010 y 2019, años en los que estas variables aportan información estadísticamente significativa de manera conjunta. Esto sugiere que las diferencias internacionales de envejecimiento se relacionan no solo con características demográficas, sino también con las condiciones de financiamiento de los sistemas sanitarios.

Como análisis complementario, el PCA y clustering de indicadores económicos y sanitarios permite identificar distintas configuraciones de ingreso, cobertura de servicios de salud y participación del gasto de bolsillo entre países.

### 3. Limitaciones y próximos pasos

Los resultados deben interpretarse como evidencia descriptiva y asociativa, sin establecer relaciones causales. Identificamos tres limitaciones principales que orientan posibles extensiones del trabajo:

- **Simplificación de las trayectorias demográficas:** la clasificación en dos perfiles permite reconocer patrones generales, pero puede ocultar situaciones intermedias. Los cambios no lineales de clasificación de algunos países motivan explorar un tercer perfil, estableciendo criterios mínimos de tamaño y calidad de los grupos.

- **Alcance temporal de las comparaciones:** aunque el clustering continuo permite estudiar la evolución demográfica, las regresiones y el análisis de quintiles se basan en cortes transversales. Una extensión sería construir modelos de datos de panel que permitan seguir a los países a lo largo del tiempo y estudiar sus cambios dentro de una misma estructura analítica.

- **Medición parcial de la capacidad de respuesta:** el gasto y los indicadores sanitarios utilizados no permiten caracterizar integralmente la preparación de los países frente al envejecimiento. Además, la disponibilidad desigual de información condiciona las muestras y los períodos analizados. Como próximo paso, sería relevante incorporar indicadores de protección social, sistemas previsionales, cuidados de larga duración y gasto sanitario por grupos de edad.

En síntesis, **el envejecimiento poblacional presenta regularidades demográficas compartidas, pero sus trayectorias son heterogéneas y se desarrollan en contextos económicos y sanitarios diferentes**. Reconocer estas diferencias resulta fundamental para comprender por qué países que atraviesan procesos demográficos comparables pueden enfrentar desafíos distintos.
