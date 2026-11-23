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

Las variables principales incluyen población total y crecimiento, natalidad, mortalidad, migración neta, fecundidad, esperanza de vida, proporciones de población de 15 a 64 y de 65 años o más, PIB per cápita en paridad de poder adquisitivo (PPA), gasto en salud y cobertura de servicios sanitarios. Algunas variables derivadas se construyen dentro de los scripts; por ejemplo, la tasa de dependencia mayor se calcula como la proporción de población de 65 años o más dividida por la proporción de población de 15 a 64 años, multiplicada por 100.

La cobertura efectiva de países y años varía según el indicador y el análisis. Los valores faltantes se diagnostican y su tratamiento se documenta para cada procedimiento. No se consideran automáticamente equivalentes a cero. Los resultados de muestras o cortes temporales diferentes no deben interpretarse como si provinieran de un mismo panel completo.

## Recorrido del análisis

El trabajo parte de la caracterización de la transición demográfica, identifica perfiles de envejecimiento y, por último, compara los contextos económicos y sanitarios en los que se desarrolla.

1. **Descarga y limpieza (`01`–`02`).** Selección de indicadores, conservación de la descarga original, exclusión de agregados y territorios definidos, estandarización de nombres y validación de la clave país-año.
2. **Exploración y estadísticas descriptivas (`03`–`05`).** Descripción de las principales variables, análisis de faltantes por región y período, detección y revisión de valores atípicos, y gráficos de evolución de la población y de indicadores demográficos.
3. **Regresiones lineales (`06`).** Modelos progresivos para 2000, 2010 y 2019 que estudian la asociación entre el porcentaje de población de 65 años o más y variables demográficas, económicas y de financiamiento sanitario. Incluyen diagnósticos, errores estándar robustos HC1 y análisis de sensibilidad al tratamiento de faltantes mediante imputación múltiple (MICE).
4. **Perfiles demográficos en cortes temporales (`07`).** Análisis de componentes principales (PCA) y clustering para 1980, 1995, 2010 y 2024, utilizando la proporción de población de 65 años o más, la fecundidad, la esperanza de vida y el crecimiento poblacional. Se comparan las características de los grupos y las trayectorias de los países entre cortes.
5. **Perfiles económicos y sanitarios (`08`).** PCA y clustering para 2023 según cobertura de servicios esenciales de salud, gasto sanitario de bolsillo y PIB per cápita PPA. Se comparan los niveles de envejecimiento entre perfiles, incluidos contrastes no paramétricos.
6. **Dependencia mayor y gasto sanitario (`09`).** Construcción de la tasa de dependencia mayor para 2019, clasificación de países en quintiles y comparación del gasto corriente en salud como porcentaje del PIB mediante descriptivos, ANOVA y pruebas de Tukey.
7. **Trayectorias demográficas continuas (`10`).** Clustering conjunto de observaciones país-año de 1961–2024, luego de diagnosticar y tratar faltantes. Permite estudiar cambios de perfil, transiciones y recorridos no lineales. También incorpora comparaciones con perfiles estimados para 1961 y 1970.
8. **Gráficos exploratorios y comunicativos (`11`).** Representación de perfiles demográficos en un espacio PCA común, mapas comparativos de 1980 y 2024 y visualizaciones de resultados de regresiones.
9. **Pirámide poblacional animada (`12`).** Reconstrucción de la estructura por edad y sexo entre 1960 y 2024, agregando cantidades estimadas de personas a partir de los porcentajes por sexo y grupo etario. Para garantizar comparabilidad temporal, la animación usa un conjunto fijo de países con información completa en todos los años.
10. **Descriptivas antes y después del tratamiento (`13`).** Comparación de la cantidad de observaciones, la distribución y otras estadísticas antes y después de las decisiones sobre faltantes y valores atípicos en las bases analíticas utilizadas.

## Estructura del repositorio

```text
proyecto/
├── README.md
├── raw/
│   └── wdi_raw.csv                               # Descarga original de WDI
├── input/
│   └── wdi_limpia.csv                            # Base limpia para los análisis
├── auxiliar/                                     # Bases intermedias reutilizables
├── output/
│   ├── tablas/                                   # Tablas, diagnósticos y bases derivadas
│   └── graficos/                                 # Gráficos y animaciones exportados
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
│   ├── 11_graficos_exploratorio_comunicativo.R
│   ├── 12_piramide_poblacional_mundial.R
│   └── 13_descriptivas_post_limpieza.R
└── utils/                                        # Funciones auxiliares propias
```

Las funciones de `utils/` forman parte del código necesario para reproducir los análisis. `raw/` conserva los datos originales. `input/` contiene la base limpia, y `output/` almacena resultados finales. La carpeta auxiliar/ incluye, entre otros archivos generados, la muestra de países para los cortes del clustering demográfico. Los scripts utilizan rutas relativas a la carpeta raíz del proyecto.

## Reproducción del análisis

### Paquetes necesarios

En RStudio, instalar una sola vez los paquetes que falten:

```r
install.packages(c(
  "tidyverse", "WDI", "mice", "naniar", "car", "lmtest", "sandwich",
  "cluster", "FactoMineR", "factoextra", "gt", "webshot2",
  "scales", "patchwork", "ggrepel", "sf", "rnaturalearth",
  "rnaturalearthdata", "ggtext", "zoo", "lattice", "gifski"
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
source("script/12_piramide_poblacional_mundial.R")
source("script/13_descriptivas_post_limpieza.R")
```

**Dependencias entre etapas:** `01` genera `raw/wdi_raw.csv`; `02` genera `input/wdi_limpia.csv`. El script `04` exporta diagnósticos que utiliza `07`; `07` genera archivos que necesitan `10` y `11`, y `06` aporta resultados para `11`. El script `13` necesita archivos producidos por `04`, `07`, `08` y `10`. El script `12` depende de la base limpia y genera su propia tabla y animación.

**Versiones de los datos:** `01` requiere conexión a Internet. Como WDI puede revisar los indicadores, una nueva descarga puede producir resultados diferentes. Para trabajar con la misma versión de los datos ya descargada, conservar `raw/wdi_raw.csv` y comenzar desde `02`, siempre que estén disponibles los archivos de código y las funciones auxiliares correspondientes.

**Resultados:** las tablas se guardan en `output/tablas/` y las visualizaciones en `output/graficos/`.

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
