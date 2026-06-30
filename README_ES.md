<img src="images/oralstats.png" alt="Logo de Oralstats" width="160"/>

[![DOI](https://zenodo.org/badge/408316659.svg)](https://zenodo.org/badge/latestdoi/408316659)

# Oralstats v1.8 — LASP (Laboratorio de Análisis de la Señal Prosódica)

**🌐 Idioma / Language:** [🇬🇧 English](README.md) · 🇪🇸 **Español**

Oralstats es una herramienta de exploración de datos para habla transcrita: une las transcripciones con datos de tono e intensidad y permite **visualizar, modelar y explorar la prosodia** en el nivel del discurso. La versión **1.8** es una modernización completa que mantiene el espíritu exploratorio original de SQL/Shiny, pero reconstruye la interfaz con `bslib` y delega el procesamiento pesado de señal y lenguaje en un **pipeline de Python** (extracción con Praat/Parselmouth, UDPipe, `pysentimiento` y emotion2vec+). Todo se organiza en torno a una única barra de navegación superior, con un resumen del corpus (archivos, hablantes, grupos entonativos, grupos fónicos, palabras, vocales) siempre a un clic.

Desarrollada por Adrián Cabedo Nebot ([adrian.cabedo@uv.es](mailto:adrian.cabedo@uv.es)), profesor titular en la Universitat de València (España).

> 📦 **¿Buscas versiones anteriores (≤ v1.3)?** Los módulos clásicos `oralstats.view` / `oralstats.creation`, el vídeo de introducción, las demos antiguas y los *mods* del proyecto están ahora en **[LEGACY.md](LEGACY.md)**.

![Página de inicio de Oralstats v1.8 (LASP)](images/last_version/1_pantalla_inicial.png)

# Instalación

Hay dos maneras de poner en marcha Oralstats v1.8. La **Opción A** lo instala como paquete de R en una sola línea y es la forma más fácil de probarlo. La **Opción B** ejecuta la app modernizada desde el código fuente con un lanzador que se autoinstala.

## Opción A — Instalar como paquete de R (recomendada)

Instala directamente desde GitHub; los paquetes de R necesarios se instalan solos:

```r
# install.packages("remotes")   # si aún no lo tienes
remotes::install_github("acabedo/oralstats", subdir = "package")

# Lanzar la aplicación
oralstats::run_app()
```

No hace falta clonar el repositorio ni fijar un directorio de trabajo. Tus datos —análisis guardados, informes generados, copias de seguridad y el audio que cargues— se guardan en una carpeta estándar por usuario (`tools::R_user_dir("oralstats", "data")`), de modo que nunca se escribe nada dentro del paquete instalado.

**Pipeline de Python opcional.** La extracción acústica (Parselmouth) y el etiquetado de sentimiento/emoción funcionan con Python y son **opcionales**: el resto de Oralstats (resumen del corpus, navegación entonativa, tablas, análisis léxico e informes sin emoción acústica) funciona sin ellos. Instala el entorno una sola vez, por niveles, desde R o desde la pestaña **«Dependencias Python»** de la app:

```r
oralstats::install_oralstats_python("core")   # praat-parselmouth, tgt (ligero)
oralstats::install_oralstats_python("text")   # + sentimiento/emoción (descarga grande)
oralstats::install_oralstats_python("asr")    # + transcripción / diarización
```

Si no se encuentra un Python adecuado (3.10–3.12), `reticulate` instala uno automáticamente. El paquete no descarga nada de Python hasta que se lo pides.

## Opción B — Ejecutar desde el código fuente (lanzador autoinstalable)

La versión modernizada vive también en [`last_version/`](last_version/) y está diseñada para instalarse sola en el primer arranque. Solo necesitas **R** y un editor (**Positron** o **RStudio**); el lanzador `run.R` se encarga de los paquetes de R y de todo el entorno de Python — **tú nunca tocas `pip` ni `conda`**.

1. Instala **R** (<https://cran.r-project.org>) y **Positron** (<https://positron.posit.co>) o RStudio. Instaladores normales de doble clic.
2. En tu editor, abre la carpeta **`last_version/`** como proyecto (`File > Open Folder…`). Esa carpeta contiene `run.R` y `app.R`.
3. Abre **`run.R`** y pulsa **Source** (RStudio/Positron), o desde una terminal ejecuta:
   ```bash
   Rscript run.R
   ```
4. **Espera.** El primer arranque descarga e instala todo automáticamente (paquetes de R, un entorno virtual de Python y las dependencias acústicas básicas). Puede tardar varios minutos; después la app se abre en el navegador. Los arranques siguientes son rápidos.

A diferencia de la Opción A, `run.R` también **instala el nivel «core» de Python automáticamente** en el primer arranque. Necesitas conexión a Internet la primera vez. No hace falta ninguna configuración manual de Python/conda.

### Qué hace `run.R`

`run.R` es un lanzador único que reproduce el entorno y arranca la app:

1. **Asegura los paquetes de R** (vía `R/ensure_r_packages.R`) — instala desde CRAN, como binarios, los que falten.
2. **Prepara un entorno de Python** (vía `R/portability.R` + `R/setup_python.R`): crea un virtualenv de arquitectura nativa `oralstats-env` (Python 3.10–3.12, gestionado con `reticulate`) e instala el nivel **core**. Si no encuentra un Python adecuado, `reticulate` instala uno automáticamente.
3. **Lanza la app Shiny** (`app.R`) en tu navegador.

## Dependencias de Python (se instalan por niveles)

Para que el arranque sea ligero, el pipeline de Python se instala por niveles — tanto `install_oralstats_python()` (Opción A) como `run.R` (Opción B) usan los mismos niveles. Solo el nivel **core** es necesario para la extracción acústica; los más pesados son descargas grandes que instalas **bajo demanda** (desde R o desde la pestaña **«Dependencias Python»** de la app):

| Nivel | Paquetes | Para qué sirve |
|---|---|---|
| **core** | `praat-parselmouth`, `tgt` | extracción de tono/intensidad/texto |
| **text** | `pysentimiento`, `funasr`, `soundfile` | sentimiento textual + emoción acústica (emotion2vec+) |
| **asr** | `whisperx`, `pyannote.audio` | transcripción / diarización |

Variables de entorno útiles (opcionales, sobre todo para la Opción B):

- `ORALSTATS_PY_LEVEL` — nivel de arranque para `run.R` (por defecto `core`; p. ej. pon `text` para instalar también el nivel de sentimiento/emoción al arrancar).
- `ORALSTATS_PYTHON` — ruta a un intérprete de Python 3.10–3.12 concreto que usar en lugar del autodetectado.

## Requisitos

- **R** ≥ 4.1 para la Opción A (el paquete); R ≥ 3.6 para la Opción B. Se recomienda una versión actual, más un navegador moderno (Chrome/Safari).
- **Positron** o **RStudio** 1.4.1717 o posterior.
- **Python 3.10–3.12**, arquitectura nativa — solo para el pipeline opcional; se instala solo si no está presente.
- Acceso a Internet en el primer arranque.

**Paquetes de R (v1.8)** — instalados automáticamente por cualquiera de las dos opciones:

> av; bslib; data.table; dplyr; DT; ggeffects; ggfun; ggplot2; jsonlite; mgcv; plotly; RColorBrewer; reticulate; seewave; shiny; shinyjs; tidyr; tuneR; udpipe

**Nota:** UDPipe necesita un archivo de modelo de lengua; por defecto se incluye uno de español, pero puedes usar cualquier otro que cubra tus necesidades.

# Qué puedes hacer

La app se organiza en torno a una única barra de navegación superior. Las capacidades principales son:

## Cargar y resumir un corpus
Abre corpus ya procesados desde el almacenamiento local o procesa material nuevo desde cero: los pares TextGrid + WAV se convierten en los archivos tabulares de texto/tono/intensidad que la app espera, usando los scripts de Praat y Parselmouth incluidos. La vista **Resumen** condensa todo el corpus en tarjetas KPI y un desglose por género discursivo, archivo o hablante.

![Resumen del corpus con tarjetas KPI y desglose por género/archivo/hablante](images/last_version/2_resumen.png)

## Navegar y visualizar la entonación
Recorre los grupos entonativos (GE) uno a uno, escúchalos y lee sus descriptores automáticos —entre ellos la etiqueta AMH (*análisis melódico del habla*) y la etiqueta ToBI nuclear. Cada grupo puede representarse como valores normalizados, Hz crudos, semitonos (ggplot2) o como una figura al estilo Praatpicture, y la proyección se puede personalizar (tono, intensidad, transición vocálica, picos tonales, marcas tónicas, etiquetas ToBI, secciones internas A/C/T…).

![Navegador de grupos entonativos con descriptores AMH y ToBI](images/last_version/3_navegacion.png)

![Configuración melódica normalizada de un grupo](images/last_version/4_visualizador_curvas.png)

La vista de **evolución temporal** sigue cualquier variable numérica (p. ej. la intensidad media) a lo largo de la línea temporal, archivo por archivo y hablante por hablante.

![Evolución temporal de una variable prosódica por hablante](images/last_version/5_grafico_visualizacion.png)

Un módulo específico detecta **cláusulas entonativas** siguiendo el *Corolario de Hidalgo Navarro* (diagnósticos de recursividad, jerarquía y declinación) y representa la trayectoria de F0 de cada cláusula detectada.

![Detección de cláusulas entonativas (Corolario de Hidalgo Navarro)](images/last_version/6_clausulas.png)

## Explorar las tablas de datos
Todos los grupos entonativos y fónicos están disponibles como tablas ordenables, con búsqueda y filtro por columna (archivo, hablante, grupo fónico, grupo entonativo, inicio/fin, duración, número de vocales y palabras, velocidad de habla, valores de F0…), listas para inspeccionar o exportar.

![Tabla ordenable y filtrable de grupos entonativos](images/last_version/7_tablas_unidades_grupos_entonativos.png)

## Analizar el léxico
El menú **Léxico** ofrece análisis de frecuencia de palabras y diversidad léxica (type/token ratio, forma de superficie o lema de UDPipe, con eliminación opcional de palabras vacías), bigramas y trigramas con *N* configurable, y nubes de palabras.

![Frecuencia de palabras y diversidad léxica (TTR)](images/last_version/8_word_frequency.png)

![Bigramas y trigramas](images/last_version/9_bigramas.png)

![Nube de palabras](images/last_version/10_wordcloud.png)

## Etiquetar sentimiento y emoción
Combina el **sentimiento textual** (`pysentimiento`) con las **emociones acústicas** (emotion2vec+) para obtener distribuciones de sentimiento y emoción y su relación. El mismo módulo permite etiquetar manualmente cada grupo entonativo con una emoción percibida (emojis de Ekman) mientras observas su curva melódica.

![Análisis de sentimiento y emoción (pysentimiento + emotion2vec+)](images/last_version/11_sentiments_barcharts.png)

![Etiquetado emocional manual de grupos entonativos](images/last_version/12_sentiments_tagging.png)

Para evaluar la fiabilidad, el módulo de **validación con jueces** construye tareas de percepción en las que uno o más jueces escuchan muestras equilibradas de grupos entonativos y valoran la emoción percibida y su intensidad.

![Módulo de validación con jueces](images/last_version/13_jueces.png)

## Generar informes prosódicos
Un **generador de informes** guiado convierte el corpus en un informe prosódico completo: elige el conjunto de datos y los filtros (solo elementos validados, eliminación de valores atípicos por percentiles/puntuaciones Z, agrupación), selecciona las variables numéricas y elige las secciones a incluir —estadística descriptiva, diagramas de caja con pruebas de significación, mapas de calor de correlación, ANOVA/Kruskal-Wallis, regresión lineal, random forest, árboles de decisión, GAMM (modelos aditivos mixtos generalizados), ejemplos extremos, curvas melódicas por patrón, cláusulas de Hidalgo Navarro y más— antes de previsualizarlo y exportarlo.

![Generador de informes — conjunto de datos y filtros](images/last_version/14_generador_informes_1.png)

![Generador de informes — secciones del informe (incl. GAMM)](images/last_version/15_generador_informes2.png)

> **Los archivos de la versión modernizada** están en [`last_version/`](last_version/): la app Shiny (`app.R`), el lanzador (`run.R`), los extractores de Praat/Parselmouth (`script_PRAAT_extraer_pitch_intensity_transcriptions.praat`, `python/extract_with_parselmouth.py`) y el analizador de sentimiento/emoción (`python/analyze_sentiment_emotion.py`). El paquete instalable (Opción A) está en [`package/`](package/).

# Cómo citar

Puedes usar las siguientes citas para referirte a esta herramienta:

- Cabedo, A. (2021). Oralstats. A tool to visualize and explore transcriptions and phonic data. Version beta 1.0 <https://github.com/acabedo/oralstats>
- Cabedo, A. (2022). Oralstats. A tool to visualize and explore transcriptions and phonic data. Version beta 1.3 <https://github.com/acabedo/oralstats>
- Cabedo Nebot, A. (2022). Using Oralstats for prosodic characterisation of speakers in different discourse genres. Loquens, 8(1-2), e079. <https://doi.org/10.3989/loquens.2021.079>
- Cabedo Nebot, A. (2022). Visualizing melody with multiple acoustic and tagging values using the visualization module of the Oralstats tool. Estudios de Fonética Experimental, XXXI, 135-148. <https://www.ub.edu/journalofexperimentalphonetics/pdf-articles/XXXI-10-Cabedo.pdf>

# Documentación

La sección de documentación **está todavía en desarrollo**. Por ahora, si tienes algún problema o duda sobre Oralstats, puedes escribirme un correo ([adrian.cabedo@uv.es](mailto:adrian.cabedo@uv.es)).

# Financiación

Esta nueva versión de Oralstats (v1.8) ha sido financiada por el proyecto **PID2023-148371NB-C42** del **Ministerio de Ciencia, Innovación y Universidades** (España).

> Los proyectos financiados anteriores que apoyaron versiones previas de Oralstats están listados en [LEGACY.md](LEGACY.md#funding-earlier-versions).

# Créditos

Debo un crédito especial a *Radiant, Business analytics using R and Shiny* (Vnijs 2016). Aunque se centra sobre todo en la minería de datos de negocio, esta herramienta fue para mí una idea profundamente inspiradora: en mi caso, desarrolló la idea de analizar datos en línea/sin conexión, con Shiny, de forma dinámica, añadiendo y/o filtrando datos para observar patrones lingüísticos a partir de transcripciones de habla.

Otra inspiración para mí fue todo el trabajo de Davies durante los últimos veinte años (2005, 2016, 2020, entre muchos otros) en la construcción de varios corpus lingüísticos; en especial, me interesó mucho la idea de unir datos con instancias de SQL. Es realmente un buen enfoque para datos estructurados, como pueden ser los procedentes de datos lingüísticos orales, en los que los fonemas se relacionan con las palabras y estas con los enunciados, y todos ellos con los hablantes, y así sucesivamente.

Por último, sobre las ideas que han sido clave para desarrollar Oralstats, quiero expresar mi gratitud a plataformas web como Spokes (<http://pelcra.clarin-pl.eu/SpokesBNC/>) y a herramientas de software como las desarrolladas por Laurence Anthony (<https://www.laurenceanthony.net/>). Además, no habría sido posible hacer el análisis de habla y fónico sin la ayuda de herramientas como ELAN (<https://archive.mpi.nl/tla/elan>) y PRAAT (Boersma y Weenink, 2021, <https://www.fon.hum.uva.nl/praat/>).

# Advertencia

Todos los errores y omisiones (operaciones estadísticas incorrectas [medias, medianas…], visualizaciones incorrectas, etc.) son responsabilidad exclusiva del autor.

# Licencia

GNU General Public License v3.0. Los permisos de esta licencia copyleft fuerte están condicionados a poner a disposición el código fuente completo de las obras licenciadas y sus modificaciones, lo que incluye obras mayores que usen una obra licenciada, bajo la misma licencia. Deben conservarse los avisos de copyright y de licencia. Los colaboradores otorgan una concesión expresa de derechos de patente.
