#!/usr/bin/env Rscript
# Lanzador único de OralStats: reproduce el entorno y arranca la app.
# Uso:  Rscript run.R     (desde la carpeta Oralstats/)

# Trabajar siempre desde el directorio de este script, tanto con `Rscript run.R`
# como con source("run.R") desde RStudio/Positron (botón Source).
.oralstats_script_dir <- function() {
  # 1) Rscript run.R  ->  argumento --file=
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1 && nzchar(f)) return(dirname(normalizePath(f)))
  # 2) source("run.R")  ->  ofile en algún frame de sourcing
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(dirname(normalizePath(of)))
  }
  # 3) RStudio/Positron: ruta del documento activo en el editor
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (length(p) == 1 && nzchar(p)) return(dirname(normalizePath(p)))
  }
  # 4) Fallback: directorio de trabajo actual
  getwd()
}
.dir <- .oralstats_script_dir()
if (length(.dir) == 1 && !is.na(.dir) && dir.exists(.dir)) setwd(.dir)

# Verificar que el directorio es el correcto (mensaje claro si no).
if (!file.exists("app.R") || !file.exists("R/portability.R")) {
  stop("No encuentro 'app.R' / 'R/portability.R' en '", getwd(), "'.\n",
       "Ejecuta run.R desde la carpeta Oralstats/ (o abre esa carpeta como ",
       "proyecto en RStudio/Positron antes de pulsar Source).")
}

# 1) Reproducir paquetes de R con renv (si está inicializado).
#    Instalación NO transaccional: si un paquete OPCIONAL pesado (p. ej. torch,
#    V8, soundgen) no se instala en esta máquina, NO tumba la instalación del
#    núcleo ni el arranque de la app (esa función opcional queda desactivada).
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
  if (requireNamespace("renv", quietly = TRUE)) {
    options(renv.config.install.transactional = FALSE)
    Sys.setenv(RENV_CONFIG_INSTALL_TRANSACTIONAL = "FALSE")
    tryCatch(renv::restore(prompt = FALSE),
             error = function(e) message("Aviso: renv::restore tuvo fallos; la app arrancará con lo instalado: ",
                                         conditionMessage(e)))
  }
}

# 2) Bootstrap del entorno Python del proyecto (core+text por defecto).
source("R/portability.R")
source("setup_python.R")
tryCatch(
  oralstats_bootstrap(level = Sys.getenv("ORALSTATS_PY_LEVEL", "text")),
  error = function(e) message("Aviso: bootstrap de Python falló (la app arrancará igual): ",
                              conditionMessage(e))
)

# 3) Arrancar la app (app.R en este directorio).
shiny::runApp(".", launch.browser = TRUE)
