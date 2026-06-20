# Bootstrap del entorno Python del proyecto para OralStats — VÍA CONDA.
#
# Crea (si no existe) un entorno conda 'oralstats-env' e instala POR NIVELES,
# usando binarios de conda-forge para las piezas difíciles (torch, spacy, numba,
# llvmlite, librosa, ffmpeg) y pip solo para los wrappers (pysentimiento, funasr,
# whisperx, pyannote.audio). Así NO se compila nada y funciona en Mac (arm64 e
# Intel), Windows y Linux. conda usa la arquitectura nativa automáticamente.
#
# Requiere conda/mamba (p. ej. miniforge).
# Como script:  Rscript setup_python.R [core|text|asr|all]
# Sourceado desde run.R:  oralstats_bootstrap("core")

if (!exists("ORALSTATS_VENV")) source("R/portability.R")

ORALSTATS_PY_VERSION <- "3.11"

# Paquetes por nivel. Acumulativos (text incluye core; asr incluye core+text).
.oralstats_conda <- list(
  core = character(0),                                                  # parselmouth/tgt -> pip
  text = c("pytorch", "spacy", "numba", "llvmlite", "librosa", "ffmpeg"),  # librosa arrastra soundfile
  asr  = character(0)
)
.oralstats_pip <- list(
  core = c("praat-parselmouth", "tgt"),    # wheels fiables en PyPI (no están en conda-forge)
  text = c("pysentimiento", "funasr"),
  asr  = c("whisperx", "pyannote.audio")
)

oralstats_bootstrap <- function(level = "core") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Falta el paquete R 'reticulate' (lo instala ensure_r_packages.R / run.R).")
  }

  conda <- tryCatch(reticulate::conda_binary(), error = function(e) NA_character_)
  if (is.na(conda)) {
    # Sin conda: instalar Miniconda automáticamente (el usuario NO tiene que
    # instalar nada a mano). Es una descarga única de ~1-2 min.
    message("No se encontró conda. Instalando Miniconda automáticamente (una sola vez)…")
    ok <- tryCatch({ reticulate::install_miniconda(update = FALSE); TRUE },
                   error = function(e) {
                     message("Aviso: no se pudo instalar Miniconda: ", conditionMessage(e)); FALSE
                   })
    conda <- tryCatch(reticulate::conda_binary(), error = function(e) NA_character_)
    if (!ok || is.na(conda)) {
      warning("No se pudo preparar conda automáticamente. Instala miniforge a mano ",
              "(https://github.com/conda-forge/miniforge) y vuelve a ejecutar.")
      return(invisible(NA_character_))
    }
  }

  # Crear el entorno si no existe (Python de arquitectura nativa, vía conda).
  envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
  if (!(ORALSTATS_VENV %in% envs)) {
    message("Creando entorno conda '", ORALSTATS_VENV, "' (Python ", ORALSTATS_PY_VERSION, ")…")
    reticulate::conda_create(ORALSTATS_VENV, python_version = ORALSTATS_PY_VERSION)
  }

  niveles <- switch(level,
    core = "core",
    text = c("core", "text"),
    asr  = c("core", "text", "asr"),
    all  = c("core", "text", "asr"),
    stop("Nivel desconocido: ", level)
  )

  for (lv in niveles) {
    cp <- .oralstats_conda[[lv]]
    if (length(cp)) {
      message("conda-forge [", lv, "]: ", paste(cp, collapse = ", "))
      tryCatch(
        reticulate::conda_install(ORALSTATS_VENV, packages = cp, channel = "conda-forge"),
        error = function(e) warning("Fallo instalando (conda) nivel ", lv, ": ", conditionMessage(e))
      )
    }
    pp <- .oralstats_pip[[lv]]
    if (length(pp)) {
      message("pip [", lv, "]: ", paste(pp, collapse = ", "))
      tryCatch(
        reticulate::conda_install(ORALSTATS_VENV, packages = pp, pip = TRUE),
        error = function(e) warning("Fallo instalando (pip) nivel ", lv, ": ", conditionMessage(e))
      )
    }
  }

  invisible(tryCatch(reticulate::conda_python(ORALSTATS_VENV), error = function(e) NA_character_))
}

# Tail ejecutable: solo actúa si se invoca como `Rscript setup_python.R <nivel>`.
local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 1 && a[[1]] %in% c("core", "text", "asr", "all")) {
    oralstats_bootstrap(a[[1]])
  }
})
