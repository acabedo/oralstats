# Bootstrap del entorno Python del proyecto para OralStats — virtualenv + pip.
#
# Crea un virtualenv 'oralstats-env' sobre un Python de ARQUITECTURA NATIVA
# (3.10-3.12; evita Rosetta en Apple Silicon) e instala por niveles con pip:
#   - core: praat-parselmouth, tgt   (al arrancar, vía run.R)
#   - text: pysentimiento, funasr, soundfile   (solo al pulsar el botón nivel 2)
#   - asr : whisperx, pyannote.audio           (solo al pulsar el botón nivel 3)
# En arm64 nativo (Apple Silicon), Windows y Linux hay wheels para
# torch/numba/spacy/etc. -> no se compila nada. Si no hay Python en la máquina,
# reticulate instala uno (binario). Todo se gestiona con reticulate; el usuario
# NO usa pip en la terminal.
#
# Como script:  Rscript R/setup_python.R [core|text|asr|all]

if (!exists("ORALSTATS_VENV")) source("R/portability.R")

# ── Selección de Python (versión 3.9-3.12 y arquitectura nativa) ──────────────
.norm_arch <- function(a) {
  a <- tolower(a)
  if (grepl("arm|aarch", a)) "arm" else if (grepl("x86|amd|x64|i386", a)) "x86" else a
}
.oralstats_py_version <- function(py) {
  v <- tryCatch(
    system2(py, c("-c", shQuote("import sys; print(sys.version_info[0], sys.version_info[1])")),
            stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  if (length(v) != 1) return(NULL)
  parts <- strsplit(trimws(v), "\\s+")[[1]]
  if (length(parts) != 2 || !all(grepl("^[0-9]+$", parts))) return(NULL)
  as.integer(parts)
}
.oralstats_py_compatible <- function(v) {
  if (is.null(v)) return(FALSE)
  (v[1] > 3 || (v[1] == 3 && v[2] >= 9)) && (v[1] < 3 || (v[1] == 3 && v[2] <= 12))
}
.oralstats_py_arch <- function(py) {
  a <- tryCatch(
    system2(py, c("-c", shQuote("import platform; print(platform.machine())")),
            stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  if (length(a) == 1) .norm_arch(trimws(a)) else ""
}
oralstats_choose_python <- function() {
  host <- .norm_arch(Sys.info()[["machine"]])
  ov <- Sys.getenv("ORALSTATS_PYTHON", "")
  if (nzchar(ov) && file.exists(ov) && .oralstats_py_compatible(.oralstats_py_version(ov))) return(ov)
  cand  <- c("python3.12", "python3.11", "python3.10", "python3.9", "python3", "python")
  paths <- unique(Filter(nzchar, vapply(cand, function(n) unname(Sys.which(n)), character(1))))
  compat <- Filter(function(p) .oralstats_py_compatible(.oralstats_py_version(p)), paths)
  native <- Filter(function(p) identical(.oralstats_py_arch(p), host), compat)
  if (length(native)) return(native[[1]])
  if (length(compat)) return(compat[[1]])
  NA_character_
}

# ── Paquetes pip por nivel ────────────────────────────────────────────────────
.oralstats_pip <- list(
  core = c("praat-parselmouth", "tgt"),
  text = c("pysentimiento", "funasr", "soundfile"),
  asr  = c("whisperx", "pyannote.audio")
)

oralstats_bootstrap <- function(level = "core") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Falta el paquete R 'reticulate' (lo instala ensure_r_packages.R).")
  }

  # Crear el virtualenv si no existe.
  if (!isTRUE(tryCatch(reticulate::virtualenv_exists(ORALSTATS_VENV), error = function(e) FALSE))) {
    py <- oralstats_choose_python()
    if (is.na(py)) {
      message("No hay Python 3.10-3.12 en el sistema. Instalando uno con reticulate (binario)…")
      py <- tryCatch(reticulate::install_python(version = "3.11:latest"),
                     error = function(e) { message("Aviso: install_python falló: ", conditionMessage(e)); NA_character_ })
    }
    if (is.na(py)) {
      warning("No se pudo preparar Python. Instala Python 3.11 (python.org) o miniforge y reintenta.")
      return(invisible(NA_character_))
    }
    message("Creando entorno virtual '", ORALSTATS_VENV, "' (Python: ", py, ")…")
    reticulate::virtualenv_create(ORALSTATS_VENV, python = py)
  }

  pybin <- reticulate::virtualenv_python(ORALSTATS_VENV)

  niveles <- switch(level,
    core = "core",
    text = c("core", "text"),
    asr  = c("core", "text", "asr"),
    all  = c("core", "text", "asr"),
    stop("Nivel desconocido: ", level)
  )

  system2(pybin, c("-m", "pip", "install", "--upgrade", "pip"))
  for (lv in niveles) {
    pp <- .oralstats_pip[[lv]]
    message("pip [", lv, "]: ", paste(pp, collapse = ", "))
    st <- tryCatch(system2(pybin, c("-m", "pip", "install", pp)),
                   error = function(e) 1L)
    if (!identical(st, 0L)) warning("Algún paquete del nivel '", lv, "' no se instaló.")
  }
  invisible(pybin)
}

# Tail ejecutable: solo si se invoca como `Rscript R/setup_python.R <nivel>`.
local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 1 && a[[1]] %in% c("core", "text", "asr", "all")) {
    oralstats_bootstrap(a[[1]])
    cat("\nORALSTATS_BOOTSTRAP_DONE\n")   # marca de fin para el modal de progreso de la app
  }
})
