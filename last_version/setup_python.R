# Bootstrap del entorno Python del proyecto para OralStats.
# Crea (si no existe) el virtualenv del proyecto e instala los niveles indicados.
# Como script:  Rscript setup_python.R [core|text|asr|all]
# Sourceado desde run.R:  oralstats_bootstrap("text")

if (!exists("ORALSTATS_VENV")) source("R/portability.R")

# ── Selección de un Python compatible para CREAR el venv del proyecto ──────────
# La pila ML (torch, tokenizers, whisperx) solo tiene wheels fiables hasta 3.12;
# en 3.13+/3.14 pip intenta compilar desde fuente y falla sin toolchains (Rust…).
.ORALSTATS_PY_MIN <- c(3L, 9L)
.ORALSTATS_PY_MAX <- c(3L, 12L)

# (major, minor) de un intérprete, o NULL si no se puede determinar.
.oralstats_py_version <- function(py) {
  # print(a, b) -> "3 12"; sin comillas en el snippet para evitar líos de quoting
  # en el shell (system2 no entrecomilla; usamos shQuote como en el resto del código).
  v <- tryCatch(
    system2(py, c("-c", shQuote("import sys; print(sys.version_info[0], sys.version_info[1])")),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (length(v) != 1) return(NULL)
  parts <- strsplit(trimws(v), "\\s+")[[1]]
  if (length(parts) != 2 || !all(grepl("^[0-9]+$", parts))) return(NULL)
  as.integer(parts)
}

.oralstats_py_compatible <- function(ver) {
  if (is.null(ver)) return(FALSE)
  ge <- ver[1] > .ORALSTATS_PY_MIN[1] ||
        (ver[1] == .ORALSTATS_PY_MIN[1] && ver[2] >= .ORALSTATS_PY_MIN[2])
  le <- ver[1] < .ORALSTATS_PY_MAX[1] ||
        (ver[1] == .ORALSTATS_PY_MAX[1] && ver[2] <= .ORALSTATS_PY_MAX[2])
  ge && le
}

# Arquitectura de un intérprete, normalizada a "arm" / "x86".
.norm_arch <- function(a) {
  a <- tolower(a)
  if (grepl("arm|aarch", a)) "arm" else if (grepl("x86|amd|x64|i386", a)) "x86" else a
}
.oralstats_py_arch <- function(py) {
  a <- tryCatch(
    system2(py, c("-c", shQuote("import platform; print(platform.machine())")),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (length(a) == 1) .norm_arch(trimws(a)) else ""
}

# Elige un intérprete Python 3.9-3.12 sobre el que basar el venv del proyecto.
# CLAVE: prefiere uno cuya ARQUITECTURA coincida con la del sistema. En Apple
# Silicon (arm64) esto evita coger un Python x86_64 bajo Rosetta, que rompe los
# wheels de numba/torch/whisperx. Prioridad: $ORALSTATS_PYTHON ->
# python3.12/3.11/3.10/3.9/python3/python, primero los de arquitectura nativa.
oralstats_choose_python <- function() {
  host_arch <- .norm_arch(Sys.info()[["machine"]])

  override <- Sys.getenv("ORALSTATS_PYTHON", "")
  if (nzchar(override)) {
    if (file.exists(override) && .oralstats_py_compatible(.oralstats_py_version(override))) {
      return(override)
    }
    warning("ORALSTATS_PYTHON='", override, "' no existe o no es Python 3.9-3.12; se ignora.")
  }

  cand <- c("python3.12", "python3.11", "python3.10", "python3.9", "python3", "python")
  paths <- unique(Filter(nzchar, vapply(cand, function(n) unname(Sys.which(n)), character(1))))
  compat <- Filter(function(p) .oralstats_py_compatible(.oralstats_py_version(p)), paths)
  if (length(compat) == 0) return(NA_character_)

  # Preferir arquitectura nativa (evita Rosetta en Apple Silicon).
  native <- Filter(function(p) identical(.oralstats_py_arch(p), host_arch), compat)
  if (length(native) > 0) return(native[[1]])

  warning("No se encontró un Python compatible de arquitectura '", host_arch,
          "'; usando ", compat[[1]], " (podrían faltar wheels de numba/torch).")
  compat[[1]]
}

oralstats_bootstrap <- function(level = "text") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Falta el paquete R 'reticulate' (ejecuta renv::restore()).")
  }
  if (!reticulate::virtualenv_exists(ORALSTATS_VENV)) {
    py_base <- oralstats_choose_python()
    if (is.na(py_base)) {
      warning("No se encontró Python 3.9-3.12. Los niveles 2/3 (torch/whisperx) ",
              "pueden fallar al no haber wheels para Python muy reciente (3.13+/3.14). ",
              "Instala Python 3.12 (o define ORALSTATS_PYTHON=/ruta/a/python3.12). ",
              "Creando el venv con el Python por defecto de reticulate…")
      reticulate::virtualenv_create(ORALSTATS_VENV)
    } else {
      message("Creando virtualenv del proyecto: ", ORALSTATS_VENV, " (Python: ", py_base, ")")
      reticulate::virtualenv_create(ORALSTATS_VENV, python = py_base)
    }
  }
  py <- reticulate::virtualenv_python(ORALSTATS_VENV)

  reqs <- switch(level,
    core = "requirements-core.txt",
    text = c("requirements-core.txt", "requirements-text.txt"),
    asr  = c("requirements-core.txt", "requirements-text.txt", "requirements-asr.txt"),
    all  = c("requirements-core.txt", "requirements-text.txt", "requirements-asr.txt"),
    stop("Nivel desconocido: ", level)
  )

  system2(py, c("-m", "pip", "install", "--upgrade", "pip"))
  for (r in reqs) {
    if (!file.exists(r)) {
      stop("No se encuentra ", r, " (¿ejecutas desde la carpeta Oralstats/?)")
    }
    message("Instalando ", r, " …")
    status <- system2(py, c("-m", "pip", "install", "-r", r))
    if (!identical(status, 0L)) {
      warning("Fallo instalando ", r, " (status ", status, ").")
    }
  }
  invisible(py)
}

# Tail ejecutable: solo actúa si se invoca como `Rscript setup_python.R <nivel>`.
local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 1 && a[[1]] %in% c("core", "text", "asr", "all")) {
    oralstats_bootstrap(a[[1]])
  }
})
