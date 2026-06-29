# Lógica del entorno Python opcional de oralstats.
#
# Constante: ORALSTATS_VENV
# Funciones públicas: oralstats_python_available(), install_oralstats_python()
# Funciones internas: oralstats_python(), oralstats_choose_python(),
#   oralstats_bootstrap(), y los helpers dotados (.norm_arch, etc.)

# Nombre del virtualenv propio del proyecto (gestionado por reticulate).
ORALSTATS_VENV <- "oralstats-env"

#' @keywords internal
#' @noRd
oralstats_python <- function() {
  if (requireNamespace("reticulate", quietly = TRUE)) {
    # (1) virtualenv del proyecto (creado por install_oralstats_python() con pip)
    if (isTRUE(tryCatch(reticulate::virtualenv_exists(ORALSTATS_VENV),
                        error = function(e) FALSE))) {
      proj <- tryCatch(reticulate::virtualenv_python(ORALSTATS_VENV),
                       error = function(e) NA_character_)
      if (!is.na(proj) && nzchar(proj) && file.exists(proj)) return(proj)
    }
    # (1b) entorno conda con el mismo nombre (compatibilidad con instalaciones previas)
    envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character(0))
    if (ORALSTATS_VENV %in% envs) {
      pc <- tryCatch(reticulate::conda_python(ORALSTATS_VENV), error = function(e) NA_character_)
      if (!is.na(pc) && nzchar(pc) && file.exists(pc)) return(pc)
    }
  }

  # (2) bert-env heredado
  home <- Sys.getenv("HOME"); if (!nzchar(home)) home <- Sys.getenv("USERPROFILE")
  legacy <- if (.Platform$OS.type == "windows") {
    file.path(home, ".virtualenvs", "bert-env", "Scripts", "python.exe")
  } else {
    file.path(home, ".virtualenvs", "bert-env", "bin", "python3")
  }
  if (file.exists(legacy)) return(legacy)

  # (3) PATH
  for (cand in c(Sys.which("python3"), Sys.which("python"))) {
    if (nzchar(cand) && file.exists(cand)) return(unname(cand))
  }
  NA_character_
}

# ── Selección de Python (versión 3.10-3.12 y arquitectura nativa) ──────────────

#' @keywords internal
#' @noRd
.norm_arch <- function(a) {
  a <- tolower(a)
  if (grepl("arm|aarch", a)) "arm" else if (grepl("x86|amd|x64|i386", a)) "x86" else a
}

#' @keywords internal
#' @noRd
.oralstats_py_version <- function(py) {
  v <- tryCatch(
    system2(py, c("-c", shQuote("import sys; print(sys.version_info[0], sys.version_info[1])")),
            stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  if (length(v) != 1) return(NULL)
  parts <- strsplit(trimws(v), "\\s+")[[1]]
  if (length(parts) != 2 || !all(grepl("^[0-9]+$", parts))) return(NULL)
  as.integer(parts)
}

#' @keywords internal
#' @noRd
.oralstats_py_compatible <- function(v) {
  # Mínimo 3.10: spacy/thinc (vía pysentimiento) ya NO soportan Python 3.9.
  if (is.null(v)) return(FALSE)
  (v[1] > 3 || (v[1] == 3 && v[2] >= 10)) && (v[1] < 3 || (v[1] == 3 && v[2] <= 12))
}

#' @keywords internal
#' @noRd
.oralstats_py_arch <- function(py) {
  a <- tryCatch(
    system2(py, c("-c", shQuote("import platform; print(platform.machine())")),
            stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  if (length(a) == 1) .norm_arch(trimws(a)) else ""
}

#' @keywords internal
#' @noRd
oralstats_choose_python <- function() {
  host <- .norm_arch(Sys.info()[["machine"]])
  ov <- Sys.getenv("ORALSTATS_PYTHON", "")
  if (nzchar(ov) && file.exists(ov) && .oralstats_py_compatible(.oralstats_py_version(ov))) return(ov)
  cand  <- c("python3.12", "python3.11", "python3.10", "python3", "python")
  paths <- unique(Filter(nzchar, vapply(cand, function(n) unname(Sys.which(n)), character(1))))
  compat <- Filter(function(p) .oralstats_py_compatible(.oralstats_py_version(p)), paths)
  native <- Filter(function(p) identical(.oralstats_py_arch(p), host), compat)
  if (length(native)) return(native[[1]])
  if (length(compat)) return(compat[[1]])
  NA_character_
}

# ── Paquetes pip por nivel ────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.oralstats_pip <- list(
  core = c("praat-parselmouth", "tgt"),
  text = c("pysentimiento", "funasr", "soundfile"),
  asr  = c("whisperx", "pyannote.audio")
)

# Nombre del módulo de import (Python) para cada paquete pip. Se usa para
# verificar, tras instalar, que el paquete realmente se puede importar.
#' @keywords internal
#' @noRd
.oralstats_import <- c(
  "praat-parselmouth" = "parselmouth",
  "tgt"               = "tgt",
  "pysentimiento"     = "pysentimiento",
  "funasr"            = "funasr",
  "soundfile"         = "soundfile",
  "whisperx"          = "whisperx",
  "pyannote.audio"    = "pyannote.audio"
)

#' @keywords internal
#' @noRd
oralstats_bootstrap <- function(level = "core") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Falta el paquete R 'reticulate'. Instálalo con install.packages('reticulate').")
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

  # Instalar paquete a paquete (NO en bloque). Si pip no puede resolver uno
  # —p.ej. conflicto de versiones de torch entre pysentimiento y funasr— los
  # demás se instalan igualmente. Instalar en bloque hacía que un fallo dejara
  # fuera a funasr/soundfile sin avisar.
  pkgs <- unique(unlist(.oralstats_pip[niveles], use.names = FALSE))
  for (pkg in pkgs) {
    message(">>> pip install: ", pkg)
    tryCatch(system2(pybin, c("-m", "pip", "install", "--upgrade", pkg)),
             error = function(e) message("  (excepción al instalar ", pkg, ": ",
                                         conditionMessage(e), ")"))
  }

  # Verificación final: importar cada paquete dentro del propio venv. Detecta
  # tanto fallos de instalación como conflictos que rompen un paquete ya puesto.
  message("\n=== Verificando importaciones en el entorno ===")
  fallos <- character(0)
  for (pkg in pkgs) {
    imp <- unname(.oralstats_import[pkg]); if (is.na(imp)) imp <- pkg
    ok <- identical(
      suppressWarnings(system2(pybin, c("-c", shQuote(paste0("import ", imp))),
                               stdout = FALSE, stderr = FALSE)),
      0L)
    if (ok) message("OK    ", pkg)
    else  { message("FALLO ", pkg, " (no se importa '", imp, "')"); fallos <- c(fallos, pkg) }
  }
  if (length(fallos)) {
    # Marca que la app detecta para avisar (sin mandar al usuario al terminal).
    message("ORALSTATS_PIP_FALLOS: ", paste(fallos, collapse = ", "))
    warning("No se pudieron instalar/importar: ", paste(fallos, collapse = ", "))
  } else {
    message("ORALSTATS_PIP_OK")
  }
  invisible(pybin)
}

# ── Funciones públicas ────────────────────────────────────────────────────────

#' Comprobar si el entorno Python de oralstats está disponible
#'
#' Devuelve TRUE si existe el virtualenv del proyecto y, opcionalmente, si los
#' módulos indicados se pueden importar. Nunca lanza error: si reticulate o el
#' venv no están, devuelve FALSE para permitir degradación elegante en la app.
#' @param modules Vector de nombres de módulos Python a verificar (p. ej.
#'   c("parselmouth")). Si NULL, solo comprueba que el venv exista.
#' @return TRUE/FALSE.
#' @export
oralstats_python_available <- function(modules = NULL) {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(FALSE)
  venv_ok <- isTRUE(tryCatch(reticulate::virtualenv_exists(ORALSTATS_VENV),
                             error = function(e) FALSE))
  if (!venv_ok) return(FALSE)
  if (is.null(modules)) return(TRUE)
  py <- tryCatch(reticulate::virtualenv_python(ORALSTATS_VENV),
                 error = function(e) NA_character_)
  if (is.na(py) || !nzchar(py) || !file.exists(py)) return(FALSE)
  all(vapply(modules, function(m) {
    identical(suppressWarnings(system2(py, c("-c", shQuote(paste0("import ", m))),
                                       stdout = FALSE, stderr = FALSE)), 0L)
  }, logical(1)))
}

#' Instalar el entorno Python de oralstats (opcional)
#'
#' Crea el virtualenv 'oralstats-env' con reticulate e instala las dependencias
#' Python por niveles. Es una operación opcional y potencialmente larga (el
#' nivel 'asr'/'text' descarga varios GB). Llámala una sola vez antes de usar
#' las funciones acústicas/de emoción de la app.
#' @param level Uno de "core", "text", "asr", "all".
#' @return Invisible: ruta al intérprete Python del entorno.
#' @examples
#' \dontrun{
#' oralstats::install_oralstats_python("core")
#' }
#' @export
install_oralstats_python <- function(level = c("core", "text", "asr", "all")) {
  level <- match.arg(level)
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Instala el paquete 'reticulate' para usar el pipeline de Python: ",
         "install.packages('reticulate').")
  }
  oralstats_bootstrap(level)
}
