# Helpers de portabilidad de OralStats.

# Nombre del virtualenv propio del proyecto (gestionado por reticulate).
ORALSTATS_VENV <- "oralstats-env"

# Devuelve la ruta al intérprete Python que debe usar OralStats.
# Prioridad: (1) venv del proyecto (reticulate),
#            (2) ~/.virtualenvs/bert-env  (compatibilidad hacia atrás),
#            (3) python3/python del PATH.
# Devuelve NA_character_ si no hay ninguno.
oralstats_python <- function() {
  # (1) venv del proyecto gestionado por reticulate
  if (requireNamespace("reticulate", quietly = TRUE) &&
      isTRUE(tryCatch(reticulate::virtualenv_exists(ORALSTATS_VENV),
                      error = function(e) FALSE))) {
    proj <- tryCatch(reticulate::virtualenv_python(ORALSTATS_VENV),
                     error = function(e) NA_character_)
    if (!is.na(proj) && nzchar(proj) && file.exists(proj)) return(proj)
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

# Detecta binarios de sistema externos que OralStats puede usar.
# Devuelve list(praat=, ffmpeg=, pandoc=, chrome=), cada uno list(found, path, hint).
check_system_deps <- function() {
  os <- if (.Platform$OS.type == "windows") "windows"
        else if (Sys.info()[["sysname"]] == "Darwin") "macos"
        else "linux"

  first_existing <- function(cands) {
    cands <- cands[nzchar(cands)]
    hit <- cands[file.exists(cands)]
    if (length(hit)) unname(hit[[1]]) else NA_character_
  }

  praat <- first_existing(c(
    Sys.which("praat"),
    "/Applications/Praat.app/Contents/MacOS/Praat",
    "/usr/bin/praat", "/usr/local/bin/praat",
    "C:/Program Files/Praat/Praat.exe",
    "C:/Program Files (x86)/Praat/Praat.exe"
  ))
  ffmpeg <- first_existing(Sys.which("ffmpeg"))
  pandoc <- first_existing(c(Sys.which("pandoc"),
                             Sys.getenv("RSTUDIO_PANDOC")))
  chrome <- first_existing(c(
    Sys.which("google-chrome"), Sys.which("chromium"),
    Sys.which("chromium-browser"),
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "C:/Program Files/Google/Chrome/Application/chrome.exe"
  ))

  hints <- list(
    praat = c(macos = "brew install --cask praat  (o praat.org)",
              windows = "Descarga Praat.exe de praat.org y añádelo al PATH",
              linux = "sudo apt install praat  (o praat.org)"),
    ffmpeg = c(macos = "brew install ffmpeg",
               windows = "choco install ffmpeg  (o ffmpeg.org)",
               linux = "sudo apt install ffmpeg"),
    pandoc = c(macos = "brew install pandoc",
               windows = "choco install pandoc",
               linux = "sudo apt install pandoc"),
    chrome = c(macos = "Instala Google Chrome (necesario solo para informes PDF)",
               windows = "Instala Google Chrome (necesario solo para informes PDF)",
               linux = "sudo apt install chromium-browser")
  )

  mk <- function(name, path) {
    list(found = !is.na(path) && nzchar(path),
         path  = path,
         hint  = unname(hints[[name]][[os]]))
  }

  list(
    praat  = mk("praat",  praat),
    ffmpeg = mk("ffmpeg", ffmpeg),
    pandoc = mk("pandoc", pandoc),
    chrome = mk("chrome", chrome)
  )
}
