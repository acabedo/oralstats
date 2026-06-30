# Deteccion de dependencias de sistema externas (Praat, ffmpeg, pandoc, Chrome).

#' Detecta binarios de sistema externos que OralStats puede usar.
#'
#' Devuelve una lista con elementos praat, ffmpeg, pandoc y chrome. Cada
#' elemento es una lista con found (logical), path (character) y hint (character).
#' @keywords internal
#' @noRd
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
              windows = "Descarga Praat.exe de praat.org y anadelo al PATH",
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
