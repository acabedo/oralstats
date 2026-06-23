# Asegura los paquetes de R necesarios para arrancar OralStats.
# Instala los que FALTEN desde CRAN actual (binarios; sin compilar). Robusto en
# máquinas nuevas. Se sourcea al inicio de app.R y de run.R, así que funciona
# igual con `Rscript run.R` que abriendo app.R con "Run App" en RStudio/Positron.
#
# Nota: solo cubre el NÚCLEO necesario para que la app arranque. Los paquetes
# opcionales (los que el código usa con requireNamespace) se instalan bajo demanda
# y, si faltan, su función concreta queda desactivada (degradación elegante).
local({
  pkgs <- c(
    # Núcleo (necesario para arrancar)
    "shiny", "bslib", "shinyjs", "ggplot2", "dplyr", "tidyr", "DT", "av", "plotly",
    "RColorBrewer", "data.table", "mgcv", "ggeffects", "ggfun", "jsonlite",
    "seewave", "tuneR", "udpipe", "reticulate",
    # Opcionales de R (CRAN, ligeros: se instalan bien como binario)
    "readxl", "irr", "httr", "ggwordcloud", "wordcloud2", "partykit", "ggparty",
    "randomForest", "praatpicture", "rPraat", "pagedown", "base64enc", "stopwords",
    # Otros usados directamente en el código (vía library/::)
    "waiter", "processx", "remotes", "reshape2", "rmarkdown", "rstudioapi",
    "webshot2", "wrassp"
  )
  # NOTA: 'audio.whisper' y 'whisper' (R, de GitHub) se dejan fuera a propósito:
  # requieren compilación y no son CRAN. Si se quieren, se instalan aparte.
  faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan) == 0) return(invisible())

  message("OralStats: instalando paquetes de R que faltan: ", paste(faltan, collapse = ", "))

  # Usar el repo configurado; si no hay uno válido, PPM (binarios, rápido).
  repos <- getOption("repos")
  cran  <- tryCatch(repos[["CRAN"]], error = function(e) NA_character_)
  if (is.null(cran) || is.na(cran) || !nzchar(cran) || identical(cran, "@CRAN@")) {
    repos <- c(CRAN = "https://packagemanager.posit.co/cran/latest")
  }

  # CLAVE: NUNCA compilar desde fuente. Evita el prompt bloqueante
  # "¿instalar desde fuentes? (Yes/no)" que en un Positron nuevo cuelga TODA la
  # instalación (incluido shiny), y que se intente construir paquetes con Rust/C
  # como gifski (dependencia de praatpicture). En Mac/Windows -> SOLO binarios.
  options(install.packages.compile.from.source = "never")
  tipo <- if (.Platform$OS.type == "windows" ||
              identical(Sys.info()[["sysname"]], "Darwin")) "binary" else getOption("pkgType")

  # Una sola llamada install.packages(c(...)). Con compile.from.source='never' y
  # type='binary' (Mac/Win), los que no tengan binario se omiten con un aviso, sin
  # bloquear ni preguntar.
  tryCatch(
    suppressWarnings(install.packages(faltan, repos = repos, type = tipo)),
    error = function(e) message("Aviso: algún paquete de R no se instaló: ", conditionMessage(e))
  )
})
