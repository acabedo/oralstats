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
    "shiny", "bslib", "shinyjs", "ggplot2", "dplyr", "tidyr", "DT", "av", "plotly",
    "RColorBrewer", "data.table", "mgcv", "ggeffects", "ggfun", "jsonlite",
    "seewave", "tuneR", "udpipe", "reticulate"
  )
  faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan) == 0) return(invisible())

  message("OralStats: instalando paquetes de R que faltan: ", paste(faltan, collapse = ", "))

  # Usar el repo configurado; si no hay uno válido, PPM (binarios, rápido).
  repos <- getOption("repos")
  cran  <- tryCatch(repos[["CRAN"]], error = function(e) NA_character_)
  if (is.null(cran) || is.na(cran) || !nzchar(cran) || identical(cran, "@CRAN@")) {
    repos <- c(CRAN = "https://packagemanager.posit.co/cran/latest")
  }

  tryCatch(
    install.packages(faltan, repos = repos),
    error = function(e) message("Aviso: fallo instalando algún paquete de R: ",
                                conditionMessage(e))
  )
})
