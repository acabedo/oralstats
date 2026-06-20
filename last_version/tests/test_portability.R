# Test de los helpers de portabilidad. Ejecutar desde Oralstats/:
#   Rscript tests/test_portability.R
source("R/portability.R")

# oralstats_python() devuelve NA o la ruta a un intérprete existente
py <- oralstats_python()
stopifnot(length(py) == 1L)
stopifnot(is.na(py) || file.exists(py))

# check_system_deps() devuelve la estructura esperada
deps <- check_system_deps()
stopifnot(all(c("praat", "ffmpeg", "pandoc", "chrome") %in% names(deps)))
for (d in deps) {
  stopifnot(all(c("found", "path", "hint") %in% names(d)))
  stopifnot(is.logical(d$found), length(d$found) == 1L)
}
cat("OK: check_system_deps\n")

cat("OK: oralstats_python\n")
