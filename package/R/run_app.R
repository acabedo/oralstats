#' Carpeta de datos del usuario para oralstats
#'
#' Devuelve (y crea, si no existe) la carpeta estándar donde oralstats persiste
#' configuración, copias de seguridad, informes y audios. Usa
#' \code{tools::R_user_dir()} para respetar las convenciones del sistema.
#' @return Ruta (cadena) a la carpeta de datos del usuario.
#' @export
oralstats_data_dir <- function() {
  d <- tools::R_user_dir("oralstats", "data")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

#' Subcarpeta dentro de la carpeta de datos del usuario
#' @param name Nombre de la subcarpeta (p. ej. "informes", "backup_etiquetado").
#' @return Ruta (cadena) a la subcarpeta, creada si no existía.
#' @keywords internal
#' @noRd
oralstats_subdir <- function(name) {
  s <- file.path(oralstats_data_dir(), name)
  if (!dir.exists(s)) dir.create(s, recursive = TRUE)
  s
}

#' Lanzar la aplicación oralstats
#'
#' Inicia la aplicación Shiny de Oralstats v1.8. Los datos del usuario se
#' guardan en \code{\link{oralstats_data_dir}()}. Para activar las funciones
#' que usan Python, ejecuta antes \code{\link{install_oralstats_python}()}.
#' @param ... Argumentos pasados a \code{shiny::runApp()} (p. ej.
#'   \code{launch.browser}, \code{port}, \code{host}).
#' @return Invisible. Se llama por su efecto: lanza la app.
#' @examples
#' \dontrun{
#' oralstats::run_app()
#' }
#' @export
run_app <- function(...) {
  options(shiny.maxRequestSize = 2048 * 1024^2)
  oralstats_data_dir()
  app_dir <- system.file("app", package = "oralstats")
  if (!nzchar(app_dir)) {
    stop("No se encuentra la carpeta 'app' del paquete oralstats; reinstala el paquete.")
  }
  shiny::runApp(app_dir, ...)
}
