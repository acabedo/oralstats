test_that("app.R no escribe en getwd(): no quedan file.path(getwd(), ...) de persistencia", {
  app <- readLines(system.file("app", "app.R", package = "oralstats"))
  ofensores <- grep('file\\.path\\(getwd\\(\\),\\s*"(lasp_datos|backup_etiquetado|audios_emocion|informes|backup_varcustom)"', app)
  expect_length(ofensores, 0)
})

test_that("la app localiza el script Python por system.file", {
  expect_true(nzchar(system.file("python", "extract_with_parselmouth.py", package = "oralstats")))
})
