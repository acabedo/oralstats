test_that("oralstats_python_available devuelve un logical y nunca lanza error", {
  res <- oralstats_python_available()
  expect_type(res, "logical")
  expect_length(res, 1)
})

test_that("oralstats_python_available es FALSE para un modulo inexistente", {
  # Sin entorno Python ya es FALSE; con entorno, el modulo inventado no importa.
  expect_false(isTRUE(oralstats_python_available("modulo_inexistente_xyz123")))
})

test_that("install_oralstats_python valida el nivel", {
  expect_error(install_oralstats_python(level = "nivel-malo"))
})
