test_that("oralstats_data_dir devuelve una carpeta existente y escribible", {
  d <- oralstats_data_dir()
  expect_true(dir.exists(d))
  expect_match(d, "oralstats", ignore.case = TRUE)
  f <- file.path(d, "x.txt"); writeLines("ok", f)
  expect_true(file.exists(f)); unlink(f)
})

test_that("oralstats_subdir crea subcarpetas bajo data_dir", {
  s <- oralstats_subdir("informes")
  expect_true(dir.exists(s))
  expect_identical(dirname(s), oralstats_data_dir())
})
