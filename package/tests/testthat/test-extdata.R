test_that("hay un corpus de ejemplo cargable", {
  p <- system.file("extdata", "ejemplo_corpus.rds", package = "oralstats")
  expect_true(nzchar(p))
  obj <- readRDS(p)
  expect_s3_class(obj, "data.frame")
  expect_gt(nrow(obj), 0)
})
