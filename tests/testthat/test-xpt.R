test_that("xpt fixture exists", {
  db_path <- testthat::test_path("fixtures", "test.xpt")
  expect_true(file.exists(db_path))
})

test_that("xpt fixture works", {
  db_path <- testthat::test_path("fixtures", "test.xpt")
  result <- leer(db_path)
  expect_equal( result |> nrow(), 32)
})
