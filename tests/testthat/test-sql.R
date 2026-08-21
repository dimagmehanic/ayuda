test_that("sql fixture exists", {
  db_path <- testthat::test_path("fixtures", "test.sql")
  expect_true(file.exists(db_path))
})

test_that("sql fixture works", {
  db_path <- testthat::test_path("fixtures", "test.sql")
  result <- leer(db_path)
  expect_equal( result$leer("mtcars") |> dplyr::collect() |> nrow(), 32)
  result$disconnect()
})
