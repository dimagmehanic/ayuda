test_that("db fixture exists", {
  db_path <- testthat::test_path("fixtures", "test.db")
  expect_true(file.exists(db_path))
})

test_that("db fixture to read data works", {
  db_path <- testthat::test_path("fixtures", "test.db")
  result <- leer(db_path)
  expect_equal( result$leer("mtcars") |> dplyr::collect() |> nrow(), 32)
  result$disconnect()
})
