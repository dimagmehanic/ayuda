test_that("rds fixture exists", {
  db_path <- testthat::test_path("fixtures", "test.rds")
  expect_true(file.exists(db_path))
})

test_that("rds fixture 2read data works", {
  db_path <- testthat::test_path("fixtures", "test.rds")
  result <- leer(db_path)
  expect_equal( result |> nrow(), 32)
})
