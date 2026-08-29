test_that("dir exists", {
  db_path <- testthat::test_path("fixtures")
  expect_true(file.exists(db_path))
})

test_that("dir fixture list datasets works", {
  db_path <- testthat::test_path("fixtures")
  result <- leer(db_path)
  expect_equal( result$list() |> length(), 9)
})

test_that("dir fixture 2read data works", {
  db_path <- testthat::test_path("fixtures")
  result <- leer(db_path)
  expect_equal( result$leer("test") |> nrow(), 32)
})
