test_that("is_package is TRUE", {
  expect_true(is_package("datasets"))
})

test_that("package datasets 2read data works", {
  result <- leer("datasets")
  expect_equal( result$mtcars() |> nrow(), 32)
})
