test_that("rda fixture exists", {
    db_path <- testthat::test_path("fixtures", "test.rda")
    expect_true(file.exists(db_path))
})

test_that("rda fixture 2read data works", {
    db_path <- testthat::test_path("fixtures", "test.rda")
    result <- leer(db_path)
    expect_equal( result$test1() |> nrow(), 32)
})
