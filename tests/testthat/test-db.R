test_that("db fixture exists", {
    db_path <- testthat::test_path("fixtures", "test.db")
    expect_true(file.exists(db_path))
})

test_that("db fixture works", {
    db_path <- testthat::test_path("fixtures", "test.db")
    result <- try2leer(db_path)
    expect_equal( result$read("mtcars") |> dplyr::collect() |> nrow(), 32)
})
