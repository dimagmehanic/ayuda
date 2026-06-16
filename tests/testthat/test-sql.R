test_that("db fixture exists", {
    db_path <- testthat::test_path("fixtures", "test.sql")
    expect_true(file.exists(db_path))
})

test_that("db fixture works", {
    db_path <- testthat::test_path("fixtures", "test.sql")
    result <- try2leer(db_path)
    expect_equal( result$read("mtcars") |> dplyr::collect() |> nrow(), 32)
})
