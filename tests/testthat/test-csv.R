test_that("csv fixture exists", {
    db_path <- testthat::test_path("fixtures", "test.csv")
    expect_true(file.exists(db_path))
})

test_that("csv fixture works", {
    db_path <- testthat::test_path("fixtures", "test.csv")
    result <- leer(db_path, show_col_types = FALSE)
    expect_equal( result |> nrow(), 32)
})
