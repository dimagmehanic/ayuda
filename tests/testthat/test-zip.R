test_that("zip fixture exists", {
    db_path <- testthat::test_path("fixtures", "test.zip")
    expect_true(file.exists(db_path))
})

test_that("zip fixture 2read data works", {
    db_path <- testthat::test_path("fixtures", "test.zip")
    result <- leer(db_path)
    expect_equal( result$leer("test") |> nrow(), 32)
})
