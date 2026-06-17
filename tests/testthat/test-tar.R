test_that("tar fixture exists", {
    db_path <- testthat::test_path("fixtures", "test.tar")
    expect_true(file.exists(db_path))
})

test_that("tar fixture 2read data works", {
    db_path <- testthat::test_path("fixtures", "test.tar")
    result <- leer(db_path)
    expect_equal( result$leer("test") |> nrow(), 32)
})
