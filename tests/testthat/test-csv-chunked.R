test_that("csv fixture exists", {
  db_path <- testthat::test_path("fixtures", "test.csv")
  expect_true(file.exists(db_path))
})

test_that("csv fixture works", {
  
  db_path_db <- testthat::test_path("fixtures", "test-chunked.db")
  db_path_csv <- testthat::test_path("fixtures", "test.csv")
  result <- leer(db_path_db)
  result$write_chunked(db_path_csv, db_args=list(overwrite = TRUE) )
  result <- leer(db_path_db, show_col_types = FALSE)
  
  expect_equal( result$leer("test") |> dplyr::collect() |> nrow(), 32)
  
  result$disconnect()
})
