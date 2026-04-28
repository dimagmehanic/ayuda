#' try2leer - A generic function to read various file formats and database tables # nolint
#'
#' @param x the path to the file to read, can be a local file path
#' @param ... additional arguments passed to the specific read method (e.g., read_csv, read_xpt, etc.) # nolint
#'
#' @return a data frame or tibble containing the contents of the file
#' @export
#'
#' @examples
#' try2leer("data.csv")
#' try2leer("data.tsv")
#' try2leer("data.csv2")
#' try2leer("data.csv.gz")
#' try2leer("data.xpt")
#' try2leer("test.sql")
#' try2leer("test.db")

#' @export
try2leer <- function(x, ...) {
  UseMethod("try2leer")
}

# character
#' @export
try2leer.character <- function(x, ...) {

  # Auto-detect path type and read data
  ext <- tools::file_ext(tolower(trimws(x)))
  ext_bool <- nzchar(ext)
  ext_dbs <- c("sqlite", "zip", "tar", "db", "sqlite3", "db3", "sql")
  isdir <- file.info(x)$isdir
  mysql  <- FALSE

  # If directory does not exist treat it as possible mysql database
  if (is.na(isdir) && !ext_bool) {
    isdir  <- TRUE
    mysql <- TRUE
  } else if (is.na(isdir)){
    isdir  <- FALSE
  }

  if (!isdir && ext_bool && !(ext %in% ext_dbs)) {
    # attach file as class to filename
    y <- structure(x, class = "file")
    leer(y, ...)
  } else if (isdir && !ext_bool) {

    # Friendly message about  object
    message("✅ R6 object of class 'DataLoader' returned.\n",
            "You can use its methods to interact with the data, for example:\n",
            "  obj$list()   # list data\n",
            "  obj$info()   # show info")

    if (!(mysql)) { 
      invisible(DataCargar$new(x, db = "dir"))  # call DataLoader R6 class
    } else {
      invisible(DataCargar$new(x, db = "mysql"))  # call DataLoader R6 class
    }

  } else if (!isdir && ext_bool && ext %in% ext_dbs) {
    # Friendly message about  object
    message("✅ R6 object of class 'DataLoader' returned.\n",
            "You can use its methods to interact with the data, for example:\n",
            "  obj$list()   # list data\n",
            "  obj$info()   # show info")
    invisible(DataCargar$new(x, db = ext)) # call DataLoader R6 class
  } else if (isdir && ext_bool) {
    stop("❌ File does not exist: ", x)
  }
}