#' leer - A generic function to read various file formats and database tables
#'
#' @param x the path to the file to read, can be a local file path
#' @param ... additional arguments passed to the specific read method (e.g., read_csv, read_xpt, etc.) # nolint
#'
#' @return a data frame or tibble containing the contents of the file
#' @export
#'
#' @examples
#' leer("data.csv")

#' @export
leer <- function(x, ...) {
  UseMethod("leer")
}

# file
#' @export
leer.file <- function(x, ...) {

  filename <- tolower(trimws(x))
  ext <- tools::file_ext(filename)

  # attach extension as class to filename
  y <- structure(x, class = ext)

  leer(y, ...)
}

# csv
#' @export
leer.csv <- function(filename, ...) {
  tryCatch(
    {
      message("📄 Reading CSV file...")
      readr::read_csv(filename, ...)
    },
    error = function(e) {
      message("↪️ Falling back to default method")
      leer.default(filename, ...)
    }
  )
}

# tsv
#' @export
leer.tsv <- function(filename, ...) {
  tryCatch(
    {
      message("📄 Reading TSV file (tab separated)...")
      readr::read_tsv(filename, ...)
    },
    error = function(e) {
      message("↪️ Falling back to default method")
      leer.default(filename, ...)
    }
  )
}

# csv2
#' @export
leer.csv2 <- function(x, ...) {
  tryCatch(
    {
      message("📄 Reading CSV2 file (semicolon separated)...")
      readr::read_csv2(x, ...)
    },
    error = function(e) {
      message("↪️ Falling back to default method")
      leer.default(x, ...)
    }
  )
}

# gz
#' @export
leer.gz <- function(x, ...) {
  message("📦 Reading gzipped file...")
  filename <- tolower(trimws(x))
  # get inner file extension
  inner_ext <- tools::file_ext(sub("\\.gz$", "", filename))
  # pass connection but keep class for dispatch
  y <- structure(x, class = inner_ext)
  leer(y, ...)
}

# xpt
#' @export
leer.xpt <- function(x, ...) {
  message("📊 Reading SAS XPT file...")
  haven::read_xpt(x, ...)
}

# SQLiteConnection
#' @export
leer.SQLiteConnection <- function(con, table, ...) {
  tryCatch(
    {
      message("🗄️ SQLite source detected")
      message("🔄 Creating lazy table reference for: ", table)

      res <- dplyr::tbl(con, table)

      message("✅ Lazy table ready (use dplyr verbs or collect())")
      return(res)
    },
    error = function(e) {
      message("❌ Failed to access SQLite table: ", table)
      message("⚠️ Details: ", e$message)
    }
  )
}

# MariaDBConnection
#' @export
leer.MariaDBConnection <- function(con, table, ...) {
  tryCatch(
    {
      message("🗄️ MariaDB source detected")
      message("🔄 Creating lazy table reference for: ", table)

      res <- dplyr::tbl(con, table)

      message("✅ Lazy table ready (use dplyr verbs or collect())")
      return(res)
    },
    error = function(e) {
      message("❌ Failed to access MariaDB table: ", table)
      message("⚠️ Details: ", e$message)
    }
  )
}

# DBIConnection
#' @export
leer.DBIConnection <- function(con, table, ...) {
  tryCatch(
    {
      message("🗄️ Database connection detected")
      message("🔄 Creating lazy table: ", table)
      res <- dplyr::tbl(con, table)
      message("✅ Lazy table ready")
      return(res)
    },
    error = function(e) {
      message("❌ Failed to access table: ", table)
      message("⚠️ Details: ", e$message)
    }
  )
}

# default
#' @export
leer.default <- function(x, ...) {
  tryCatch(
    {
      readr::read_delim(x, ...)
    },
    error = function(e) {
      message("⚠️ leer.default() unable to read the file")
    }
  )
}
