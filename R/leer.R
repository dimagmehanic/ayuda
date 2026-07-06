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

# character
#' @export
leer.character <- function(x, ...) {

    # Auto-detect path type and read data
    ext <- tools::file_ext(tolower(trimws(x)))
    ext_bool <- nzchar(ext)
    ext_dbs <- c("sqlite", "zip", "tar", "db", "sqlite3", "db3", "sql")
    isdir <- file.info(x)$isdir
    # if NA the set FALSE
    isdir <- ifelse(is.na(isdir), FALSE, isdir)


    if (!isdir && ext_bool && !(ext %in% ext_dbs)) {
        # attach file as class to filename
        y <- structure(x, class = "file")
        leer(y, ...)
    } else if (!isdir && ext_bool && ext %in% ext_dbs) {

        # Friendly message about  object
        message("✅ R6 object of class 'GetData' returned.\n",
                "You can use its methods to interact with the data, for example:\n",
                "  obj$list()   # list data\n",
                "  obj$info()   # show info")

        invisible(GetData$new(x, db = ext))  # call GetData R6 class

    } else if (isdir) {

        # Friendly message about  object
        message("✅ R6 object of class 'GetData' returned.\n",
                "You can use its methods to interact with the data, for example:\n",
                "  obj$list()   # list data\n",
                "  obj$info()   # show info")

        invisible(GetData$new(x, db = "dir"))  # call GetData R6 class

    } else if (is_package(x)) {
        # attach package as class to filename
        y <- structure(x, class = "package")
        leer(y, ...)
    } else {
        tryCatch(
            {
                dbname <- ask_input(
                    args$dbname,
                    "🗄️  Database name (default: postgres): ",
                    "postgres"
                )

                message(
                    "✅ Connected to database '", dbname, "'.\n",
                    "📦 Returning a 'GetData' R6 object.\n",
                    "Use the object's methods to work with the database, for example:\n",
                    "  • obj$list() # list data\n",
                    "  • obj$info() # show info"
                )

                invisible(GetData$new(x, db = dbname))

            },
            error = function(e) {
                message(
                    "❌ Failed to connect to database '", dbname, "'.\n",
                    "Reason: ", conditionMessage(e)
                )
                invisible(NULL)
            }
        )
    }
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

# rds
#' @export
leer.rds <- function(filename, ...) {
    tryCatch(
        {
            message("📄 Reading Rds file ...")
            readRDS(filename, ...)
        },
        error = function(e) {
            message("↪️ Falling back to default method")
            leer.default(filename, ...)
        }
    )
}

# rda
#' @export
leer.rda <- function(filename, ...) {
  ##lazy load rda files
  rda_load <- function(f){
    names <- load(f, ...)
    if (length(names) > 1) {
      message(
        "📦 RDA datasets have been lazy loaded.\n",
        "✅ No data has been loaded into memory yet.\n\n",
        "📖 To access an RDA file:\n",
        '   result <- leer("test.rda")\n\n',
        "🗂️ To access an object inside the RDA:\n",
        '   result[["test"]]()\n\n',
        "🔍 Available objects can be listed with:\n",
        "   names(result)"
      )
      df <- purrr::set_names(
        purrr::map(names, function(nm) {
          function() {
            # load the data
            load(f, ...)
            get(nm)
          }
        }),
        names)
    }else{
      df <- get(names[1])
    }
    return (df)
  }

  tryCatch(
  {
    message("📄 Reading rda file ...")
    rda_load(filename, ...)
  },
  error = function(e) {
    message("↪️ Falling back to default method")
    leer.default(filename, ...)
  }
  )
}


# package
#' @export
leer.package <- function(filename, ...) {
    ##lazy load package datasets
    package_load <- function(f){
        names <- data(package = f)$results[, "Item"]

        if (length(names) >= 1) {
            message(
                "📦 Lazy-loaded ", length(names), " dataset",
                if (length(names) != 1) "s" else "",
                " from package '", f, "'.\n",
                "Examples: ", paste(head(names, 3), collapse = ", "),
                if (length(names) > 3) ", ..." else "", "\n",
                "Load a dataset with:\n",
                "  data <- result[['dataset_name']]()"
            )
            df <- purrr::set_names(
                purrr::map(names, function(nm) {
                    function() {
                        # load the data
                        data(list = nm, package = f)
                        get(nm)
                    }
                }),
                names)
        }else{
            df <- NULL
        }
        return (df)
    }

    tryCatch(
        {
            message("📄 Reading package ", filename, " ...")
            package_load(filename, ...)
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

# PqConnection (PostgreSQL)
#' @export
leer.PqConnection <- function(con, table, ...) {
  tryCatch(
    {
      message("🗄️ PostgreSQL source detected")
      message("🔄 Creating lazy table reference for: ", table)

      res <- dplyr::tbl(con, table)

      message("✅ Lazy table ready (use dplyr verbs or collect())")
      return(res)
    },
    error = function(e) {
      message("❌ Failed to access PostgreSQL table: ", table)
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
