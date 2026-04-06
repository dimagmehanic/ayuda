#' Data loader class
#'
#' @description Load data from files, directories, archives, or databases.
#'
#' @field datos type of data source (character: "sqlite", "mysql", "zip", etc.)
#'
#' @examples
#' # SQL example
#' # db <- DataCargar$new("data.db", db = "sqlite")
#' # db$leer("my_table")
#' # db$disconnect()
#' # zip example
#' # db <- DataCargar$new("data.zip", db = "zip")
#' # db$leer("file_in_zip.csv")
#' # db$disconnect()

# Define R6 class
#' @export
DataCargar <- R6::R6Class("DataCargar", # nolint
  public = list(
    datos = NULL,
    # Initialize db type: "mysql", "sqlite", "zip", "tar", "db" etc or path to folder # nolint
    #' @description Initialize a new DataCargar object
    #' @param x Path to file, directory, archive, or a DBIConnection object
    #' @param db type of database
    #' @param ... Additional arguments passed to internal methods
    initialize = function(x, db = NULL, ...) {

      args  <- list(...)

      if (is.na(file.info(x)$isdir)) {
        stop("❌ Path does not exist: ", x)
      }

      if (is.null(args$db)) {
        self$guess_ext(x)
      }

      if (db %in% c("sqlite", "db", "sqlite3", "db3")) {
        self$datos <- db
        private$pointer <- private$connect_sqlite(x, ...)
      } else if (db == "sql") {
        self$datos <- db
        private$pointer <- private$connect_sql(x, ...)
      } else if (db == "dir") {
        self$datos <- db
        private$pointer <- private$connect_dir(x, ...)
      } else if (db == "zip") {
        self$datos <- db
        private$pointer <- private$connect_zip(x, ...)
      } else if (db == "tar") {
        self$datos <- db
        private$pointer <- private$connect_tar(x, ...)
      } else if (db == "mysql") {
        self$datos <- db
        private$pointer <- private$connect_mysql(dbname = x, ...)
      } else {
        stop("❌ Unsupported database.")
      }
    },

    # Method to read a table(s)
    #' @description Read data
    #' @param table_name for databases or filename
    #' @param ... Additional arguments passed to read functions
    #' @return tibble/data.frame/lazy table
    leer = function(table_name, ...) {

      if (is.null(private$pointer)) stop("No connection established")

      if (all(table_name %in% self.list())) {
        if (self$datos %in% c("zip", "tar", "dir")) {
          files <- list_dir(private.pointer, full.names = TRUE, recursive = TRUE) # nolint
          # attach extension as class to filepath
          y <- files[table_name]
          # attach file as class to filename
          y <- structure(y, class = "file")
          leer(y, ...)
        }else if (self$datos %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "sql")) { # nolint
          leer(private$pointer, table_name, ...)
        }
      }else {
        message(
          sprintf("❌ Table '%s' not found in the database.\nAvailable tables: %s", # nolint
                  table_name,
                  paste(self.list(), collapse = ", "))
        )
      }

    },

    #' @description Guess file extension
    #' @param x path or file
    #' @return character string
    guess_ext = function(x) {
      # Auto-detect path type and read data
      tools::file_ext(tolower(trimws(x)))
    },

    # Disconnect method
    #' @description Disconnect from DB if needed
    disconnect = function() {

      if (!is.null(private$pointer)) {
        if (self$datos %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "sql")) { # nolint
          DBI::dbDisconnect(private$pointer)
        } else if (self$datos %in% c("dir")) {
          private$pointer <- NULL
        } else if (self$datos %in% c("zip", "tar")) {
          unlink(private$pointer, recursive = TRUE, force = TRUE)
          private$pointer <- NULL
        }
      }
    },

    #' @description List tables or files
    #' @return character vector
    list = function() {
      message("🔍 Listing available tables...")

      if (self$datos %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "sql")) {

        message("🗄️ Detected database source")
        tables <- DBI::dbListTables(private.pointer)

        message("📊 Found ", length(tables), " table(s)")
        return(tables) # nolint

      } else if (self$datos %in% c("zip", "tar", "dir")) {

        message("📁 Detected file-based source (archive/directory)")
        files <- list_dir(private.pointer, full.names = TRUE, recursive = TRUE)

        message("📄 Found ", length(files), " file(s)")
        return(names(files)) # nolint

      } else {

        message("⚠️ Unsupported data source type: ", self$datos)
        return(NULL) # nolint
      }
    },

    #' @return The DataCargar object itself (invisibly), allowing for method chaining # nolint
    #' @export
    info = function() {
      message("ℹ️ DataCargar object info")
      message("─────────────────────────────")

      # Source type
      message("📂 Data source type: ", self$datos)

      # DB vs files
      if (!is.null(private$pointer)) {
        message("🗄️ Database connection active")
        tables <- self.list()
        message("📊 Tables available: ", paste(tables, collapse = ", "))
      }
      message("─────────────────────────────")
      invisible(self)
    }

  ),

  private = list(
    pointer = NULL,
    # MySQL connection
    connect_mysql = function(...) {

      args <- list(...)

      args$dbname <- ask_input(args$dbname,
                               "🗄️ Database name (default: mysql): ",
                               "mysql")
      args$host <- ask_input(args$host,
                             "🌐 Host (e.g., localhost): ",
                             "127.0.0.1")
      args$port <- ask_input(args$port,
                             "🔌 Port (e.g., 3306): ",
                             "3306")
      args$user <- ask_input(args$user,
                             paste0("👤 User (default: ", whoami(), "): "),
                             whoami())

      ### Connection Check
      tryCatch(
        {
          message("🔄 Attempting MySQL database connection...")
          con <- do.call(DBI::dbConnect, c(list(RMariaDB::MariaDB()), args, list(password = passwd()))) # nolint
          message("✅ Connection successful")
          return(con)
        },
        error = function(e) {
          message("❌ Connection failed: ", e$message)
        }
      )
    },

    # SQLite connection
    connect_sqlite = function(db_file, ...) {
      ### Connection
      tryCatch(
        {
          message("🔄 Attempting SQLite database connection...")
          con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file, ...)
          message("✅ Connection successful")
          return(con)
        },
        error = function(e) {
          message("❌ Connection failed: ", e$message)
        }
      )
    },

    # SQL dump
    connect_sql = function(db_file, ...) {
      ### Connection
      tryCatch(
        {
          message("🔄 Attempting SQLite database connection...")
          con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", ...)
          sql_script <- readLines(db_file)
          sql_script <- paste(sql_script, collapse = "\n")

          # Execute the dump (creates tables and inserts data)
          DBI::dbExecute(con, sql_script)
          message("✅ Connection successful")
          return(con)
        },
        error = function(e) {
          message("❌ Connection failed: ", e$message)
        }
      )
    },

    ### 📦 ZIP Connection (extract to temp)
    connect_zip = function(db_file, ...) {
      tryCatch(
        {
          message("📦 Preparing temporary workspace for ZIP extraction...")
          temp_dir <- tempfile("temp_zip_")  # unique temporary directory
          dir.create(temp_dir)
          message("📁 Temporary directory created: ", temp_dir)
          message("✅ Ready to extract and process ZIP contents")
          utils::unzip(db_file, exdir = temp_dir)
          return(temp_dir)
        },
        error = function(e) {
          message("❌ Failed to prepare temporary directory: ", e$message)
        }
      )
    },
    ### 📦 TAR Connection (extract to temp)
    connect_tar = function(db_file, ...) {
      tryCatch(
        {
          message("📦 Preparing temporary workspace for TAR extraction...")

          temp_dir <- tempfile("temp_tar_")  # unique temporary directory
          dir.create(temp_dir)

          message("📁 Temporary directory created: ", temp_dir)
          message("✅ Ready to extract and process TAR contents")
          utils::untar(db_file, exdir = temp_dir)

          return(temp_dir)
        },
        error = function(e) {
          message("❌ Failed to prepare temporary directory: ", e$message)
        }
      )
    },
    ### 📦 DIR Connection (extract to temp)
    connect_dir = function(db_file, ...) {
      tryCatch(
        {
          message("✅ Ready to extract and process dir contents")
          return(db_file)
        },
        error = function(e) {
          message("❌ Failed to use directory: ", e$message)
        }
      )
    }
  )
)
