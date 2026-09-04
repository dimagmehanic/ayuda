#' Data loader class
#'
#' @description Load data from files, directories, archives, or databases.
#'
#' @field datos type of data source (character: "sqlite", "mysql", "zip", etc.)
#'
#' @examples
#' # SQL example
#' # db <- GetData$new("data.db", db = "sqlite")
#' # db$leer("my_table")
#' # db$disconnect()
#' # zip example
#' # db <- GetData$new("data.zip", db = "zip")
#' # db$leer("file_in_zip.csv")
#' # db$disconnect()

# Define R6 class
#' @export
GetData <- R6::R6Class("GetData", # nolint
  public = list(
    datos = NULL,
    # Initialize db type: "mysql", "sqlite", "zip", "tar", "db" etc or path to folder # nolint
    #' @description Initialize a new GetData object
    #' @param x Path to file, directory, archive, or a DBIConnection object
    #' @param db database
    #' @param ... Additional arguments passed to internal methods
    initialize = function(x, db = NULL, ...) {

      args  <- list(db = db, ...)

      if (is.null(args$db)) {
        args$db <- self$guess_ext(x)
      }

      if (is.na(file.info(x)$isdir) &&
            !(args$db %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "postgres"))) {
        stop("❌ Path does not exist: ", x)
      } else if (is.na(file.info(x)$isdir) &&
                   (args$db %in% c("sqlite", "db", "sqlite3", "db3"))) {
        message("⚠️ Specified database path does not exist: ", x, ". Initializing a new database.") # nolint
      }

      if (args$db %in% c("sqlite", "db", "sqlite3", "db3")) {
        self$datos <- args$db
        private$pointer <- private$connect_sqlite(x, ...)
      } else if (args$db == "sql") {
        self$datos <- args$db
        private$pointer <- private$connect_sql(x, ...)
      } else if (args$db == "dir") {
        self$datos <- args$db
        private$pointer <- private$connect_dir(x, ...)
      } else if (args$db == "zip") {
        self$datos <- args$db
        private$pointer <- private$connect_zip(x, ...)
      } else if (args$db == "tar") {
        self$datos <- args$db
        private$pointer <- private$connect_tar(x, ...)
      } else if (args$db == "mysql") {
        self$datos <- args$db
        private$pointer <- private$connect_mysql(dbname = x, ...)
      } else if (args$db == "postgres") {
        self$datos <- args$db
        private$pointer <- private$connect_postgres(dbname = x, ...)
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

      if (all(table_name %in% self$list())) {
        if (self$datos %in% c("zip", "tar", "dir")) {
          files <- list_dir(private$pointer, full.names = TRUE, recursive = TRUE) # nolint
          # attach extension as class to filepath
          y <- unlist(files[table_name])
          # attach file as class to filename
          y <- structure(y, class = "file")
          leer(y, ...)
        }else if (self$datos %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "sql", "postgres")) { # nolint
          leer(private$pointer, table_name, ...)
        }
      }else {
        message(
          sprintf("❌ Table '%s' not found in the database.\nAvailable tables: %s", # nolint
                  table_name,
                  paste(self$list(), collapse = ", "))
        )
      }

    },

    # Method to write a table(s)
    #' @description Write data
    #' @param table_name name
    #' @param df dataframe or tibble
    #' @param ... Additional arguments passed to read functions
    write = function(df, table_name, ...) {

      if (is.null(private$pointer)) stop("No connection established")

      if (self$datos %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "sql", "postgres")) { # nolint
        DBI::dbWriteTable(private$pointer, name = table_name, value = df, ...)
      }else {
        message(sprintf("❌ Failed to write table '%s': the current database type does not support this operation.", # nolint
                        table_name))
      }

    },

    #' @description Write a large local file to the DB in chunks
    #' @param file_path path to the local data file to insert 
    #' @param db_args named list of arguments passed to the DB writer
    #'        (e.g. overwrite) for each chunk
    #' @param readr_args named list of arguments passed to the chunked reader
    #'        (e.g. col_types, delimiter, chunk_size)
    #' @return TRUE invisibly on success
    write_chunked = function(file_path, db_args = list(), readr_args = list()) {

      table_name <- tools::file_path_sans_ext(basename(file_path))

      if (is.null(private$pointer)) stop("No connection established")

      if (!(self$datos %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "sql", "postgres"))) { # nolint
        stop("❌ Chunked writing is only supported for database sources.")
      }

      message("📄 Reading '", file_path, "' in chunks (default 10000 rows) ...")

      first_chunk <- TRUE

      # Raw callback inserts each chunk into the DB. The first chunk creates the
      # table (overwrite=TRUE), subsequent chunks append. db_args is passed only
      # to the DB writer and read_args only to the reader, so the two never
      # overlap.
      cb <- function(x, pos) {
        message("CALLBACK START: pos=", pos, " rows=", nrow(x))
        if (first_chunk) {
          message("WRITING: pos=", pos, " rows=", nrow(x))
          do.call(self$write, c(list(x, table_name), db_args)) 
          
          first_chunk <<- FALSE
           
        }else {
          message("APPENDING")
          do.call(DBI::dbAppendTable,
                  c(list(private$pointer, name = table_name, value = x), db_args))
        }
        
        message("CALLBACK END")
      }
      
      # Reuse leer's existing chunked S3 methods (csv_chunked, tsv_chunked, ...).
      # leer() dispatches on the file's extension.
      do.call(leer, c(list(file_path, callback = cb), readr_args))

      message("✅ Finished writing '", table_name, "'.")
      invisible(TRUE)
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
        if (self$datos %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "sql", "postgres")) { # nolint
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
    #' @param ... Additional arguments passed to read functions
    #' @return character vector
    list = function(...) {
      message("🔍 Listing available tables...")

      if (self$datos %in% c("sqlite", "db", "sqlite3", "db3", "mysql", "sql", "postgres")) {

        message("🗄️ Detected database source")
        tables <- DBI::dbListTables(private$pointer)

        message("📊 Found ", length(tables), " table(s)")
        return(tables) # nolint

      } else if (self$datos %in% c("zip", "tar", "dir")) {

        message("📁 Detected file-based source (archive/directory)")
        files <- list_dir(private$pointer, full.names = TRUE, recursive = TRUE, ...) # nolint

        message("📄 Found ", length(files), " file(s)")
        return(names(files)) # nolint

      } else {

        message("⚠️ Unsupported data source type: ", self$datos)
        return(NULL) # nolint
      }
    },
    
    #' @description Connection pointer
    #' @return path or established connection
    #'
    #' Returns the internal connection pointer stored in \code{private$pointer}.
    #' For database sources this is the active DB connection (e.g. MySQL), and
    #' for file-based sources it is the path to the data. It is a read-only
    #' accessor and does not open or close any connection itself.
    con = function(){
      return(private$pointer)
    },

    #' @return The GetData object itself (invisibly), allowing for method chaining # nolint
    #' @export
    info = function() {
      message("ℹ️ GetData object info")
      message("─────────────────────────────")

      # Source type
      message("📂 Data source type: ", self$datos)

      # DB vs files
      if (!is.null(private$pointer)) {
        message("🗄️ Database connection active")
        tables <- self$list()
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

      args <- list()
      arggs <- list(...)

      args$dbname <- ask_input(arggs$dbname,
                               "🗄️ Database name (default: test): ",
                               "test")
      args$host <- ask_input(arggs$host,
                             "🌐 Host (e.g., localhost): ",
                             "127.0.0.1")
      args$port <- ask_input(arggs$port,
                             "🔌 Port (e.g., 3306): ",
                             "3306")
      args$user <- ask_input(arggs$user,
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


    # Postgres connection
    connect_postgres = function(...) {

      args <- list()
      arggs <- list(...)

      args$dbname <- ask_input(arggs$dbname,
                               "🗄️ Database name (default: test): ",
                               "test")
      args$host <- ask_input(arggs$host,
                             "🌐 Host (e.g., localhost): ",
                             "127.0.0.1")
      args$port <- ask_input(arggs$port,
                             "🔌 Port (e.g., 5432): ",
                             "5432")
      args$user <- ask_input(arggs$user,
                             paste0("👤 User (default: ", whoami(), "): "),
                             whoami())

      ### Connection Check
      tryCatch(
        {
          message("🔄 Attempting PostgreSQL database connection...")
          con <- do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), args, list(password = passwd()))) # nolint
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

          # Execute the dump (creates tables and inserts data)
          sql <- paste(readLines(db_file, warn = FALSE), collapse = "\n")

          statements <- strsplit(sql, ";", fixed = TRUE)[[1]]

          for (stmt in statements) {

            stmt <- trimws(stmt)

            if (nzchar(stmt)) {
              DBI::dbExecute(con, stmt)
            }
          }

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
