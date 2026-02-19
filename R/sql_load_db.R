#' Load data into MySQL database
#'
#' @param path the path to the directory containing the data files
#' @param ... additional parameters for loading data into the MySQL database,
#' such as file extension, database name, etc.
#'
#' @return no return value, the function writes data to the MySQL database
#' @export
#'
#' @examples sql_load_db(path = "path/to/data", ext = "xpt", dbname = "mysql")

sql_load_db <- function(path, ...) {

  rest <- list(...)

  rest$ext <- ask_input(rest$ext,
                        "Please provide file extension (default is xpt): ",
                        "xpt")

  rest$dbname <- ask_input(rest$dbname,
                           "Please provide database name (default is mysql): ",
                           "mysql")

  #user defined datasets not to read
  rest$notread <- ask_input(rest$notread,
                            "Please provide names of datasets not to read (default is NULL): ", # nolint
                            NULL)

  #ask to define not readed datasets if not provided
  if (!is.null(rest$notread)) {
    rest$readfunc <- ask_input(rest$readfunc,
                               paste("Please provide function for",
                                     rest$notread,"dataframes (default is NULL): "), # nolint
                               NULL)
  }

  #get names and location of data
  datapath <- list_dir(path, ext = rest$ext)

  # Connect to the MySQL database
  connect <- passwd() %>% sql_connect_db(...)

  # write data to the MySQL database
  for (name in names(datapath)) {
    if (!(name %in% rest$notread)) {
      print(paste("Write table", name, "to database", rest$dbname))
      DBI::dbWriteTable(connect, name = name,
                        value = haven::read_xpt(as.character(datapath[name])),
                        overwrite = TRUE, row.names = FALSE)
    }else if (!is.null(rest$notread)) {
      print(paste("Write table", name, "to database", rest$dbname))
      rest$readfunc(datapath, name) %>% DBI::dbWriteTable(connect, name = name,
                                                          value = .,
                                                          overwrite = TRUE,
                                                          row.names = FALSE)
    }
  }
}
