#' Connect to MySQL database
#'
#' @param passw password for MySQL database connection
#' @param ... additional parameters for MySQL database connection,
#' such as dbname, host, port, user, etc.
#'
#' @return a connection object to the MySQL database
#' @export
#'
#' @examples
#' sql_connect_db(passwd())

sql_connect_db <- function(passw, ...) {
  ### Connection Check
  can_con <- DBI::dbCanConnect(RMySQL::MySQL(),
                               password = passw,
                               ...)
  if (can_con) {
    con <- DBI::dbConnect(RMySQL::MySQL(),
                          password = passw,
                          ...)
  }else {
    print("Can't connect to database")
    con <- NULL
  }
  return(con) # nolint
}