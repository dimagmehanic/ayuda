
sql_connect_db <- function(passw, dbname = "temp") {
  ### Connection Check
  can_con <- dbCanConnect(RMySQL::MySQL(),
                          dbname = dbname,
                          host = "127.0.0.1",
                          port = 3306,
                          user = "hasand",
                          password = passw)
  if (can_con) {
    con <- dbConnect(RMySQL::MySQL(),
                     dbname = dbname,
                     host = "127.0.0.1",
                     port = 3306,
                     user = "hasand",
                     password = passw)
  }else {
    print(paste("Can't connect to database", dbname))
    con <- NULL
  }
}

load2mysql <- function(path, ...) {

  rest <- list(...)
  rest$ext <- askUser(rest$ext,
                      "Please provide file extension (default is xpt): ", 
                      "xpt")
  rest$dbname <- askUser(rest$dbname, 
                         "Please provide database name (default is mysql): ", 
                         "mysql")
  #user defined datasets not to read
  rest$notread <- askUser(rest$notread, 
                          "Please provide names of datasets not to read (default is NULL): ", 
                          NULL)
  
  #ask to define not readed datasets if not provided
  if (!is.null(rest$notread)){
    rest$readfunc <- askUser(rest$readfunc,
                         paste("Please provide function for", 
                               rest$notread,"dataframes (default is NULL): "), 
                         NULL)
    }
  
  #get names and location of data
  datapath <- getdata(path, ext=rest$ext) 
 
  # Connect to the MySQL database
  connect<- passwd() %>%
    sqlConnectDB(dbname = rest$dbname, ...)

  # # write data to the MySQL database
  for (name in names(datapath)) {
    if (!(name %in% rest$notread)){
      print(paste("Write table", name, "to database", rest$dbname))
      dbWriteTable(connect, name = name,
                   value = read_xpt(as.character(datapath[name])),
                   overwrite = TRUE, row.names = FALSE)
    }else if (!is.null(rest$notread)){
      print(paste("Write table", name, "to database", rest$dbname))

      rest$readfunc (datapath, name) %>% dbWriteTable(connect, name = name,
                                                      value = .,
                                                      overwrite = TRUE, 
                                                      row.names = FALSE)
    }
  }
}
