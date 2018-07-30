library(RMySQL)

user  <- 'openday'
pass  <- 'openday'
db    <- 'openday'
table <- 'noodle'

# fetch from the database if possible
read_rows <- function(columns) {
  values <- NULL

  try({
    conn <- dbConnect(RMySQL::MySQL(), user=user, password=pass, host='localhost', dbname=db)

    fields <- paste(columns, collapse=",")
    values <- dbGetQuery(conn, paste("SELECT ",fields, " FROM", table))

    dbDisconnect(conn)
  }, silent=TRUE)

  values
}

# create the database if possible
create_database <- function(columns) {
  success <- FALSE

  try({
    conn <- dbConnect(RMySQL::MySQL(), user=user, password=pass, host='localhost', dbname=db)
    
    # create score table if not already there
    fields <- paste(c("sample_id INT PRIMARY KEY AUTO_INCREMENT", sprintf("%s INT", columns)), collapse=",")

    res <- dbSendQuery(conn, paste("CREATE TABLE IF NOT EXISTS", table, "(", fields, ");"))
    dbClearResult(res)

    dbDisconnect(conn)

    success <- TRUE
  }, silent=TRUE)

  success
}

# write row to the database if possible
write_row <- function(columns, values) {
  success <- FALSE

  try({
    conn <- dbConnect(RMySQL::MySQL(), user=user, password=pass, host='localhost', dbname=db)

    # ignore NAs
    wch <- which(!is.na(values))
    if (length(wch) > 0) {
      cols <- paste(columns[wch], collapse=",")
      vals <- paste(values[wch], collapse=",")
      sql  <- paste("INSERT INTO", table ,"(",cols,") VALUES(",vals,");")
      cat("SQL is:", sql, "\n")

      res <- dbSendQuery(conn, sql)
      dbClearResult(res)
    }

    dbDisconnect(conn)
    success <- TRUE
  }, silent=TRUE)

  success
}
