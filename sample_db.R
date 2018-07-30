library(RMySQL)

user  <- 'openday'
pass  <- 'openday'
db    <- 'openday'
table <- 'mandm'

# fetch from the database if possible
read_samples <- function(colours) {
  values <- NULL

  try({
    conn <- dbConnect(RMySQL::MySQL(), user=user, password=pass, host='localhost', dbname=db)
    
    fields <- paste(c(colours, "year"), collapse=",")
    values <- dbGetQuery(conn, paste("SELECT ",fields, " FROM", table))

    dbDisconnect(conn)
  }, silent=TRUE)

  values
}

# create the database if possible
create_database <- function(colours) {
  success <- FALSE

  try({
    conn <- dbConnect(RMySQL::MySQL(), user=user, password=pass, host='localhost', dbname=db)
    
    # create score table if not already there
    fields <- paste(c("sample_id INT PRIMARY KEY AUTO_INCREMENT", sprintf("%s INT", c(colours, "year"))), collapse=",")

    res <- dbSendQuery(conn, paste("CREATE TABLE IF NOT EXISTS", table, "(", fields, ");"))
    dbClearResult(res)

    dbDisconnect(conn)

    success <- TRUE
  }, silent=TRUE)

  success
}

# write sample to the database if possible
write_sample <- function(colours, sample, year) {
  success <- FALSE

  try({
    conn <- dbConnect(RMySQL::MySQL(), user=user, password=pass, host='localhost', dbname=db)
    
    cols <- paste(c(colours,"year"), collapse=",")
    vals <- paste(c(sample, year), collapse=",")
    sql  <- paste("INSERT INTO", table ,"(",cols,") VALUES(",vals,");")
    
    res <- dbSendQuery(conn, sql)
    dbClearResult(res)

    dbDisconnect(conn)
    success <- TRUE
  }, silent=TRUE)

  success
}
