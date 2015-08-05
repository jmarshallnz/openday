library(RMySQL)

# fetch from the database if possible
read_samples <- function(colours) {
  values <- NULL

  try({
    conn <- dbConnect(RMySQL::MySQL(), user='mandm', password='mandm', host='localhost', dbname='mandm')

    fields <- paste(colours, collapse=",")
    values <- dbGetQuery(conn, paste("SELECT ",fields, " FROM samples"))

    dbDisconnect(conn)
  }, silent=TRUE)

  values
}

# create the database if possible
create_database <- function(colours) {
  success <- FALSE

  try({
    conn <- dbConnect(RMySQL::MySQL(), user='mandm', password='mandm', host='localhost', dbname='mandm')

    # create score table if not already there
    fields <- paste(c("sample_id INT PRIMARY KEY AUTO_INCREMENT", sprintf("%s INT", colours)), collapse=",")

    res <- dbSendQuery(conn, paste("CREATE TABLE IF NOT EXISTS samples (", fields, ");"))
    dbClearResult(res)

    dbDisconnect(conn)

    success <- TRUE
  }, silent=TRUE)

  success
}

# write sample to the database if possible
write_sample <- function(colours, sample) {
  success <- FALSE

  try({
    conn <- dbConnect(RMySQL::MySQL(), user='mandm', password='mandm', host='localhost', dbname='mandm')

    cols <- paste(colours, collapse=",")
    vals <- paste(sample, collapse=",")
    sql  <- paste("INSERT INTO samples(",cols,") VALUES(",vals,");")

    res <- dbSendQuery(conn, sql)
    dbClearResult(res)

    dbDisconnect(conn)
    success <- TRUE
  }, silent=TRUE)

  success
}
