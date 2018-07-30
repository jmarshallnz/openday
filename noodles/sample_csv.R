
# fetch from the database if possible
read_rows <- function(columns) {
  values <- NULL

  try({
    values = read.csv("db.csv")
  }, silent=TRUE)

  values
}

# create the database if possible
create_database <- function(columns) {
  success <- FALSE

  try({
    # check if file exists
    if (!file.exists("db.csv")) {
      # otherwise, create it
      scores <- matrix(NA, 1, length(colours))
      colnames(scores) <- colours
      write.csv(scores, "mandm.csv", row.names=FALSE)
    }, silent=TRUE)

  success
}

# write row to the database if possible
write_row <- function(columns, values) {
  success <- FALSE

  wch <- which(!is.na(values))
  if (length(wch) > 0) {
    cols <- paste(columns[wch], collapse=",")
    vals <- paste(values[wch], collapse=",")
    sql  <- paste("INSERT INTO", table ,"(",cols,") VALUES(",vals,");")
  }

  try({
    scores <- read.csv("mandm.csv")
    scores <- rbind(scores, sample)
    write.csv(scores, "mandm.csv", row.names=FALSE)
  
    
    # ignore NAs
    if (length(wch) > 0) {
      res <- dbSendQuery(conn, sql)
      dbClearResult(res)
    }

    dbDisconnect(conn)
    success <- TRUE
  }, silent=TRUE)

  success
}
