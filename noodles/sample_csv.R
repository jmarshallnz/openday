db_file <- "db.csv"

# fetch from the database if possible
read_rows <- function(columns) {
  values <- NULL

  try({
    values = read.csv(db_file)
  }, silent=TRUE)

  values
}

# create the database if possible
create_database <- function(columns) {
  success <- FALSE

  try({
    # check if file exists
    if (!file.exists(db_file)) {
      # otherwise, create it
      db <- matrix(NA, 1, length(columns))
      colnames(db) <- columns
      write.csv(db, db_file, row.names=FALSE)
    }
    success <- TRUE
    }, silent=TRUE)

  success
}

# write row to the database if possible
write_row <- function(columns, values) {
  success <- FALSE

  try({
    db <- read.csv(db_file)
    db <- rbind(db, values)
    write.csv(db, db_file, row.names=FALSE)
    success <- TRUE
  }, silent=TRUE)

  success
}
