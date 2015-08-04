
# fetch from the database if possible
read_samples <- function() {
  values <- NULL

  try({
    values = na.omit(read.csv("mandm.csv"))
  }, silent=TRUE)

  values
}

# create the database if possible
create_database <- function(colours) {
  success <- FALSE

  try({
    # check if the file exists
    if (!file.exists("mandm.csv")) {
      # otherwise, create it
      scores <- matrix(NA, 1, length(colours))
      colnames(scores) <- colours
      write.csv(scores, "mandm.csv", row.names=FALSE)
    }
    success <- TRUE
  }, silent=TRUE)

  success
}

# write sample to the database if possible
write_sample <- function(colours, sample) {
  success <- FALSE

  try({
    scores <- read.csv("mandm.csv")
    scores <- rbind(scores, sample)

    write.csv(scores, "mandm.csv", row.names=FALSE)
    
    success <- TRUE
  }, silent=TRUE)

  success
}
