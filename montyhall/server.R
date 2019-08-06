library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)

source("sample_db.R")
#source("sample_csv.R")

# storage
columns <- c("change", "won", "year")

current_year = lubridate::year(Sys.Date())

# create database for noodles
if (!create_database("montyhall", columns)) {
  cat("Unable to create database\n", file=stderr());
}

shinyServer(function(input, output, session) {

  v <- reactiveValues(samples = read_rows("montyhall", columns))

  # make the submit button do something useful
  observeEvent(input$submit, {
    sample <- c(ifelse(input$change == "Yes", 1, 0),
                ifelse(input$won == "Yes", 1, 0))

    # write the results to the database
    v$samples <- rbind(v$samples, c(sample, current_year))
    if (!write_row("montyhall", columns, c(sample, current_year))) {
      cat("Unable to write sample to database\n", file=stderr())
    }
  })

  # plot on the left shows actual data
  output$graph <- renderPlot( {
    dat <- na.omit(v$samples) %>% mutate(Mind = factor(ifelse(change, "Changed mind", "Didn't change mind")),
                                         Outcome = factor(ifelse(won, "Won", "Lost")))
    if (nrow(dat) > 0) {
      ggplot(dat, aes(x=Mind, fill=Outcome)) +
        geom_bar(position='dodge') + ylab("Count") +
        theme_bw(base_size=18) + scale_y_continuous(expand=c(0,0,0.1,0)) +
        theme(axis.title.x = element_blank())
    }
  })
})
