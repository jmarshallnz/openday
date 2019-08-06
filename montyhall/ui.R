library(shiny)
library(shinyWidgets)

shinyUI(fluidPage(
  titlePanel("Changing Minds with Statistics"),
  fluidRow(
    column(width=4, radioGroupButtons(
      inputId = "change",
      label = "Did you change your mind?",
      choices = c("Yes",
                  "No"),
      justified = TRUE
    ),radioGroupButtons(
      inputId = "won",
      label = "Did you win?",
      choices = c("Yes",
                  "No"),
      justified = TRUE
    ),actionButton(
      inputId = "submit",
      label = "Submit"
    )),
    column(width=8, plotOutput("graph", height="550px"))
  ))
)
