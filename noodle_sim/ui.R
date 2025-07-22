library(shiny)

inputs <- list()
inputs[[length(inputs)+1]] <- sliderInput("slider", label=NULL, value=0, min=0, max=100, step=1,
                                          animate=animationOptions(interval=1000, loop=TRUE))

shinyUI(fluidPage(
  titlePanel("Throwing Noodles to estimate \U03C0"),
  fluidRow(plotOutput("history", height="550px")),
  do.call(wellPanel, inputs)
  )
)
