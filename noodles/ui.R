library(shiny)

# fancy input function
inlineNumeric<-function (inputId, label, value = 0, min = NULL, max = NULL, step = NULL, width=NULL, color=NULL) 
{
  style = "display:inline-block;"
  if (!is.null(width)) style = paste0(style, "max-width: ", width, ";")
  text_style = NULL
  if (!is.null(color)) text_style = paste0("background-color: ", color, ";")
  div(class="form-group shiny-input-container",
      style=style,
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type="number", class="form-control", value = value, min=min, max=max, step=step, style=text_style))
}

createInput <- function(num, width="80px") {
  inlineNumeric(inputId=paste0("count",num), label=paste("Noodle", num), value = NA, min = 0, max = 20, width=width)
}

inputs <- list()
inputs[[1]] <- h4("Sample input")
inputs <- c(inputs, lapply(1:5, createInput, width="80px"))
inputs[[length(inputs)+1]] <- actionButton("submit", label="Submit")
inputs[[length(inputs)+1]] <- sliderInput("slider", label=NULL, value=0, min=0, max=100, step=1,
                                          animate=animationOptions(interval=500, loop=TRUE))

shinyUI(fluidPage(
  titlePanel("Throwing Noodles to estimate \U03C0"),
  fluidRow(
    column(width=4, plotOutput("data", height="550px")),
    column(width=8, plotOutput("history", height="550px"))
  ),
  do.call(wellPanel, inputs)
  )
)
