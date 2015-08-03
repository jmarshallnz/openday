library(shiny)

# fancy input function
inlineNumeric<-function (inputId, label, value = "", width=NULL) 
{
  style = "display:inline-block;color='yellow';"
  if (!is.null(width)) style = paste0(style, "max-width: ", width, ";")
  div(class="form-group shiny-input-container",
      style=style,
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type="number", class="form-control", value = value))
}

shinyUI(#fluidPage(
  # Show a plot of the generated distribution
  #fluidRow(
    bootstrapPage(
      inlineNumeric(inputId="xlimitsmin", label="x-min", value = 0.0, width="50px"),
      inlineNumeric(inputId="xlimitsmax", label="x-max", value = 0.5, width="50px")
    )
#  )
#)
)
