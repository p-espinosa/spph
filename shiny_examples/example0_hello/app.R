# Example 0 of the talk: the smallest Shiny app
# Run it: open this file in RStudio and click "Run App",
# or from the materials folder: shiny::runApp("shiny_examples/example0_hello")
#
# Every Shiny app has the same three parts:
#   ui      what the page shows (inputs and empty slots for outputs)
#   server  how outputs are computed from inputs
#   shinyApp(ui, server) puts them together
#
# Try: change the slider range, add a second sentence, add a textInput("name", "Your name").

library(shiny)

ui <- fluidPage(
  titlePanel("Hello Shiny"),
  sliderInput("n", "How many weeks?", min = 1, max = 104, value = 52),
  textOutput("message")
)

server <- function(input, output, session) {
  output$message <- renderText({
    paste("You selected", input$n, "weeks.")   # re-runs every time input$n changes
  })
}

shinyApp(ui, server)
