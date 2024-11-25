library(shiny)
ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  varSelectInput(
    inputId = "chosenVar",
    label = "Choose a variable:",
    data = mtcars
  ),
  plotOutput(outputId = "plotOfChosenVar")
)

server <- function(input, output) {
  output$plotOfChosenVar <- renderPlot({
    hist(mtcars[[input$chosenVar]], breaks = 20)
  })
}

shinyApp(ui = ui, server = server)
