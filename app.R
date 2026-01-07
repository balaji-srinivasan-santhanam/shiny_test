# app.R

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Shiny App Reading from GCS"),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  
  output$scatter <- renderPlot({
    ggplot(DATA, aes(x, y)) +
      geom_point() +
      theme_minimal()
  })
  
}

shinyApp(ui, server)
