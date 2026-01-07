# app.R
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Shiny App Reading from GCS"),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  
  # Reactive container for the data
  DATA <- reactiveVal(NULL)
  
  # Authenticate and load data once at startup
  observe({
    tryCatch({
      authenticate_gcs()          # Authenticate
      DATA(load_data_from_gcs())  # Load data
    }, error = function(e) {
      DATA(NULL)
      message("ERROR loading DATA: ", e$message)
    })
  })
  
  # Render plot
  output$scatter <- renderPlot({
    df <- DATA()
    if (is.null(df)) {
      plot.new()
      text(0.5, 0.5, "DATA not available. Check environment variable or permissions.", cex = 1.2)
    } else {
      ggplot(df, aes(x, y)) +
        geom_point() +
        theme_minimal()
    }
  })
}

# Run app
shinyApp(ui, server)
