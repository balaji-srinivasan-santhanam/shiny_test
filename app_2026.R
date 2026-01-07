ui <- fluidPage(
  titlePanel("Shiny App with GCS Data"),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  
  DATA <- reactiveVal(NULL)
  
  # Authenticate and load data once at startup
  observe({
    tryCatch({
      authenticate_gcs()
      DATA(load_data_from_gcs())
    }, error = function(e) {
      DATA(NULL)
      message("ERROR loading DATA: ", e$message)
    })
  })
  
  output$scatter <- renderPlot({
    df <- DATA()
    if (is.null(df)) {
      plot.new()
      text(0.5, 0.5, "DATA not available. Check permissions or logs.", cex = 1.5)
    } else {
      library(ggplot2)
      ggplot(df, aes(x, y)) +
        geom_point() +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)
