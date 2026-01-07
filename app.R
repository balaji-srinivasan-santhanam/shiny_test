library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Shiny App Reading from GCS"),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  DATA <- reactiveVal(NULL)
  
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
      text(0.5, 0.5, "DATA not available. Check environment variable or permissions.", cex = 1.2)
    } else {
      ggplot(df, aes(x, y)) +
        geom_point() +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)
