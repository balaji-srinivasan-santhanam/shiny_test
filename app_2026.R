library(shiny)
library(rdrop2)

ui <- fluidPage(
  titlePanel("Dropbox CSV Viewer"),
  tableOutput("data")
)

server <- function(input, output, session) {
  # Authenticate once per session
  drop_auth(rdstoken = Sys.getenv("DROPBOX_TOKEN"))
  
  output$data <- renderTable({
    tmp <- tempfile(fileext = ".csv")
    tryCatch({
      drop_download(
        path = "Apps/icamp_test/data/example.csv",
        local_path = tmp,
        overwrite = TRUE
      )
      read.csv(tmp)
    }, error = function(e) {
      data.frame(Error = e$message)
    })
  })
}

shinyApp(ui, server)
