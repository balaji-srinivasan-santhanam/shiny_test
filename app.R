library(shiny)
library(rdrop2)

# Function to get Dropbox token from environment variable
get_dropbox_token <- function() {
  structure(list(access_token = Sys.getenv("DROPBOX_TOKEN")),
            class = "DropboxToken")
}

ui <- fluidPage(
  titlePanel("Dropbox CSV Viewer"),
  sidebarLayout(
    sidebarPanel(
      actionButton("load", "Load CSV from Dropbox")
    ),
    mainPanel(
      tableOutput("data"),
      verbatimTextOutput("messages")
    )
  )
)

server <- function(input, output, session) {
  token <- get_dropbox_token()
  
  # Reactive values to store data or messages
  rv <- reactiveValues(
    df = NULL,
    msg = NULL
  )
  
  observeEvent(input$load, {
    tmp <- tempfile(fileext = ".csv")
    
    # Safe download with error handling
    tryCatch({
      drop_download(
        path = "data/example.csv",
        local_path = tmp,
        dtoken = token,
        overwrite = TRUE
      )
      rv$df <- read.csv(tmp)
      rv$msg <- "CSV loaded successfully!"
    }, error = function(e) {
      rv$df <- NULL
      rv$msg <- paste("Error downloading CSV:", e$message)
    })
  })
  
  output$data <- renderTable({
    rv$df
  })
  
  output$messages <- renderText({
    rv$msg
  })
}

shinyApp(ui, server)
