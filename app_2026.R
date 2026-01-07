library(shiny)
library(rdrop2)

# ------------------------------
# Function to authenticate with Dropbox
# ------------------------------
get_dropbox_auth <- function() {
  # Read token from environment variable
  token <- Sys.getenv("DROPBOX_TOKEN")
  if (token == "") stop("Dropbox token not set in environment variables")
  
  # Authenticate rdrop2 session with long-lived token
  drop_auth(rdstoken = token)
}

# ------------------------------
# Shiny UI
# ------------------------------
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

# ------------------------------
# Shiny Server
# ------------------------------
server <- function(input, output, session) {
  
  # Reactive values for data and messages
  rv <- reactiveValues(df = NULL, msg = NULL)
  
  observeEvent(input$load, {
    
    # Authenticate Dropbox (once per session)
    tryCatch({
      get_dropbox_auth()
    }, error = function(e) {
      rv$df <- NULL
      rv$msg <- paste("Dropbox authentication failed:", e$message)
      return()
    })
    
    tmp <- tempfile(fileext = ".csv")
    
    # Attempt to download CSV safely
    tryCatch({
      drop_download(
        path = "Apps/icamp_test/data/example.csv",
        local_path = tmp,
        overwrite = TRUE
      )
      rv$df <- read.csv(tmp)
      rv$msg <- "CSV loaded successfully!"
    }, error = function(e) {
      rv$df <- NULL
      rv$msg <- paste("Error downloading CSV:", e$message)
    })
  })
  
  # Render outputs
  output$data <- renderTable({ rv$df })
  output$messages <- renderText({ rv$msg })
}

# ------------------------------
# Launch Shiny App
# ------------------------------
shinyApp(ui, server)
