library(shiny)
#install.packages(c(
#  "googleCloudStorageR",
#  "jsonlite"
#))
library('jsonlite')
library('googleCloudStorageR')


# Read service account JSON from env var
gcs_json <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")

if (gcs_json == "") {
  stop("GCS_SERVICE_ACCOUNT_JSON not set")
}

# Authenticate
gcs_auth(jsonlite::fromJSON(gcs_json))


# ------------------------------
# Shiny UI
# ------------------------------
ui <- fluidPage(
  titlePanel("Dropbox CSV Viewer"),
  sidebarLayout(
    sidebarPanel(
      actionButton("load", "Load CSV from GCS")
    ),
    mainPanel(
      tableOutput("data"),
      verbatimTextOutput("messages")
    )
  )
)

load_data <- function() {
  tmp <- tempfile(fileext = ".rds")
  
  gcs_get_object(
    object_name = "example_data/mydata.RDS",
    bucket = "shiny-data",
    saveToDisk = tmp,
    overwrite = TRUE
  )
  
  readRDS(tmp)
}

DATA <- load_data_from_gcs()
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
