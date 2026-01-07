# app.R
library(shiny)
library(ggplot2)


# global.R
# Load packages
library(googleCloudStorageR)
library(jsonlite)

# -----------------------------
# Function: Authenticate to GCS
# -----------------------------
authenticate_gcs <- function() {
  gcs_json <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
  
  if (nchar(gcs_json) == 0) {
    stop("GCS_SERVICE_ACCOUNT_JSON environment variable is not set!")
  }
  
  sa <- tryCatch(
    fromJSON(gcs_json),
    error = function(e) {
      stop("Failed to parse GCS_SERVICE_ACCOUNT_JSON: ", e$message)
    }
  )
  
  tryCatch(
    {
      gcs_auth(sa)
    },
    error = function(e) {
      stop("GCS authentication failed: ", e$message)
    }
  )
  
  message("Authenticated to GCS successfully")
}

# -----------------------------
# Function: Load data from GCS
# -----------------------------
load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".RDS")
  tryCatch(
    {
      gcs_get_object(
        object_name = "example_data/mydata.RDS",  # adjust path in your bucket
        bucket = "shiny-data",         # replace with your bucket
        saveToDisk = tmp,
        overwrite = TRUE
      )
      readRDS(tmp)
    },
    error = function(e) {
      stop("Failed to download or read data from GCS: ", e$message)
    }
  )
}


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
