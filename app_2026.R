# global.R
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
  
  # Parse JSON
  sa <- tryCatch(
    fromJSON(gcs_json),
    error = function(e) {
      stop("Failed to parse GCS_SERVICE_ACCOUNT_JSON: ", e$message)
    }
  )
  
  # Authenticate
  tryCatch(
    {
      gcs_auth(sa)
      message("Authenticated to GCS successfully")
    },
    error = function(e) {
      stop("GCS authentication failed: ", e$message)
    }
  )
}

# -----------------------------
# Function: Load data from GCS
# -----------------------------
load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".rds")
  tryCatch(
    {
      # List objects for debug
      objs <- gcs_list_objects("shiny-data")
      message("Objects in bucket: ", paste(objs$name, collapse = ", "))
      
      # Download object
      gcs_get_object(
        object_name = "example_data/mydata.RDS",
        bucket = "shiny-data",
        saveToDisk = tmp,
        overwrite = TRUE
      )
      message("Downloaded object successfully")
      readRDS(tmp)
    },
    error = function(e) {
      stop("Failed to download or read data from GCS: ", e$message)
    }
  )
}


# app.R
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Shiny App Reading from GCS"),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  
  DATA <- reactiveVal(NULL)
  
  # Authenticate and load data once at startup
  observe({
    tryCatch({
      authenticate_gcs()           # authenticate
      DATA(load_data_from_gcs())   # load data
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

shinyApp(ui, server)
