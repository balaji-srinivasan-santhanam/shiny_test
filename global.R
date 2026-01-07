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
  tmp <- tempfile(fileext = ".rds")
  tryCatch(
    {
      gcs_get_object(
        object_name = "data/mydata.rds",  # adjust path in your bucket
        bucket = "my-shiny-data",         # replace with your bucket
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
