# global.R
library(googleCloudStorageR)
library(jsonlite)

authenticate_gcs <- function() {
  gcs_json <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
  
  if (nchar(gcs_json) == 0) stop("GCS_SERVICE_ACCOUNT_JSON not set!")
  
  # Use readChar trick to handle multiline private_key
  sa <- tryCatch({
    jsonlite::fromJSON(gcs_json)
  }, error = function(e) {
    stop("Failed to parse GCS_SERVICE_ACCOUNT_JSON: ", e$message)
  })
  
  tryCatch({
    gcs_auth(sa)
    message("Authenticated to GCS successfully")
  }, error = function(e) {
    stop("GCS authentication failed: ", e$message)
  })
}

# Function to load data
load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".rds")
  gcs_get_object(
    object_name = "example_data/mydata.RDS",
    bucket = "shiny-data",
    saveToDisk = tmp,
    overwrite = TRUE
  )
  readRDS(tmp)
}
