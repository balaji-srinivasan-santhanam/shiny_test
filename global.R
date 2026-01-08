# global.R
library(googleCloudStorageR)

authenticate_gcs <- function() {
  json_text <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
  if (nchar(json_text) == 0) {
    stop("GCS_SERVICE_ACCOUNT_JSON not set")
  }
  
  # Write JSON exactly as-is to a temp file
  key_file <- tempfile(fileext = ".json")
  writeLines(json_text, key_file, useBytes = TRUE)
  
  # Authenticate using FILE (this is the critical fix)
  tryCatch({
    gcs_auth(key_file)
    message("Authenticated to GCS successfully")
  }, error = function(e) {
    stop("GCS authentication failed: ", e$message)
  })
}

load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".rds")
  
  gcs_get_object(
    bucket = "shiny-data",
    object_name = "example_data/mydata.RDS",
    saveToDisk = tmp,
    overwrite = TRUE
  )
  
  readRDS(tmp)
}
