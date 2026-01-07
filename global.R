library(googleCloudStorageR)
library(jsonlite)

authenticate_gcs <- function() {
  json_env <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
  if (nchar(json_env) == 0) stop("GCS_SERVICE_ACCOUNT_JSON not set!")
  
  # Convert escaped \n back to actual newlines
  json_text <- gsub("\\\\n", "\n", json_env)
  
  # Parse JSON
  sa <- tryCatch({
    fromJSON(json_text)
  }, error = function(e) {
    stop("Failed to parse JSON: ", e$message)
  })
  
  # Authenticate
  tryCatch({
    gcs_auth(sa)
    message("Authenticated to GCS successfully")
  }, error = function(e) {
    stop("GCS authentication failed: ", e$message)
  })
}

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
