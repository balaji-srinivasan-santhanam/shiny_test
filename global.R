# global.R
library(googleCloudStorageR)
library(jsonlite)

# -----------------------------
# Function: Authenticate to GCS
# -----------------------------
authenticate_gcs <- function() {
  json_env <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
  if (nchar(json_env) == 0) stop("GCS_SERVICE_ACCOUNT_JSON not set!")
  
  # Convert escaped \n back to actual newlines
  json_text <- gsub("\\\\n", "\n", json_env)
  
  # Parse JSON
  sa <- tryCatch({
    gcs_auth(json_text)
  }, error = function(e) {
    stop("Failed to parse JSON: ", e$message)
  })
  
  # Authenticate with GCS
  tryCatch({
    gcs_auth(sa)
    message("Authenticated to GCS successfully")
  }, error = function(e) {
    stop("GCS authentication failed: ", e$message)
  })
}

# -----------------------------
# Function: Load data from GCS
# -----------------------------
load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".rds")
  tryCatch({
    # Optional: list objects for debugging
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
  }, error = function(e) {
    stop("Failed to download or read data from GCS: ", e$message)
  })
}
