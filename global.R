# global.R
library(googleCloudStorageR)

# -----------------------------
# Authenticate to GCS
# -----------------------------
authenticate_gcs <- function() {
  # Read raw JSON string from environment variable
  json_text <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
  if (nchar(json_text) == 0) stop("GCS_SERVICE_ACCOUNT_JSON environment variable not set!")
  
  # Convert escaped \n to real newlines if needed
  json_text <- gsub("\\\\n", "\n", json_text)
  
  # Authenticate directly with JSON string (DO NOT use fromJSON)
  tryCatch({
    gcs_auth(json_text)
    message("Authenticated to GCS successfully")
  }, error = function(e) {
    stop("GCS authentication failed: ", e$message)
  })
}

# -----------------------------
# Load data from GCS
# -----------------------------
load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".rds")
  tryCatch({
    # Optional: list objects in bucket for debug
    objs <- gcs_list_objects("shiny-data")
    message("Objects in bucket: ", paste(objs$name, collapse = ", "))
    
    # Download the RDS object
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
