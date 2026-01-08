library(googleCloudStorageR)
library(base64enc)

authenticate_gcs <- function() {
  encoded_key <- Sys.getenv("GCS_AUTH_BASE64")
  
  if (encoded_key == "") {
    stop("Environment variable GCS_AUTH_BASE64 is not set.")
  }
  
  # Create the temp file
  tmp_key_file <- tempfile(fileext = ".json")
  
  decoded_bytes <- base64decode(encoded_key)
  writeBin(decoded_bytes, tmp_key_file)
  
  # CRITICAL CHANGE: 
  # Instead of just setting the Sys.setenv, pass the path directly to gcs_auth()
  googleCloudStorageR::gcs_auth(json_file = tmp_key_file)
  
  message("Authenticated to GCS successfully using Service Account")
  
  # Optional: check which service account is active in the logs
  # message("Active service account: ", googleCloudStorageR::gcs_get_service_email())
}

load_data_from_gcs <- function() {
  # Ensure we are authenticated before trying to download
  # (You might call authenticate_gcs() at the start of your app)
  
  tmp_rds <- tempfile(fileext = ".rds")
  
  gcs_get_object(
    bucket = "shiny-data",
    object_name = "example_data/mydata.RDS",
    saveToDisk = tmp_rds,
    overwrite = TRUE
  )
  
  readRDS(tmp_rds)
}


authenticate_gcs()
my_data <- load_data_from_gcs()