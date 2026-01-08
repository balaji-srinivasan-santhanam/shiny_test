library(googleCloudStorageR)
library(base64enc)

authenticate_gcs <- function() {
  # 1. Get the encoded string from Posit Connect Cloud environment variables
  encoded_key <- Sys.getenv("GCS_AUTH_BASE64")
  
  if (encoded_key == "") {
    stop("Environment variable GCS_AUTH_BASE64 is not set.")
  }
  
  # 2. Decode the string and write to a temporary file
  # This file only exists while the app instance is running
  tmp_key_file <- tempfile(fileext = ".json")
  
  tryCatch({
    decoded_bytes <- base64decode(encoded_key)
    writeBin(decoded_bytes, tmp_key_file)
  }, error = function(e) {
    stop("Failed to decode Base64 string. Ensure the secret is copied correctly.")
  })
  
  # 3. Point Google to the temporary file
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = normalizePath(tmp_key_file))
  
  # 4. Authenticate
  gcs_auth() 
  
  message("Authenticated to GCS successfully using decoded Base64 key")
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