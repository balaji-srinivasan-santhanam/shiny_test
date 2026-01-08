# global.R
library(googleCloudStorageR)
library(base64enc)


auth_env <- Sys.getenv("GCS_AUTH_BASE64")

if (auth_env != "") {
  # 1. Decode the Base64 string back to raw bytes
  decoded_bytes <- base64decode(auth_env)
  
  # 2. Write bytes to a temporary file
  tmp_json <- tempfile(fileext = ".json")
  writeBin(decoded_bytes, tmp_json)
  
  # 3. Authenticate using the temp file
  gcs_auth(json_file = tmp_json)
  
  # Clean up the temp file after auth
  on.exit(unlink(tmp_json), add = TRUE)
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
