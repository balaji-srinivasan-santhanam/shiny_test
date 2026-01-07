library(googleCloudStorageR)
library(jsonlite)

# Function to authenticate to GCS
authenticate_gcs <- function() {
  gcs_json <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
  if (nchar(gcs_json) == 0) stop("GCS_SERVICE_ACCOUNT_JSON not set!")
  sa <- fromJSON(gcs_json)
  gcs_auth(sa)
}

# Function to load data from GCS
load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".RDS")
  gcs_get_object(
    object_name = "example_data/mydata.RDS",  # adjust as needed
    bucket = "shiny-data",
    saveToDisk = tmp,
    overwrite = TRUE
  )
  readRDS(tmp)
}
