library(shiny)
library(jsonlite)
library(googleCloudStorageR)

message("---- global.R startup ----")

gcs_json <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
message("Length of GCS_SERVICE_ACCOUNT_JSON: ", nchar(gcs_json))

if (gcs_json == "") stop("GCS_SERVICE_ACCOUNT_JSON environment variable is not set!")

sa <- tryCatch(
  fromJSON(txt = gcs_json),
  error = function(e) {
    stop("Failed to parse JSON: ", e$message)
  }
)

tryCatch(
  {
    gcs_auth(sa)
    message("Authenticated to GCS successfully")
  },
  error = function(e) {
    stop("Failed to authenticate to GCS: ", e$message)
  }
)

load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".rds")
  tryCatch(
    {
      gcs_get_object(
        object_name = "example_data/mydata.RDS",
        bucket = "shiny-data",
        saveToDisk = tmp,
        overwrite = TRUE
      )
      readRDS(tmp)
    },
    error = function(e) {
      stop("Failed to download/read data from GCS: ", e$message)
    }
  )
}

DATA <- tryCatch(
  load_data_from_gcs(),
  error = function(e) {
    message("WARNING: Failed to load DATA: ", e$message)
    NULL
  }
)

message("Global DATA object created: ", !is.null(DATA))
