library(shiny)
library(jsonlite)
library(googleCloudStorageR)

cat("Starting global.R\n")

gcs_json <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
cat("GCS_SERVICE_ACCOUNT_JSON length:", nchar(gcs_json), "\n")

if (gcs_json == "") stop("GCS_SERVICE_ACCOUNT_JSON env var missing!")

sa <- tryCatch(
  fromJSON(txt = gcs_json),
  error = function(e) {
    stop("Failed to parse JSON: ", e$message)
  }
)

cat("JSON parsed successfully\n")

gcs_auth(sa)
cat("Authenticated to GCS successfully\n")
