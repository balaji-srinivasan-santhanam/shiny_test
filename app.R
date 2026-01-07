# global.R

library(shiny)
library(googleCloudStorageR)
library(jsonlite)

# ---- Step 1: Read service account JSON from environment variable ----
gcs_json <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")

# ---- Step 2: Check if env var exists ----
if (gcs_json == "") {
  stop(
    "GCS_SERVICE_ACCOUNT_JSON environment variable is not set. ",
    "Please set it in Posit Connect Cloud App Settings."
  )
}

# ---- Step 3: Parse JSON safely ----
# Use tryCatch to catch malformed JSON
sa <- tryCatch(
  fromJSON(txt = gcs_json),
  error = function(e) {
    stop("Failed to parse GCS_SERVICE_ACCOUNT_JSON: ", e$message)
  }
)

# ---- Step 4: Authenticate to GCS ----
gcs_auth(sa)

# ---- Step 5: Load data from GCS ----
load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".rds")
  gcs_get_object(
    object_name = "data/mydata.rds",
    bucket = "my-shiny-data",
    saveToDisk = tmp,
    overwrite = TRUE
  )
  readRDS(tmp)
}

# ---- Step 6: Cache in memory to avoid repeated downloads ----
DATA <- load_data_from_gcs()
