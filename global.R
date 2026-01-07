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
)# global.R

library(shiny)
library(jsonlite)
library(googleCloudStorageR)

cat("---- global.R startup ----\n")

# Step 1: Read JSON from environment variable
gcs_json <- Sys.getenv("GCS_SERVICE_ACCOUNT_JSON")
cat("Length of GCS_SERVICE_ACCOUNT_JSON:", nchar(gcs_json), "\n")

if (gcs_json == "") {
  stop("GCS_SERVICE_ACCOUNT_JSON environment variable is not set in Connect Cloud!")
}

# Step 2: Parse JSON safely
sa <- tryCatch(
  fromJSON(txt = gcs_json),
  error = function(e) {
    stop("Failed to parse GCS_SERVICE_ACCOUNT_JSON: ", e$message)
  }
)
cat("Service account JSON parsed successfully\n")

# Step 3: Authenticate to GCS
tryCatch(
  {
    gcs_auth(sa)
    cat("Authenticated to GCS successfully\n")
  },
  error = function(e) {
    stop("Failed to authenticate to GCS: ", e$message)
  }
)

# Step 4: Function to load data from GCS
load_data_from_gcs <- function() {
  tmp <- tempfile(fileext = ".rds")
  tryCatch(
    {
      gcs_get_object(
        object_name = "data/mydata.rds",  # Update if your path differs
        bucket = "my-shiny-data",         # Update if your bucket differs
        saveToDisk = tmp,
        overwrite = TRUE
      )
      readRDS(tmp)
    },
    error = function(e) {
      stop("Failed to download or read data from GCS: ", e$message)
    }
  )
}

# Step 5: Load data into global variable
DATA <- tryCatch(
  load_data_from_gcs(),
  error = function(e) {
    cat("WARNING: Failed to load DATA: ", e$message, "\n")
    NULL
  }
)

cat("Global DATA object created:", !is.null(DATA), "\n")


cat("JSON parsed successfully\n")

gcs_auth(sa)
cat("Authenticated to GCS successfully\n")
