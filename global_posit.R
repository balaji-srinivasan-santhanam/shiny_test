

#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#options(repos = BiocManager::repositories())
library('shiny')
library('ggplot2')
library('ggfortify')
library('googleCloudStorageR')
library('base64enc')
library('data.table')
library('grid') ; library('gridExtra')
library('survminer')
library('foreach')
library('doMC')
library("RColorBrewer")
#library('viridis')
library('dplyr')
#library('readr')



# --- GCS AUTHENTICATION HELPER ---
authenticate_gcs <- function() {
  encoded_key <- Sys.getenv("GCS_AUTH_BASE64")
  if (encoded_key == "") stop("Environment variable GCS_AUTH_BASE64 not set.")
  
  tmp_key <- tempfile(fileext = ".json")
  writeBin(base64decode(encoded_key), tmp_key)
  gcs_auth(json_file = tmp_key)
}
gcs_global_bucket("shiny-data")
authenticate_gcs()

get_gcs_rds <- function(path) {
  tmp <- tempfile(fileext = ".RDS")
  gcs_get_object(path, saveToDisk = tmp)
  readRDS(tmp)
}

module_rbps <- get_gcs_rds("data/module_rbps_slim.RDS")
common_genes <- get_gcs_rds("data/common_genes_quant_0.1")


