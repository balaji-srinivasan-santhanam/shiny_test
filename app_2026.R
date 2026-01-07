server <- function(input, output, session) {
  DATA <- reactiveVal(NULL)
  
  observe({
    tryCatch({
      authenticate_gcs()
      DATA(load_data_from_gcs())
    }, error = function(e) {
      DATA(NULL)
      message("ERROR loading DATA: ", e$message)
    })
  })
  
  # ...
}
