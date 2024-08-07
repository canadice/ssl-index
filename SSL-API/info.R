#* Checks the R and package versions
#* @get /status
function(req, res) {
  res$setHeader("Content-Type", "application/json")
  res$status <- 200L
  res$body <- jsonlite::toJSON(
    # sessioninfo::session_info(), auto_unbox = TRUE, null = "null",
    # Sys.getenv("HOME")
    # portalQuery(paste("SELECT pid FROM playerdata;"))
    # getwd()
  )
  res
}

# Programmatically alter your API
#* @plumber
function(pr) {
  
  log_error <- function(message) {
    log_file <- "error.log"
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", message, "\n", file = log_file, append = TRUE)
  }
  
  handler_error <- function(req, res, err){
    log_error(paste("Error in request to", req$PATH_INFO, ":", err$message))
    res$status <- 500
    list(error = paste("Custom Error Message", err))
  }
  
  
  pr %>%
    # Overwrite the default serializer to return unboxed JSON
    pr_set_serializer(serializer_unboxed_json()) %>% 
    pr_set_debug(TRUE) %>%
    pr_get("/error", function() log("a")) %>% 
    pr_set_error(handler_error)
}
