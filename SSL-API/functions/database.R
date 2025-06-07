## Loads config information for database information
config <- config::get(config = "mysql")

query <- function(query, ..., schema){
  
  tryCatch({
    con <- 
      dbConnect(
        MySQL(),
        dbname = schema,
        host = config$mysql$host,
        port = config$mysql$port,
        user = config$mysql$user,
        password = config$mysql$pass
      )
    
    dbSendQuery(con, "SET NAMES utf8mb4;")
    dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
    dbSendQuery(con, "SET character_set_connection=utf8mb4;")
    
    # req <- glue::glue_sql(query, .con = con)
    
    safeQuery <- sqlInterpolate(con, query, ...)
    
    # print(safeQuery)
    
    req <- dbGetQuery(con, safeQuery)
    # req <- dbSendQuery(con, req)
    # res <- dbFetch(req, n = -1)
    
    # dbClearResult(req)
    
    return(req)
  }, error = function(e) {
    # Log or handle the error
    message("Error executing query: ", e$message)
    return(NULL)
  }, finally = {
    # Ensure the connection is closed
    if (!is.null(con) && dbIsValid(con)) {
      dbDisconnect(con)
    }
  })
}

## Function for queries to mybb
mybbQuery <- function(query, ...){
  
  query(query, ..., schema = config$mysql$mybb)
  
}

## Function for queries to portal
portalQuery <- function(query, ...){
  
  query(query, ..., schema = config$mysql$portal)
  
}


## Function for queries to index
indexQuery <- function(query, ...){
  
  query(query, ..., schema = config$mysql$index)
  
}

budgetQuery <- function(query, ...){
  
  query(query, ..., schema = config$mysql$budget)
  
}

