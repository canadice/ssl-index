## Loads config information for database information
config <- config::get(config = "mysql")

## Function for queries to mybb
mybbQuery <- function(query){
  
  con <- 
    dbConnect(
      MySQL(),
      dbname = config$mysql$mybb,
      host = config$mysql$host,
      port = config$mysql$port,
      user = config$mysql$user,
      password = config$mysql$pass
    )
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
  dbSendQuery(con, "SET character_set_connection=utf8mb4;")
  
  req <- glue::glue_sql(query, .con = con)
  
  req <- dbSendQuery(con, req)
  res <- dbFetch(req, n = -1)
  
  dbClearResult(req)
  
  dbDisconnect(con)
  
  return(res)
  
}

## Function for queries to portal
portalQuery <- function(query){
  
  con <- 
    dbConnect(
      MySQL(),
      dbname = config$mysql$portal,
      host = config$mysql$host,
      port = config$mysql$port,
      user = config$mysql$user,
      password = config$mysql$pass
    )
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
  dbSendQuery(con, "SET character_set_connection=utf8mb4;")
  
  req <- glue::glue_sql(query, .con = con)
  
  # print(req)
  
  req <- dbSendQuery(con, req)
  res <- dbFetch(req, n = -1)
  
  dbClearResult(req)
  
  dbDisconnect(con)
  
  return(res)
  
}


## Function for queries to index
indexQuery <- function(query){
  
  con <- 
    dbConnect(
      MySQL(),
      dbname = config$mysql$index,
      host = config$mysql$host,
      port = config$mysql$port,
      user = config$mysql$user,
      password = config$mysql$pass,
      encoding = "UTF-8"
    )
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
  dbSendQuery(con, "SET character_set_connection=utf8mb4;")
  
  req <- glue::glue_sql(query, .con = con)
  
  # print(req)
  
  req <- dbSendQuery(con, req)
  res <- dbFetch(req, n = -1)
  
  dbClearResult(req)
  
  dbDisconnect(con)
  
  return(res)
  
}

