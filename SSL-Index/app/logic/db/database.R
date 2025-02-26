## Function for queries to mybb
box::use(
  config,
  DBI,
  RMySQL,
  glue
)

sqlQuery <- function(db){
  con <- 
    DBI$dbConnect(
      RMySQL$MySQL(),
      dbname = config$get(config = "mysql", db),
      host = config$get(config = "mysql", "host"),
      port = config$get(config = "mysql", "port"),
      user = config$get(config = "mysql", "user"),
      password = config$get(config = "mysql", "pass")
    )
  
  DBI$dbSendQuery(con, "SET NAMES utf8mb4;")
  DBI$dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
  DBI$dbSendQuery(con, "SET character_set_connection=utf8mb4;")
  
  req <- glue$glue_sql(query, .con = con)
  
  req <- DBI$dbSendQuery(con, req)
  res <- DBI$dbFetch(req, n = -1)
  
  DBI$dbClearResult(req)
  
  DBI$dbDisconnect(con)
  
  return(res)
}

#' Function for queries to mybb
#' @export
mybbQuery <- function(query){
  sqlQuery("mybb")
}

#' Function for queries to portal
#' @export
portalQuery <- function(query){
  sqlQuery("portal")
}

#' Function for queries to index
#' @export
indexQuery <- function(query){
  sqlQuery("index")
}

#' Function for queries to budget
#' @export
budgetQuery <- function(query){
  sqlQuery("budget")
}

