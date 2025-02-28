## Function for queries to mybb
box::use(
  config,
  DBI,
  glue,
  RMySQL,
)

sqlQuery <- function(query, db) {
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

  res
}

#' Function for queries to mybb
#' @export
mybbQuery <- function(query) {
  sqlQuery(query, "mybb")
}

#' Function for queries to portal
#' @export
portalQuery <- function(query) {
  sqlQuery(query, "portal")
}

#' Function for queries to index
#' @export
indexQuery <- function(query) {
  sqlQuery(query, "index")
}

#' Function for queries to budget
#' @export
budgetQuery <- function(query) {
  sqlQuery(query, "budget")
}
