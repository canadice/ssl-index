getActiveTeams <- function(){
  indexQuery(
    paste(
      "SELECT * FROM teaminformation WHERE active = 1;"
    )
  ) %>% 
    future_promise()
}