
getPlayerData <- function(uid = NULL, pid = NULL){
  if(pid %>% is.null()){
    portalQuery(
      paste(
        "SELECT *
      FROM playerdata
      WHERE uid =", uid
      )
      # ORDER BY pid DESC
      
      ## NEED TO ADD SOMETHING THAT ONLY TAKES BACK ONE PLAYER IF SOMEONE HAS RECREATED
    )
  } else {
    portalQuery(
      paste(
        "SELECT *
      FROM playerdata
      WHERE pid = ", pid
      )
    )
  }
}

getPlayerDataAsync <- function(uid = NULL, pid = NULL){
  future_promise(
    if(pid %>% is.null()){
      portalQuery(
        paste(
          "SELECT *
        FROM playerdata
        WHERE uid =", uid
        )
        # ORDER BY pid DESC
        
        ## NEED TO ADD SOMETHING THAT ONLY TAKES BACK ONE PLAYER IF SOMEONE HAS RECREATED
      )
    } else {
      portalQuery(
        paste(
          "SELECT *
        FROM playerdata
        WHERE pid = ", pid
        )
      )
    }
  )
}












