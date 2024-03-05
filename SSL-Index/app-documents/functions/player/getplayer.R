
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
    ) %>% 
      return()
  } else {
    portalQuery(
      paste(
        "SELECT *
        FROM playerdata
        WHERE pid = ", pid
      )
    ) %>% 
      return()
  }
}












