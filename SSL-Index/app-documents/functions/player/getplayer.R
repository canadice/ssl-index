
getPlayerData <- function(uid = NULL, pid = NULL){
  if(pid %>% is.null()){
    portalQuery(
      paste(
        "SELECT *
      FROM playerdata
      WHERE uid =", uid, " AND status_p = 1",
        "ORDER BY pid DESC LIMIT 1;"
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
  future_promise({
    if(pid %>% is.null()){
      portalQuery(
        paste(
          "SELECT *
        FROM playerdata
        WHERE uid =", uid, " AND status_p = 1",
          "ORDER BY pid DESC LIMIT 1;"
        )
        # ORDER BY pid DESC
        
        ## NEED TO ADD SOMETHING THAT ONLY TAKES BACK ONE PLAYER IF SOMEONE HAS RECREATED
      )
    } else {
      portalQuery(
        paste(
          "SELECT *
        FROM playerdata
        WHERE pid = ", pid, " AND status_p = 1"
        )
      )
    }
  })
}

hasActivePlayer <- function(uid){
  res <- 
    portalQuery(
      paste(
        "SELECT *
          FROM playerdata
          WHERE uid =", uid, " AND status_p = 1"
      )
    ) 
  
  nrow(res) > 0
}

getPlayerName <- function(pid){
  future_promise({
    portalQuery(
      paste(
        "SELECT name
          FROM playerdata
          WHERE pid =", pid, ";"
      )
    )
  })
}

getPlayerTeam <- function(pid){
  future_promise({
    portalQuery(
      paste(
        "SELECT team
          FROM playerdata
          WHERE pid =", pid, ";"
      )
    )
  })
}

getPlayersFromTeam <- function(uid){
  future_promise({
    portalQuery(
      paste(
        "SELECT name, class, tpe, tpebank, footedness, position, affiliate, pid
      FROM playerdata
      WHERE team IN (
        SELECT team
        FROM playerdata
        WHERE uid = ", uid, " AND status_p = 1
      )"
      )
    )
  })
}

getPlayersFromAllTeams <- function(){
  future_promise({
    portalQuery(
      paste(
        "SELECT name, class, tpe, tpebank, footedness, position, team, affiliate, pid
      FROM playerdata
      WHERE status_p = 1 AND team NOT IN ('FA', 'Prospect');
      "
      )
    )
  })
}










