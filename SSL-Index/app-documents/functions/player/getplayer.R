# 
# getPlayerData <- function(uid = NULL, pid = NULL){
#   if(pid %>% is.null()){
#     portalQuery(
#       paste(
#         "SELECT *
#       FROM playerdata
#       WHERE uid =", uid, " AND status_p = 1",
#         "ORDER BY pid DESC LIMIT 1;"
#       )
#       # ORDER BY pid DESC
#       
#       ## NEED TO ADD SOMETHING THAT ONLY TAKES BACK ONE PLAYER IF SOMEONE HAS RECREATED
#     )
#   } else {
#     portalQuery(
#       paste(
#         "SELECT *
#       FROM playerdata
#       WHERE pid = ", pid
#       )
#     )
#   }
# }

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

getPlayerName <- function(uid = NULL, pid = NULL){
  future_promise({
    if(pid %>% is.null()){
      portalQuery(
        paste(
          "SELECT pid, name, class
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
          "SELECT pid, name, class
        FROM playerdata
        WHERE pid = ", pid, " AND status_p = 1"
        )
      )
    }
  })
}

getPlayerTraits <- function(pid){
  future_promise({
    portalQuery(
      paste(
        "SELECT traits
        FROM playerdata
        WHERE pid =", pid, ";"
      )
    ) %>% 
      str_split(
        pattern = " \\\\ "
      ) %>% 
      unlist()
  })
}

getPlayerPositions <- function(pid){
  future_promise({
    portalQuery(
      paste(
        "SELECT pos_st, pos_lam, pos_cam, pos_ram, pos_lm, pos_cm, pos_rm, pos_lwb, pos_cdm, pos_rwb, pos_ld, pos_cd, pos_rd, pos_gk
        FROM playerdata
        WHERE pid =", pid, ";"
      )
    ) %>% 
      pivot_longer(everything()) %>% 
      mutate(
        name = str_remove(name, pattern = "pos_") %>% str_to_upper()
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
        SELECT name
        FROM teams
        WHERE orgID IN (
          SELECT orgID
        FROM managers
        WHERE orgManager = ", uid, " OR assManager1 = ", uid, "OR assManager2 = ", uid, "
        )
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

getRecentCreates <- function(){
  portalQuery(
    paste(
      "SELECT 
            pd.name AS Name,
            mbb.username AS Username
        FROM 
            playerData pd
        LEFT JOIN
            mybbdb.mybb_users mbb ON pd.uid = mbb.uid
        WHERE 
            pd.status_p = 1
        ORDER BY 
            created DESC
        LIMIT 5;"
    )
  ) %>% 
    future_promise()
}









