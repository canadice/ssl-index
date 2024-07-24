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
          "SELECT pd.*, mb.username AS username, us.desc AS `userStatus`, ps.desc AS `playerStatus`
        FROM playerdata pd
        LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
        LEFT JOIN useractivity ua ON pd.uid = ua.uid
        LEFT JOIN userstatuses us ON ua.status_u = us.status
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
        WHERE pd.pid = ", pid,";"
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

getPlayerNameFromUsername <- function(username){
  portalQuery(
    paste(
      "SELECT pd.pid, pd.name
        FROM playerdata AS pd
        JOIN mybbdb.mybb_users AS mbb ON pd.uid = mbb.uid
        WHERE mbb.username = ", paste0("'", username, "'"), " AND pd.status_p = 1;"
    )
  ) %>% 
    future_promise()
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
  }) %>% 
    suppressWarnings()
}

getPlayerID <- function(name){
  portalQuery(
    paste(
      "SELECT pid
        FROM playerdata
        WHERE name = '", name %>% str_replace_all(pattern = "'", replacement = "\\\\'"), "' AND status_p = 1;",
      sep = ""
    )
  )
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
        pattern = traitSep
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

getAllPlayerPositions <- function(){
  future_promise({
    portalQuery(
      paste(
        "SELECT ua.status_u AS status, pd.pos_st, pd.pos_lam, pd.pos_cam, pd.pos_ram, pd.pos_lm, pd.pos_cm, pd.pos_rm, 
        pd.pos_lwb, pd.pos_cdm, pd.pos_rwb, pd.pos_ld, pd.pos_cd, pd.pos_rd, pd.pos_gk
        FROM playerdata pd
        JOIN useractivity ua ON pd.uid = ua.uid
        WHERE pd.status_p = 1;"
      )
    ) 
  })
}

getPlayerFootedness <- function(pid){
  future_promise({
    portalQuery(
      paste(
        "SELECT `left foot`, `right foot`
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
        "SELECT us.desc AS `user status`, ps.desc AS `player status`, pd.name, pd.class, pd.tpe, pd.tpebank, `left foot`, `right foot`, pd.position, (CASE WHEN teams.affiliate = 2 THEN 'Minor' ELSE 'Major' END) AS affiliate, pid
      FROM playerdata pd
      JOIN useractivity ua ON pd.uid = ua.uid
      JOIN userstatuses us ON ua.status_u = us.status
      JOIN playerstatuses ps ON pd.status_p = ps.status
      JOIN teams ON pd.team = teams.orgID AND pd.affiliate = teams.affiliate
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
    ) %>% 
      arrange(affiliate, tpe %>% desc())
  })
}

getPlayersFromAllTeams <- function(){
  future_promise({
    portalQuery(
      paste(
        "SELECT pd.name, pd.class, pd.tpe, pd.tpebank, pd.position, teams.name AS organization, (CASE WHEN teams.affiliate = 2 THEN 'Minor' ELSE 'Major' END) AS affiliate, pid
      FROM playerdata AS pd
      LEFT JOIN teams ON pd.team = teams.orgID AND pd.affiliate = teams.affiliate
      WHERE status_p > 0 AND team > -3;
      "
      )
    )
  })
}

getPlayerNames <- function(){
  future_promise({
    portalQuery(
      paste(
        "SELECT pd.name, pd.pid, mb.username, pd.team, pd.status_p
      FROM playerdata pd
      LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid;
      "
      )
    )
  })
}

getRecentCreates <- function(){
  portalQuery(
    paste(
      "SELECT 
            pd.position AS Pos,
            pd.name AS Name,
            mbb.username AS Username
        FROM 
            playerdata pd
        LEFT JOIN
            mybbdb.mybb_users mbb ON pd.uid = mbb.uid
        WHERE 
            pd.status_p > 0
        ORDER BY 
            created DESC
        LIMIT 5;"
    )
  ) %>% 
    future_promise()
}

getChangedBuilds <- function(){
  ## Gets date of the start of the week in Pacific
  weekEnd <- 
    lubridate::now() %>% 
    with_tz("US/Pacific") %>% 
    floor_date("week", week_start = "Monday") %>% 
    as.numeric() %>% 
    {. - 1}
  
  weekStart <- 
    lubridate::now() %>% 
    with_tz("US/Pacific") %>% 
    floor_date("week", week_start = "Monday") %>% 
    as.numeric() %>% 
    {. - 604800}
  
  portalQuery(
    paste(
      "SELECT DISTINCT pd.*
        FROM playerdata pd
        JOIN updatehistory uh ON pd.pid = uh.pid
        WHERE uh.Time < ", weekEnd, " AND uh.Time > ", weekStart," AND uh.uid <> 1;"
    )
  ) %>% 
    future_promise()
}

getPlayerStatus <- function(pid){
  future_promise({
    portalQuery(
      paste(
        "SELECT status.desc
        FROM playerdata pd
        JOIN playerstatuses status ON pd.status_p = status.status
        WHERE pd.pid =", pid, ";"
      )
    )
  })
}













