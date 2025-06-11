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

# getPlayerDataAsync <- function(uid = NULL, pid = NULL){
#   future_promise({
#     if(pid %>% is.null()){
#       portalQuery(
#         paste(
#           "SELECT *
#         FROM playerdata
#         WHERE uid =", uid, " AND status_p = 1",
#           "ORDER BY pid DESC LIMIT 1;"
#         )
#         # ORDER BY pid DESC
#         
#         ## NEED TO ADD SOMETHING THAT ONLY TAKES BACK ONE PLAYER IF SOMEONE HAS RECREATED
#       )
#     } else {
#       portalQuery(
#         paste(
#           "SELECT pd.*, mb.username AS username, us.desc AS `userStatus`, ps.desc AS `playerStatus`
#         FROM playerdata pd
#         LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
#         LEFT JOIN useractivity ua ON pd.uid = ua.uid
#         LEFT JOIN userstatuses us ON ua.status_u = us.status
#         LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
#         WHERE pd.pid = ", pid,";"
#         )
#       )
#     }
#   })
# }

hasActivePlayer <- function(userID){
  res <- 
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>% 
    filter(uid == userID, status_p == 1)
  
  nrow(res) > 0
}

getPlayerNameFromUsername <- function(userName){
  readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>%
    filter(username == userName, status_p == 1) %>% 
    select(pid, name) %>% 
    future_promise()
}

getPlayerName <- function(userID = NULL, playerID = NULL){
  future_promise({
    if(playerID %>% is.null()){
      readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>%
        filter(uid == userID, status_p == 1) %>% 
        select(pid, name, class, purchasedTPE) %>% 
        arrange(pid %>% desc()) %>% 
        slice_head()
        
    } else {
      readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>%
        filter(pid == playerID) %>% 
        select(pid, name, class, purchasedTPE)
      
    }
  }) %>% 
    suppressWarnings()
}

getPlayerID <- function(playerName){
  readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>%
    filter(name == playerName) %>% 
    select(pid)
}

getPlayerTraits <- function(playerID){
  future_promise({
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>%
      filter(pid == playerID) %>%
      select(traits) %>%
      str_split(
        pattern = traitSep
      ) %>%
      unlist() %>%
      {
        if(all(. == "")|all(. == "NO TRAITS")){
          NULL
        } else {
          .
        }
      }
  })
}

getPlayerPositions <- function(playerID){
  future_promise({
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>% 
      filter(pid == playerID) %>% 
      select(pos_st:pos_gk)%>% 
      pivot_longer(everything()) %>% 
      mutate(
        name = str_remove(name, pattern = "pos_") %>% str_to_upper()
      )
  })
}

getAllPlayerPositions <- function(){
  future_promise({
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>% 
      select(status = userStatus, pos_st:pos_gk)
  })
}

getPlayerFootedness <- function(playerID){
  future_promise({
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>% 
      filter(pid == playerID) %>% 
      select(`left foot`, `right foot`)
  })
}

getPlayerTeam <- function(playerID){
  future_promise({
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>% 
      filter(pid == playerID) %>% 
      select(team)
  })
}

getPlayersFromTeam <- function(uid){
  future_promise({
    readAPI("https://api.simulationsoccer.com/organization/getPlayersFromOrg", query = list(uid = uid))
  })
}

getPlayerNames <- function(){
  future_promise({
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "false")) %>% 
      select(name, pid, username, team, status_p)
    
  })
}

getRecentCreates <- function(){
  
  future_promise({
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>% 
      arrange(created %>% desc()) %>% 
      slice_head(n = 5) %>% 
      select(Pos = position, Name = name, Username = username) 
      
  })
  
}

getChangedBuilds <- function(){
  readAPI("https://api.simulationsoccer.com/player/getUpdatedBuilds") |> 
    future_promise()
}

getPlayerStatus <- function(playerID){
  future_promise({
    readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "false")) %>% 
      filter(pid == playerID) %>% 
      select(playerStatus)    
  })
}













