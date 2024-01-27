#### List Player Functions ####

#* Return the player data for a given user
#* @param username  The username of the user
#* @param player    The player name
#* @serializer json
#* @get /getPlayer
function(username = "", player = "") {
  if(username == "" & player == ""){
    res$status = 400  # the response object that is always available in plumber functions
    return("You have not sent in a username or player name.")
  }
  
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  player <- 
    tbl(con, "Daily_Scrape") %>% 
    filter(
      Username == username | Name == player
    ) %>% 
    filter(
      Created == max(Created, na.rm = TRUE)
    ) %>% 
    collect() %>% 
    mutate(
      Created = lubridate::as_date(Created)
    )
  
  dbDisconnect(con)
  
  return(player)
}

#* Return a list of players in the SSL
#* @serializer json
#* @get /listPlayers
function(retired = FALSE) {
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  players <- 
    tbl(con, "Daily_Scrape")
  
  if(retired){
    # DO NOTHING
  } else {
    players <- 
      players %>% 
      filter(
        Team != "Retired"
      )
  }
  
  players <- 
    players %>% 
    select(Username, Name) %>% 
    arrange(Name) %>% 
    collect() 
  
  dbDisconnect(con)
  
  playerNames <- as.list(players$Name)
  
  names(playerNames) <- players$Username
  
  return(playerNames %>% toJSON(named = TRUE))
}

#* Return player data of a given draft class
#* @param draftClass    The draft class
#* @serializer json
#* @get /getPlayers
function(draftClass = "ALL"){
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  players <- 
    tbl(con, "Daily_Scrape")
  
  if(draftClass != "ALL"){
    players <- 
      players %>% 
      filter(
        Class == draftClass
      )  
  }
  
  players <- 
    players %>% 
    select(
      Name,
      Username,
      Class,
      Team,
      `Preferred Position`,
      TPE,
      # `Applied TPE` = TPE - `TPE Available`,
      Active
    ) %>% 
    collect() 
  
  dbDisconnect(con)
  
  return(players)
}
