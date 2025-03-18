getKeeperIndex <- function(league, season){
  readAPI(url = "https://api.simulationsoccer.com/index/keeper", query = list(league = league, season = season)) %>% future_promise()
}

getKeeperCareer <- function(league){
  readAPI(url = "https://api.simulationsoccer.com/index/keeper", query = list(league = league, season = "ALL")) %>% future_promise()
}

getKeeperMatchStats <- function(name){
  future_promise({
    games <- 
      readAPI(url = "https://api.simulationsoccer.com/index/latestGames", query = list(name = name, outfield = FALSE))
    
      
    if(games %>% is_empty()){
      NULL
    } else{
      games
    }
  })
}