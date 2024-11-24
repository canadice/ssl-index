getKeeperIndex <- function(league, season){
  readAPI(url = "https://api.simulationsoccer.com/index/keeper", query = list(league = league, season = season)) %>% future_promise()
}

getKeeperCareer <- function(league){
  readAPI(url = "https://api.simulationsoccer.com/index/keeper", query = list(league = league, season = "ALL")) %>% future_promise()
}

getKeeperMatchStats <- function(name){
  future_promise({
    games <- 
      readAPI(url = "https://api.simulationsoccer.com/index/keeperGameByGame", query = list(name = name))
    
      
    if(games %>% is_empty()){
      NULL
    } else{
      games %>% 
        select(1:8) %>% 
        slice_head(n = 5)
    }
  })
}