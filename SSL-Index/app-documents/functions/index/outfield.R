getOutfieldIndex <- function(league, season){
  readAPI(url = "https://api.simulationsoccer.com/index/outfield", query = list(league = league, season = season))  %>% future_promise()
}

getOutfieldCareer <- function(league){
  readAPI(url = "https://api.simulationsoccer.com/index/outfield", query = list(league = league, season = "ALL"))  %>% future_promise()
}

getOutfieldMatchStats <- function(name){
  future_promise({
    readAPI(url = "https://api.simulationsoccer.com/index/outfieldGameByGame", query = list(name = name)) %>%
      select(1:10) %>% 
      slice_head(n = 5)
  })
}