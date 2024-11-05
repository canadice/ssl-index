## DON'T NEED TO BE EXPOSED TO API
topEarners <- function(){
  readAPI(url = "https://api.simulationsoccer.com/player/topEarners") %>% 
    future_promise()
}