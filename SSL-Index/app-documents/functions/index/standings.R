getStandings <- function(division, season){
  readAPI(url = "https://api.simulationsoccer.com/index/standings", query = list(league = division, season = season))
}


getSchedule <- function(season = currentSeason$season){
  readAPI(url = "https://api.simulationsoccer.com/index/schedule", query = list(season = season))
}
