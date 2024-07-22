getStandings <- function(division, season){
  
  indexQuery(
    paste(
      "SELECT
    Team,
    COUNT(*) AS MatchesPlayed,
    SUM(CASE
        WHEN (Home = Team AND HomeScore > AwayScore) OR (Away = Team AND AwayScore > HomeScore) THEN 1
        ELSE 0
    END) AS Wins,
    SUM(CASE
        WHEN (HomeScore = AwayScore) THEN 1
        ELSE 0
    END) AS Draws,
    SUM(CASE
        WHEN (Home = Team AND HomeScore < AwayScore) OR (Away = Team AND AwayScore < HomeScore) THEN 1
        ELSE 0
    END) AS Losses,
    SUM(CASE
        WHEN (Home = Team) THEN HomeScore
        ELSE AwayScore
    END) AS GoalsFor,
    SUM(CASE
        WHEN (Home = Team) THEN AwayScore
        ELSE HomeScore
    END) AS GoalsAgainst,
    SUM(CASE
        WHEN (Home = Team AND HomeScore > AwayScore) OR (Away = Team AND AwayScore > HomeScore) THEN 3
        WHEN (HomeScore = AwayScore) THEN 1
        ELSE 0
    END) AS Points
FROM (
    SELECT Home AS Team, Home, Away, HomeScore, AwayScore FROM schedule WHERE matchtype =", division, " AND Season = ", season, 
    "UNION ALL
    SELECT Away AS Team, Home, Away, HomeScore, AwayScore FROM schedule WHERE matchtype =", division, " AND Season = ", season,
") AS combined
GROUP BY Team
ORDER BY Points DESC, GoalsFor DESC, GoalsAgainst ASC;"
    )
  ) %>% 
    suppressWarnings()
}


getSchedule <- function(season = currentSeason$season){
  indexQuery(
    paste(
      "SELECT IRLDate, MatchType, MatchDay, Home, Away, HomeScore, AwayScore, ExtraTime, Penalties
      FROM schedule
      WHERE season = ", season,
      "ORDER BY IRLDate;"
    )
  )
}
