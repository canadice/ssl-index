## Gets TPE history from the log
getTpeHistory <- function(pid){
  portalQuery(
    query = 
      "SELECT Time, Username, Source, `TPE Change`
      FROM tpehistoryview
      WHERE pid = ?pid
      ORDER BY time DESC;",
    pid = pid
  ) %>% 
    mutate(
      Time = Time %>% as.numeric() %>% as_datetime(tz = "US/Pacific")
    )
}

completedActivityCheck <- function(pid) {
  # Get the start of the current week (Monday) in US/Pacific as a numeric value.
  weekStart <- lubridate::now() %>% 
    with_tz("US/Pacific") %>% 
    floor_date("week", week_start = "Monday") %>% 
    as.numeric()
  
  # Execute a parameterized query to retrieve any matching records.
  res <- portalQuery(
    query = "SELECT * FROM tpehistory 
             WHERE pid = ?pid 
               AND source LIKE ?pattern 
               AND time > ?weekStart;",
    pid = pid,
    pattern = "%Activity Check",
    weekStart = weekStart,
    type = "get"
  )
  
  # Return TRUE if one or more rows were found.
  nrow(res) > 0
}


completedTrainingCamp <- function(pid) {
  # Calculate the season start as a numeric timestamp
  seasonStart <- (currentSeason$startDate %>% 
                    as_date() %>% 
                    force_tz("US/Pacific")) %>% 
    as.numeric()
  
  # Execute the parameterized query
  res <- portalQuery(
    query = "SELECT * FROM tpehistory 
             WHERE pid = ?pid 
               AND source LIKE ?pattern 
               AND time > ?seasonStart;",
    pid = pid,
    pattern = "%Training Camp",
    seasonStart = seasonStart,
    type = "get"
  )
  
  # Return TRUE if one or more rows are found
  nrow(res) > 0
}


tpeLog <- function(uid, pid, tpe) {
  currentTime <- lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric()
  
  portalQuery(
    query = "INSERT INTO tpehistory (uid, pid, time, source, tpe)
             VALUES (?uid, ?pid, ?time, ?source, ?tpe);",
    uid    = uid,
    pid    = pid,
    time   = currentTime,
    source = tpe$source,
    tpe    = tpe$tpe,
    type   = "set"
  )
}


## Updates build on player
updateTPE <- function(pid, tpe) {
  portalQuery(
    query = "UPDATE playerdata 
             SET tpe = tpe + ?amount, 
                 tpebank = tpebank + ?amount 
             WHERE pid = ?pid;",
    amount = tpe$tpe,
    pid = pid,
    type = "set"
  )
}

