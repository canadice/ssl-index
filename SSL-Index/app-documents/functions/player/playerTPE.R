## Gets TPE history from the log
getTpeHistory <- function(pid){
  portalQuery(
    paste("SELECT 
            tpeh.time AS Time,
            mbb.username AS Username,
            tpeh.source AS Source,
            tpeh.tpe AS `TPE Change`
        FROM 
            tpehistory tpeh
        LEFT JOIN
            mybbdb.mybb_users mbb ON tpeh.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY time DESC")
  ) |> 
    mutate(
      Time = Time |> as.numeric() |> as_datetime(tz = "US/Pacific")
    )
}

completedActivityCheck <- function(pid){
  ## Gets date of the start of the week in Pacific
  weekStart <- 
    lubridate::now() |> 
    with_tz("US/Pacific") |> 
    floor_date("week", week_start = "Monday") |> 
    as.numeric()
  
  (portalQuery(
    paste("SELECT * FROM tpehistory WHERE pid = ", pid, " AND source LIKE '%Activity Check' AND time > ", weekStart)
  ) |> 
    nrow()) > 0
}

completedTrainingCamp <- function(pid){
  seasonStart <- 
    (currentSeason$startDate |> 
       as_date() |> 
       force_tz("US/Pacific")
    ) |> 
    as.numeric()
  
  (portalQuery(
    paste("SELECT * FROM tpehistory WHERE pid = ", pid, " AND source LIKE '%Training Camp' AND time > ", seasonStart)
  ) |> 
    nrow()) > 0
}

## Adds tpe updates to the log
tpeLog <- function(uid, pid, tpe){
  portalQuery(
  # print(
    paste(
      "INSERT INTO tpehistory ( uid, pid, time, source, tpe )
                VALUES
                    ",
      paste(
        "(",
        paste(
          uid,
          pid,
          paste0("'", lubridate::now() |> with_tz("US/Pacific") |> as.numeric(), "'"),
          paste0("'", tpe$source, "'"),
          tpe$tpe,
          sep = ","
        ),
        ")",
        collapse = ","
      ),
      ";"
    )
  )
}

## Updates build on player
updateTPE <- function(pid, tpe){
  portalQuery(
    paste(
      "UPDATE playerdata SET tpe = tpe + ", tpe$tpe, 
      ", tpebank = tpebank + ", tpe$tpe,
      "WHERE pid =", pid
    )
  )
}
