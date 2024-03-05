## Gets TPE history from the log
getTpeHistory <- function(pid){
  portalQuery(
    paste("SELECT * FROM tpehistory WHERE pid = ", pid, "ORDER BY time DESC")
  )
}

## Adds tpe updates to the log
tpeLog <- function(uid, pid, tpe){
  portalQuery(
    paste(
      "INSERT INTO tpehistory ( uid, pid, time, source, tpe )
                VALUES
                    ",
      paste(
        "(",
        paste(
          uid,
          pid,
          paste0("'", lubridate::now() ,"'"),
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
      "WHERE pid =", pid
    )
  )
}
