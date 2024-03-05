
## Adds update to the update log
updateLog <- function(uid, pid, updates){
  portalQuery(
    paste(
      "INSERT INTO updatehistory ( uid, pid, time, attribute, old, new )
                VALUES
                    ",
      paste(
        "(",
        paste(
          uid,
          pid,
          paste0("'", lubridate::now() ,"'"),
          paste0("'", updates$attribute, "'"),
          updates$old,
          updates$new,
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
updateBuild <- function(pid, updates){
  portalQuery(
    paste(
      "UPDATE playerdata
               SET",
      paste(
        paste("`", str_to_lower(updates$attribute), "`", sep = ""),
        "=",
        updates$new,
        collapse = ", "
      ),
      "WHERE pid =", pid
    )
  )
}

## Gets update history
getUpdateHistory <- function(pid){
  portalQuery(
    paste("SELECT * FROM updatehistory WHERE pid = ", pid, "ORDER BY time DESC")
  )
}

# updateCheck <- function(tpe, updates)

