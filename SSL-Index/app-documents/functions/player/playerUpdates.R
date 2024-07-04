
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
          paste0("'", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), "'"),
          paste0("'", updates$attribute %>% str_to_upper(), "'"),
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
updateBuild <- function(pid, updates, bank){
  portalQuery(
    paste(
      "UPDATE playerdata
               SET",
      paste(
        paste("`", str_to_lower(updates$attribute), "`", sep = ""),
        "=",
        updates$new,
        collapse = ", "
      ), ", tpebank = ", bank,
      "WHERE pid =", pid
    )
  )
}

completeRedistribution <- function(pid){
  portalQuery(
    paste(
      "UPDATE playerdata
      SET redistused = 1 
      WHERE pid =", pid
    )
  )
}

completeReroll <- function(pid){
  portalQuery(
    paste(
      "UPDATE playerdata
      SET rerollused = 1 
      WHERE pid =", pid
    )
  )
}

completeRetirement <- function(pid){
  portalQuery(
    paste(
      "UPDATE playerdata
      SET status_p = 2 
      WHERE pid =", pid
    )
  )
}

## Gets update history
getUpdateHistory <- function(pid){
  portalQuery(
    paste("SELECT 
            uh.time AS Time,
            mbb.username AS Username,
            uh.attribute AS `Changed attribute`,
            uh.old AS `From`,
            uh.new AS `To`
        FROM 
            updatehistory uh
        LEFT JOIN
            mybbdb.mybb_users mbb ON uh.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY Time DESC")
  ) %>% 
    mutate(
      Time = Time %>% as.numeric() %>% as_datetime(tz = "US/Pacific")
    )
}

## Summarizes updated attributes in a tibble
updateSummary <- function(current, inputs){
  updates <- 
    tibble(
      attribute = 
        current %>% 
        select(acceleration:throwing) %>% 
        # select(!where(is.na)) %>% 
        colnames() %>%
        str_to_title(),
      old = current %>% 
        select(acceleration:throwing) %>% 
        # select(!where(is.na)) %>% 
        t() %>% 
        c(),
      new = 
        attribute %>%
        str_remove_all(pattern = " ") %>% 
        sapply(
          X = .,
          FUN = function(x) {
            if(inputs[[x]] %>% is.null()){
              5
            } else {
              inputs[[x]]
            }
          },
          simplify = TRUE
        ) %>% 
        unlist()
    ) %>% 
    mutate(
      old = if_else(old %>% is.na(), 5, old)
    ) %>% 
      filter(old != new) 
  
  
  return(updates)
}

# updateCheck <- function(tpe, updates)

