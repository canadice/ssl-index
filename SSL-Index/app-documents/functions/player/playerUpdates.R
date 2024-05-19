
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
          paste0("'", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(),"'"),
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

## Gets update history
getUpdateHistory <- function(pid){
  portalQuery(
    paste("SELECT * FROM updatehistory WHERE pid = ", pid, "ORDER BY time DESC")
  )
}

## Summarizes updated attributes in a tibble
updateSummary <- function(current, inputs){
  tibble(
    attribute = 
      current %>% 
      select(acceleration:throwing) %>% 
      select(!where(is.na)) %>% 
      colnames() %>%
      str_to_title(),
    old = current %>% 
      select(acceleration:throwing) %>% 
      select(!where(is.na)) %>% 
      t() %>% 
      c(),
    new = 
      attribute %>%
      str_remove_all(pattern = " ") %>% 
      sapply(
        X = .,
        FUN = function(x) inputs[[x]],
        simplify = TRUE
      ) %>% 
      unlist()
  ) %>% 
    filter(old != new) 
}

# updateCheck <- function(tpe, updates)

