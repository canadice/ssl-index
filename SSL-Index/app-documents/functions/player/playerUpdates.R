
## Adds update to the update log
updateLog <- function(uid, pid, updates) {
  # Start transaction to ensure all updates are applied atomically.
  portalQuery(query = "START TRANSACTION;", type = "set")
  
  result <- tryCatch({
    # Iterate over each row in the updates data frame.
    for (i in seq_len(nrow(updates))) {
      # Calculate the current timestamp in US/Pacific (in numeric format).
      currentTime <- lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric()
      
      # Extract values for this update and convert the attribute to uppercase.
      attr_val <- toupper(updates$attribute[i])
      old_val  <- updates$old[i]
      new_val  <- updates$new[i]
      
      # Insert a record into the update history using a parameterized query.
      portalQuery(
        query = "INSERT INTO updatehistory (uid, pid, time, attribute, old, new)
                 VALUES (?uid, ?pid, ?time, ?attribute, ?old, ?new);",
        uid       = uid,
        pid       = pid,
        time      = currentTime,
        attribute = attr_val,
        old       = old_val,
        new       = new_val,
        type      = "set"
      )
    }
    TRUE  # Indicate success if all insertions succeed.
  }, error = function(e) {
    # If any error occurs, rollback the transaction and show an error message.
    portalQuery(query = "ROLLBACK;", type = "set")
    showToast(.options = myToastOptions, "error", paste("Error in inserting update log.", e, sep = "\n"))
    FALSE
  })
  
  # If the tryCatch block completed successfully, commit the transaction.
  if (result) {
    portalQuery(query = "COMMIT;", type = "set")
  }
}


## Updates build on player
updateBuild <- function(pid, updates, bank = NULL) {
  # Construct update clauses for each attribute.
  # Note: Column names are directly embedded after converting to lower-case.
  # Make sure updates$attribute is prevalidated against a safe list of column names.
  clauses <- sapply(seq_along(updates$attribute), function(i) {
    col <- str_to_lower(updates$attribute[i])
    paste0("`", col, "` = ?val", i)
  })
  
  # If a bank value is provided, add its clause.
  if (!is.null(bank)) {
    clauses <- c(clauses, "tpebank = ?bank")
  }
  
  # Build the full query string.
  query <- paste0(
    "UPDATE playerdata SET ",
    paste(clauses, collapse = ", "),
    " WHERE pid = ?pid;"
  )
  
  # Build the list of parameters.
  params <- list(pid = pid)
  
  # Add update values with keys matching the placeholders (val1, val2, etc.)
  for (i in seq_along(updates$new)) {
    params[[paste0("val", i)]] <- updates$new[i]
  }
  
  # Add bank parameter if bank is provided.
  if (!is.null(bank)) {
    params$bank <- bank
  }
  
  # Remove any double quotes from each character value
  params <- lapply(params, function(x) {
    if (is.character(x)) {
      gsub('"', '', x)
    } else {
      x
    }
  })
  
  do.call(portalQuery, c(list(query = query, type = "set"), params))
  
}

completeRedistribution <- function(pid) {
  portalQuery(
    query = "UPDATE playerdata
             SET redistused = 1 
             WHERE pid = ?pid;",
    pid = pid,
    type = "set"
  )
}


completeReroll <- function(pid) {
  portalQuery(
    query = "UPDATE playerdata 
             SET rerollused = 1 
             WHERE pid = ?pid;",
    pid = pid,
    type = "set"
  )
}


completeRetirement <- function(pid){
  data <- 
    portalQuery(
      query = 
        "SELECT * 
        FROM allplayersview
        WHERE pid = ?pid;",
      pid = pid
    )
  
  sendRetiredPlayer(data)
  
  portalQuery(
    query = "UPDATE playerdata SET status_p = 2 WHERE pid = ?pid;",
    pid = pid,
    type = "set"
  )
  
}

## Gets update history
getUpdateHistory <- function(pid){
  portalQuery(
    query = 
      "SELECT Time, Username, `Changed attribute`, `From`, `To`
      FROM updatehistoryview
      WHERE pid = ?pid
      ORDER BY Time DESC;",
    pid = pid
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

