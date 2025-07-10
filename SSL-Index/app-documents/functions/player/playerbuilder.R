checkBuild <- function(input, tpebank, session){
  playerInfo <- 
    tibble(
      first = input$firstName,
      last = input$lastName,
      tpe = 350,
      tpeused = 350 - tpebank,
      tpebank = tpebank,
      birthplace = input$birthplace,
      nationality = input$nationality,
      height = input$height,
      weight = input$weight,
      hair_color = input$hairColor,
      hair_length = input$hairLength,
      skintone = input$skintone,
      render = input$render
    ) %>% 
    mutate(
      `left foot` = if_else(input$footedness == "Right", 10, 20),
      `right foot` = if_else(input$footedness == "Left", 10, 20),
      render = input$render
    )
  
  
  if(input$playerType == "Outfield"){
    playerInfo$position = input$primary
    
    # Add pos_ variables for each position
    positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
    for (pos in positions) {
      playerInfo <- playerInfo %>%
        mutate(!!paste0("pos_", tolower(pos)) := case_when(
          input$primary == pos ~ 20,
          pos %in% input$secondary ~ 15,
          TRUE ~ 0
        ))
    }
    
    playerInfo$traits <- paste0(input$traits, collapse = traitSep)
  } else {
    playerInfo$position <- "GK"
    
    # Add pos_ variables for each position
    positions <- c("LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
    for (pos in positions) {
      playerInfo <- playerInfo %>%
        mutate(!!paste0("pos_", tolower(pos)) := 0)
    }
    playerInfo$pos_gk <- 20
  }
  
  # Add attributes variables for each position
  for (att in attributes$attribute) {
    playerInfo <- playerInfo %>%
      mutate(!!tolower(att) := input[[att %>% str_to_title() %>% str_remove_all(pattern = " ")]])
  }
  
  modalVerifyBuild(playerInfo, session)
  
}

submitBuild <- function(input, tpebank, userinfo){
  playerInfo <- 
    tibble(
      uid = userinfo$uid,
      status_p = -1,
      first = input$firstName,
      last = input$lastName,
      tpe = 350,
      tpeused = 350 - tpebank,
      tpebank = tpebank,
      birthplace = input$birthplace,
      nationality = input$nationality,
      height = input$height,
      weight = input$weight,
      hair_color = input$hairColor,
      hair_length = input$hairLength,
      skintone = input$skinColor,
      render = input$render
    ) %>% 
    mutate(
      `left foot` = if_else(input$footedness == "Right", 10, 20),
      `right foot` = if_else(input$footedness == "Left", 10, 20)
    )
  
  
  if(input$playerType == "Outfield"){
    playerInfo$position = input$primary
    
    # Add pos_ variables for each position
    positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
    for (pos in positions) {
      playerInfo <- playerInfo %>%
        mutate(!!paste0("pos_", tolower(pos)) := case_when(
          input$primary == pos ~ 20,
          pos %in% input$secondary ~ 15,
          TRUE ~ 0
        ))
    }
    
    playerInfo$traits <- paste0(input$traits, collapse = traitSep)
  } else {
    playerInfo$position <- "GK"
    
    # Add pos_ variables for each position
    positions <- c("LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
    for (pos in positions) {
      playerInfo <- playerInfo %>%
        mutate(!!paste0("pos_", tolower(pos)) := 0)
    }
    playerInfo$pos_gk <- 20
    
    playerInfo$traits <- "NO TRAITS"
  }
  
  # Add attributes variables for each position
  for (att in attributes$attribute) {
    playerInfo <- playerInfo %>%
      mutate(!!tolower(att) := input[[att %>% str_to_title() %>% str_remove_all(pattern = " ")]])
  }
  
  playerInfo <- 
    playerInfo %>% 
    mutate(
      across(
        everything(),
        ~ if_else(.x == "", NA, .x)
      )
    )
  
  ## SUBMIT BUILD TO PLAYER DATA FOR APPROVAL 
  insertBuildForApproval(playerInfo)
  
  sendNewCreate(playerInfo, username = userinfo$username)
}

insertBuildForApproval <- function(playerInfo) {
  # Extract column names from playerInfo (assumes it's a data frame)
  cols <- colnames(playerInfo)
  
  # Create cleaned parameter names by removing spaces from the column names.
  # You can adjust this replacement if needed.
  param_names <- gsub(" ", "", cols)
  
  # Build the placeholders for each column using the cleaned names.
  # For example, if a column is "left foot", then the placeholder becomes ?leftfoot.
  placeholders <- paste0("?", param_names)
  
  # Construct the query:
  # The column names are still quoted exactly as in the database using backticks.
  query <- paste0(
    "INSERT INTO playerdata (", 
    paste(sprintf("`%s`", cols), collapse = ", "), 
    ") VALUES (", 
    paste(placeholders, collapse = ", "), 
    ");"
  )
  
  # Convert the first row of playerInfo into a list of parameters.
  params <- as.list(playerInfo[1, , drop = FALSE])
  
  # Clean character values by removing any double quotes.
  params <- lapply(params, function(x) {
    if (is.character(x)) gsub('"', '', x) else x
  })
  
  # Rename the list elements to match our cleaned parameter names.
  names(params) <- param_names
  
  # print(query)
  # print(params)
  
  # Use do.call to invoke portalQuery with the query and the named parameters.
  do.call(portalQuery, c(list(query = query, type = "set"), params))
}


checkIfAlreadyApproving <- function(uid) {
  portalQuery(
    query = "SELECT * FROM playerdata WHERE uid = ?uid AND status_p = -1;",
    uid   = uid,
    type  = "get"
  ) |> 
    nrow() > 0
}

checkDuplicatedName <- function(first, last) {
  portalQuery(
    query = "SELECT * FROM playerdata WHERE first = ?first AND last = ?last;",
    first = first,
    last  = last,
    type  = "get"
  ) |>
    nrow() > 0
}

getPlayersForApproval <- function(){
  portalQuery(
    query = 
      "SELECT username, pid, first, last, tpe, tpebank, render, uid 
    FROM unapprovedplayersview;"
  ) 
}

approvePlayer <- function(uid, session = getDefaultReactiveDomain()){
  # Start a transaction before running the queries
  portalQuery(query = "START TRANSACTION;", type = "set")
  
  # Use a tryCatch block to run all queries then commit or rollback
  result <- tryCatch({
    # -------------------------------
    # 1. Insert TPE History
    currentTime <- lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric()
    portalQuery(
      query = "INSERT INTO tpehistory (time, uid, pid, source, tpe)
               SELECT ?currentTime, 1, pid, 'Initial TPE', 350
               FROM playerdata
               WHERE uid = ?uid AND status_p = -1;",
      currentTime = currentTime,
      uid = uid,
      type = "set"
    )
    
    # -------------------------------
    # 2. Retrieve player attributes for update-history
    data_attributes <- portalQuery(
      query = "SELECT pid, 
                       `pos_st`, `pos_lam`, `pos_cam`, `pos_ram`, `pos_lm`, `pos_cm`, `pos_rm`, 
                       `pos_lwb`, `pos_cdm`, `pos_rwb`, `pos_ld`, `pos_cd`, `pos_rd`, `pos_gk`, 
                       acceleration, agility, balance, `jumping reach`, `natural fitness`, pace, 
                       stamina, strength, corners, crossing, dribbling, finishing, `first touch`, 
                       `free kick`, heading, `long shots`, `long throws`, marking, passing, 
                       `penalty taking`, tackling, technique, aggression, anticipation, bravery, 
                       composure, concentration, decisions, determination, flair, leadership, 
                       `off the ball`, positioning, teamwork, vision, `work rate`, `aerial reach`, 
                       `command of area`, communication, eccentricity, handling, kicking, 
                       `one on ones`, reflexes, `tendency to rush`, `tendency to punch`, throwing, 
                       traits, `left foot`, `right foot`
                FROM playerdata
                WHERE uid = ?uid AND status_p = -1;",
      uid = uid,
      type = "get"
    )
    
    # Pivot the attribute data from wide to long format
    data_long <- data_attributes %>% pivot_longer(!pid, values_transform = as.character)
    
    # -------------------------------
    # 3. Insert update history for each attribute row
    for(i in seq_len(nrow(data_long))) {
      attribute <- toupper(data_long$name[i])
      oldVal <- if_else(str_detect(data_long$name[i], "pos"),
                        "0",
                        if_else(data_long$name[i] == "traits", "NO TRAITS", "5"))
      newVal <- data_long$value[i]
      
      portalQuery(
        query = "INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
                 VALUES (?time, ?uid, ?pid, ?attribute, ?old, ?new);",
        time      = currentTime,
        uid       = 1,  # Adjust if necessary; here uid 1 is used for update history records
        pid       = data_long$pid[i],
        attribute = attribute,
        old       = oldVal,
        new       = newVal,
        type      = "set"
      )
    }
    
    # -------------------------------
    # 4. Retrieve updated player data 
    data_player <- portalQuery(
      query = 
        "SELECT *
        FROM unapprovedplayersview
        WHERE uid = ?uid;",
      uid  = uid,
      type = "get"
    )
    
    # -------------------------------
    # 5. Add Academy Contract Bank Transaction
    addBankTransaction(uid = 1, pid = data_player$pid,
                       source = "Academy Contract", transaction = 3000000, status = 1)
    
    # -------------------------------
    # 6. Calculate and handle Catch-up TPE
    today <- now() %>% with_tz("US/Pacific") %>% as_date() %>% as.numeric()
    start <- currentSeason$startDate %>% as_date() %>% as.numeric()
    tpe   <- tibble(source = "Catch-up TPE",
                    tpe = floor((today - start)/7) * 6)
    
    if(tpe$tpe > 0) {
      tpeLog(uid = 1, pid = data_player$pid, tpe = tpe)
      updateTPE(pid = data_player$pid, tpe = tpe)
    }
    
    # -------------------------------
    # 7. Send approved create message to Discord
    sendApprovedCreate(data_player)
    
    # -------------------------------
    # 8. Final update to playerdata
    newSeason <- currentSeason$season + 1
    portalQuery(
      query = "UPDATE playerdata 
               SET rerollused = 0,
                   redistused = 0,
                   team = -1,
                   affiliate = 1,
                   status_p = 1,
                   name = concat(first, ' ', last),
                   class = concat('S', ?newSeason),
                   created = ?created
               WHERE uid = ?uid AND status_p = -1;",
      newSeason = newSeason,
      created   = currentTime,
      uid       = uid,
      type      = "set"
    )
    
    # If all queries succeed, mark as TRUE so we can commit later
    TRUE 
  }, error = function(e) {
    # If any error occurs, roll back the entire transaction
    portalQuery(query = "ROLLBACK;", type = "set")
    showToast(.options = myToastOptions,
              "error",
              paste("Error during approvePlayer transaction.", e, sep = "\n"))
    FALSE
  })
  
  # Only if the transaction block was successful, commit the changes
  if(isTRUE(result)) {
    portalQuery(query = "COMMIT;", type = "set")
  }
}




