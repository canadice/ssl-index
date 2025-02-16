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
      render = input$render %>% str_replace_all("'", "\\\\'")
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
    
    playerInfo$traits <- paste0(input$traits, collapse = traitSep) %>% str_replace_all(pattern = "'", replacement = "\\\\'")
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
        where(is.character),
        ~ paste("'", .x, "'", sep = "")
      ),
      across(
        everything(),
        ~ if_else(.x == "", NA, .x)
      )
    )
  
  ## SUBMIT BUILD TO PLAYER DATA FOR APPROVAL 
  insertBuildForApproval(playerInfo)
  
  sendNewCreate(playerInfo, username = userinfo$username)
}

insertBuildForApproval <- function(playerInfo){
  portalQuery(
  # print(
    paste(
      "INSERT INTO playerdata (", paste("`", colnames(playerInfo), "`", sep = "", collapse = ", "), ")
               VALUES",
      paste(
        "(",
        paste(
          playerInfo[1, ],
          collapse = ","
        ),
        ")",
        collapse = ","
      ),
      ";"
    )
  )  
}

checkIfAlreadyApproving <- function(uid){
  current <- 
    portalQuery(
      paste(
        "SELECT * FROM playerdata WHERE uid = ", uid, "AND status_p = -1;"
      )
    )
  
  current %>% nrow() > 0
}

checkDuplicatedName <- function(first, last){
  portalQuery(
    paste(
      "SELECT * FROM playerdata WHERE first = '", first, "' AND last = '", last, "';", sep = ""
    )
  ) %>% 
    nrow() > 0
}

getPlayersForApproval <- function(){
  portalQuery(
    # print(
    paste(
      "SELECT mybb.username, pd.uid, pd.pid, pd.first, pd.last, pd.tpe, pd.tpeused, pd.tpebank 
      FROM playerdata pd
      LEFT JOIN 
          mybbdb.mybb_users mybb ON pd.uid = mybb.uid
      WHERE status_p = -1;"
    )
  ) 
}

approvePlayer <- function(uid, session = getDefaultReactiveDomain()){
  tryCatch(
    portalQuery(
      paste(
        "INSERT INTO tpehistory (time, uid, pid, source, tpe) 
      SELECT ", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), ", 1, pid, 'Initial TPE', 350
      FROM playerdata
      WHERE uid = ", uid, "AND status_p = -1;"
      )
    ),
    error = function(e) showToast(.options = myToastOptions,"error", paste("Error in inserting TPE History.", e, sep = "\n"))
  )
  
  data <- portalQuery(
  paste(
  "SELECT pid, `pos_st`, `pos_lam`, `pos_cam`, `pos_ram`, `pos_lm`, `pos_cm`, `pos_rm`, `pos_lwb`, 
    `pos_cdm`, `pos_rwb`, `pos_ld`, `pos_cd`, `pos_rd`, `pos_gk`, `acceleration`, `agility`, 
    `balance`, `jumping reach`, `natural fitness`, `pace`, `stamina`, `strength`, `corners`, 
    `crossing`, `dribbling`, `finishing`, `first touch`, `free kick`, `heading`, `long shots`, 
    `long throws`, `marking`, `passing`, `penalty taking`, `tackling`, `technique`, `aggression`, 
    `anticipation`, `bravery`, `composure`, `concentration`, `decisions`, `determination`, `flair`, 
    `leadership`, `off the ball`, `positioning`, `teamwork`, `vision`, `work rate`, `aerial reach`, 
    `command of area`, `communication`, `eccentricity`, `handling`, `kicking`, `one on ones`, `reflexes`, 
    `tendency to rush`, `tendency to punch`, `throwing`, `traits`, `left foot`, `right foot`
     FROM playerdata WHERE uid = ", uid, "AND status_p = -1;")
  ) %>% 
    pivot_longer(!pid, values_transform = as.character)
  
  tryCatch(
    portalQuery(
      paste(
        "INSERT INTO updatehistory (time, uid, pid, attribute, old, new) VALUES",
        paste(
          "(",
          paste(
            paste0("'", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), "'"),
            1,
            data$pid,
            paste0("'", data$name %>% str_to_upper(), "'"),
            if_else(data$name %>% str_detect("pos"), '0', if_else(data$name == "traits", "'NO TRAITS'", '5')),
            paste0("'", data$value, "'"),
            sep = ","
          ),
          ")",
          collapse = ","
        ),
        ";"
      )
    ),
    error = function(e) showToast(.options = myToastOptions,"error", paste("Error in inserting Update History.", e, sep = "\n"))
  )
  
  data <- 
    portalQuery(
      paste(
        "SELECT pd.pid, pd.first, pd.last, pd.tpebank, pd.tpe, pd.position, mybb.username AS username, mbuf.fid4 AS discord
        FROM playerdata pd 
        LEFT JOIN mybbdb.mybb_users mybb ON pd.uid = mybb.uid 
        LEFT JOIN mybbdb.mybb_userfields mbuf ON pd.uid = mbuf.ufid
        WHERE pd.uid = ", uid, "AND pd.status_p = -1;"
      )
    )
  
  tryCatch(
    addBankTransaction(uid = 1, pid = data$pid, source = "Academy Contract", transaction = 3000000, status = 1),
    error = function(e) showToast(.options = myToastOptions,"error", paste("Error in adding academy contract.", e, sep = "\n"))
  )
  
  today <- (now() %>% with_tz("US/Pacific") %>% as_date() %>% as.numeric())
  start <- (currentSeason$startDate %>% as_date()) %>% as.numeric()
  
  tpe <- 
    tibble(
      source = "Catch-up TPE",
      tpe = floor((today - start)/7)*6
    )
  
  if(tpe$tpe > 0){
    tryCatch(
      tpeLog(uid = 1, pid = data$pid, tpe = tpe),
      error = function(e) showToast(.options = myToastOptions,"error", paste("Error in logging catch-up TPE.", e, sep = "\n"))
    )
    
    tryCatch(
      updateTPE(pid = data$pid, tpe = tpe),
      error = function(e) showToast(.options = myToastOptions,"error", paste("Error in adding catch-up TPE.", e, sep = "\n"))
    )
  }
  
  tryCatch(
    sendApprovedCreate(data),
    error = function(e) showToast(.options = myToastOptions,"error", paste("Error in sending approved create to Discord.", e, sep = "\n"))
  )
  
  tryCatch(
    portalQuery(
      paste("UPDATE playerdata SET rerollused = 0, redistused = 0, team = -1, affiliate = 1, status_p = 1, name = concat(first, ' ', last),",
            "class = concat('S', ", currentSeason$season + 1, "), created = ", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), 
            "WHERE uid = ", uid, "AND status_p = -1;")
    ),
    error = function(e) showToast(.options = myToastOptions,"error", paste("Error in updating player data.", e, sep = "\n"))
  )
  
}




