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
      render = input$render,
      footedness = input$footedness
    ) %>% 
      mutate(
        footedness = if_else(footedness == "Right", "10 | 20", "20 | 10")
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
    
    playerInfo$traits <- paste0(input$traits, collapse = " \\ ")
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
      render = input$render,
      footedness = input$footedness
    ) %>% 
    mutate(
      footedness = if_else(footedness == "Right", "10 | 20", "20 | 10")
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
    
    playerInfo$traits <- paste0(input$traits, collapse = " \\ ")
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
  
  ## SUBMIT BUILD TO A TEMPORARY TABLE FOR APPROVAL 
  insertBuildForApproval(playerInfo)
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
      "SELECT uid, first, last, tpe, tpeused, tpebank FROM playerdata WHERE status_p = -1;"
    )
  ) 
}

approvePlayer <- function(uid){
  portalQuery(
    paste("UPDATE playerdata SET team = 'Academy', status_p = 1, name = concat(first, ' ', last),",
          "class = concat('S', ", currentSeason$season + 1, "), created = ", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), 
          "WHERE uid = ", uid, "AND status_p = -1;")
  )
}




