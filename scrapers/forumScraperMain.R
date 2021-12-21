
###########################################################################
###########################################################################
###                                                                     ###
###                        FORUM SCRAPER FOR SSL                        ###
###                                                                     ###
###########################################################################
###########################################################################

print(lubridate::today())

suppressMessages(
  {
    ### Loads all the required packages
    require(XML)
    require(parallel)
    require(fuzzyjoin)
    require(rvest)
    require(dplyr)
    require(tidyr)
    require(magick)
    require(stringr)
    
    if(getwd() == "D:/GitHubs/ssl-index"){
      #Do Nothing
    } else {
      setwd("D:/GitHubs/ssl-index")
    }
    
    ### Loads the separate scripts containing created scraper functions
    ## Functions to find links to teams and players from main forum page
    source("scrapers/rvestPlayerLinkScraper.R")
    
    ## Functions to scrape and structure information from a player page
    source("scrapers/rvestPlayerScraper.R")
    
    ## Loads data sets and other important objects
    source("SSL-Index/app-documents/dataLoader.R", encoding = "UTF-8")
  }
)


##################################################################
##                      Scraping the forum                      ##
##################################################################

# createdForum <- 
#   "http://sslforums.com/index.php?showforum=42" %>% 
#   c(
#     .,
#     paste(., "&st=15", sep = ""),
#     paste(., "&st=30", sep = "")
#   )

prospectForum <- 
  "http://sslforums.com/index.php?showforum=61" %>% 
  c(
    .,
    paste(., "&st=15", sep = ""),
    paste(., "&st=30", sep = ""),
    paste(., "&st=45", sep = "")
  )

teamForums <- 
  c(
    "http://sslforums.com/index.php?showforum=44",
    "http://sslforums.com/index.php?showforum=45",
    "http://sslforums.com/index.php?showforum=51",
    "http://sslforums.com/index.php?showforum=53",
    "http://sslforums.com/index.php?showforum=55",
    "http://sslforums.com/index.php?showforum=66"
  )

forumsToScrape <-
  c(
    # createdForum,
    prospectForum,
    teamForums
  )

players <- 
  sapply(
    forumsToScrape,
    playerLinkScraper
  ) %>% 
  unlist() %>% 
  unname() %>% 
  unique()


{
playerData <- 
  lapply(
    players,
    playerScraper
  ) %>% 
  do.call(
    what = plyr::rbind.fill,
    args = .
  ) %>% 
  mutate(
    across(
      .cols = `TPE Available`:`Defense [R]`,
      as.numeric
    ),
    across(
      .cols = `Aerial Reach`:`Throwing`,
      as.numeric
    ),
    `Natural Fitness` = 20,
    Stamina = 20,
    Team = if_else(is.na(Team), "FA", Team),
    Goalkeeper = if_else(Position == "Goalkeeper", 20, NaN),
    Name = paste(`First Name`, `Last Name`) %>% str_squish(),
    
    `Minimum Wage` = 
      if_else(
        TPE <= 350,
        1*10^6,
        if_else(
          TPE <= 500,
          1.5*10^6,
          if_else(
            TPE <= 650,
            2*10^6,
            if_else(
              TPE <= 800,
              2.5*10^6,
              if_else(
                TPE <= 950,
                3*10^6,
                if_else(
                  TPE <= 1100,
                  3.5*10^6,
                  if_else(
                    TPE <= 1250,
                    4*10^6,
                    if_else(
                      TPE <= 1400,
                      4.5*10^6,
                      if_else(
                        TPE <= 1550,
                        5*10^6,
                        if_else(
                          TPE <= 1750,
                          5.5*10^6,
                          6*10^6
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
  ) %>% 
  relocate(
    Goalkeeper,
    .after = `Defense [R]`
  ) %>% 
  relocate(
    Name, 
    .after = `Last Name`
  ) %>% 
  relocate(
    Striker:Goalkeeper,
    .after = `Preferred Position`
  ) %>% 
  relocate(
    `Aerial Reach`:`Throwing`,
    .after = `Work Rate`
  ) %>% 
  mutate(
    Position = ifelse(is.na(Position), "Undefined", Position),
    Position = 
      factor(Position, levels = c("Goalkeeper", "Defender", "Midfielder", "Forward", "Undefined"))
  ) %>% 
  arrange(
    Created
  ) 
  
}


##----------------------------------------------------------------
##        Calculating player current ability at position         -
##----------------------------------------------------------------

playerAttributesMatrix <- 
  playerData %>% 
  select(
    Acceleration:Throwing
  ) %>% 
  mutate(
    across(
      .fns = ~ replace_na(., 0) %>% as.numeric()
    )
  ) %>% 
  as.matrix()

abilityMatrixFixed <- 
  abilityMatrix[,playerData %>% select(Acceleration:Throwing) %>% colnames()] %>% 
  mutate(
    rowSum = rowSums(.)
  ) %>% 
  rowwise() %>% 
  mutate(
    across(
      .fns = ~ .x/rowSum
    )
  ) %>% 
  select(-rowSum) %>% 
  t() 

currentAbility <- 
  playerAttributesMatrix%*%abilityMatrixFixed

colnames(currentAbility) <- abilityMatrix$Attribute

playerData$currentAbility <- currentAbility


##----------------------------------------------------------------
##                Calculating role and duty values               -
##----------------------------------------------------------------

roleMatrixFixed <-
  roleMatrix[,playerData %>% select(Acceleration:Throwing) %>% colnames()] %>% 
  t()

roleAbility <- 
  playerAttributesMatrix%*%roleMatrixFixed

colnames(roleAbility) <- 
  paste(
    paste(
      roleMatrix$pos, 
      roleMatrix$side %>% replace_na(""),
      sep = " "
      ) %>% 
      str_squish(),
    roleMatrix$role, sep = "\n"
  ) %>% 
  str_replace(pattern = " - ", replacement = "\n")
  

playerData$roleAbility <- roleAbility

playerData <- do.call(what = data.frame, args = playerData)

colnames(playerData) <- 
  colnames(playerData) %>% 
  str_replace(pattern = "\\.", replacement = " ") %>% 
  str_squish()

## Hard-code positional experience as factor in current ability
playerData <- 
  playerData %>% 
  mutate(
    across(
      contains("Ability Goalkeeper"),
      ~ .x * (playerData$Goalkeeper/20)
    ),
    across(
      contains("Ability Defender.L"),
      ~ .x * (playerData$`Defense .L.`/20)
    ),
    across(
      contains("Ability Defender.R"),
      ~ .x * (playerData$`Defense .R.`/20)
    ),
    across(
      contains("Ability Defender.C"),
      ~ .x * (playerData$`Defense .C.`/20)
    ),
    across(
      contains("Ability Fullback.L"),
      ~ .x * (playerData$`Wingback .L.`/20)
    ),
    across(
      contains("Ability Fullback.R"),
      ~ .x * (playerData$`Wingback .R.`/20)
    ),
    across(
      contains("Ability Defensive.Midfielder"),
      ~ .x * (playerData$`Defensive Midfielder..C.`/20)
    ),
    across(
      contains("Ability Midfielder.L"),
      ~ .x * (playerData$`Midfielder .L.`/20)
    ),
    across(
      contains("Ability Midfielder.R"),
      ~ .x * (playerData$`Midfielder .R.`/20)
    ),
    across(
      contains("Ability Midfielder.C"),
      ~ .x * (playerData$`Midfielder .C.`/20)
    ),
    across(
      contains("Ability Attacking.Midfielder.L"),
      ~ .x * (playerData$`Attacking Midfielder..L.`/20)
    ),
    across(
      contains("Ability Attacking.Midfielder.R"),
      ~ .x * (playerData$`Attacking Midfielder..R.`/20)
    ),
    across(
      contains("Ability Attacking.Midfielder.C"),
      ~ .x * (playerData$`Attacking Midfielder..C.`/20)
    ),
    across(
      contains("Ability Striker"),
      ~ .x * (playerData$Striker/20)
    ),
    across(
      contains("Ability"),
      ~ replace_na(.x, 0)
    )
  )

colnames(playerData) <- 
  colnames(playerData) %>% 
  str_replace_all(pattern = "\\.", replacement = " ") %>% 
  str_squish()

# playerData$`currentAbility Goalkeeper` <- playerData$`currentAbility Goalkeeper`*(playerData$Goalkeeper/20)
# playerData$`currentAbility Defender.L` <- playerData$`currentAbility Defender.L`*(playerData$`Defense .L.`/20)
# playerData$`currentAbility Defender.R` <- playerData$`currentAbility Defender.R`*(playerData$`Defense .R.`/20)
# playerData$`currentAbility Defender.C` <- playerData$`currentAbility Defender.C`*(playerData$`Defense .C.`/20)
# playerData$`currentAbility Fullback.L` <- playerData$`currentAbility Fullback.L`*(playerData$`Wingback .L.`/20)
# playerData$`currentAbility Fullback.R` <- playerData$`currentAbility Fullback.R`*(playerData$`Wingback .R.`/20)
# playerData$`currentAbility Defensive.Midfielder` <- playerData$`currentAbility Defensive.Midfielder`*(playerData$`Defensive Midfielder..C.`/20)
# playerData$`currentAbility Midfielder.L` <- playerData$`currentAbility Midfielder.L`*(playerData$`Midfielder .L.`/20)
# playerData$`currentAbility Midfielder.R` <- playerData$`currentAbility Midfielder.R`*(playerData$`Midfielder .R.`/20)
# playerData$`currentAbility Midfielder.C` <- playerData$`currentAbility Midfielder.C`*(playerData$`Midfielder .C.`/20)
# playerData$`currentAbility Attacking.Midfielder.L` <- playerData$`currentAbility Attacking.Midfielder.L`*(playerData$`Attacking Midfielder..L.`/20)
# playerData$`currentAbility Attacking.Midfielder.R` <- playerData$`currentAbility Attacking.Midfielder.R`*(playerData$`Attacking Midfielder..R.`/20)
# playerData$`currentAbility Attacking.Midfielder.C` <- playerData$`currentAbility Attacking.Midfielder.C`*(playerData$`Attacking Midfielder..C.`/20)
# playerData$`currentAbility Striker` <- playerData$`currentAbility Striker`*(playerData$Striker/20)
# 


##################################################################
##                      Writing the results                     ##
################################################################## 

## Writing data to Google Sheet for easier distribution
googlesheets4::gs4_auth(path = ".secrets/client_secret.json")

## Writes the current scrape data to the sheet
googlesheets4::write_sheet(
  data = playerData,
  ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
  sheet = "Daily Scrape"
)



