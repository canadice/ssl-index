
###########################################################################
###########################################################################
###                                                                     ###
###               FORUM SCRAPER SCRIPT FOR GITHUB ACTIONS               ###
###                                                                     ###
###########################################################################
###########################################################################

#remotes::install_github("Canadice/sslrtools")
require(sslrtools)

require(rvest)

require(plyr)

require(dplyr)

require(tidyr)

require(stringr)

require(DBI)
require(dbplyr)
require(RSQLite)

## Opens the connection to the SQLite Database
con <- RSQLite::dbConnect(RSQLite::SQLite(), "database/SSL_Database.db")

teamInfo <- 
  dbGetQuery(con, "SELECT * FROM Team_Information")

#################################################################
##                      Scrapes the forum                      ##
#################################################################

playerLinks <-
  sslrtools::teamLinks() %>% 
  sapply(
    X = .,
    FUN = sslrtools::playerLinkScraper
  ) %>% 
  unlist() %>% 
  unname() %>% 
  unique()

forumData <- 
  lapply(
    playerLinks,
    FUN = function(x){
      scrape <- tryCatch(playerScraper(x), error = function(e) paste(x, "produces this error: ", e))
      
      if( 
        inherits(
          scrape,  
          "error")
      ){
        print(scrape)
      } else {
        scrape
      }
    }
  )%>% 
  do.call(
    what = plyr::rbind.fill,
    args = .
  ) %>% 
  dplyr::relocate(
    c(
      Acceleration,Agility,Balance,`Jumping Reach`,
      `Natural Fitness`,Pace,Stamina,Strength,
      Corners,Crossing,Dribbling,Finishing,`First Touch`,
      `Free Kick`,Heading,`Long Shots`,`Long Throws`,
      Marking,Passing,`Penalty Taking`,Tackling,
      Technique,Aggression,Anticipation,Bravery,
      Composure,Concentration,Decisions,
      Determination,Flair,Leadership,`Off the Ball`,
      Positioning,Teamwork,Vision,`Work Rate`,
      `Aerial Reach`:`Throwing`
    ),
    .after = `TPE Available`
  ) %>% 
  dplyr::relocate(
    contains("Trait"),
    .after = Throwing
  ) %>% 
  dplyr::relocate(
    c(lastPost, Active),
    .after = `All Traits`
  ) %>% 
  dplyr::mutate(
    across(
      .cols = `TPE Available`:`Throwing`,
      as.numeric
    ),
    `Natural Fitness` = 20,
    Stamina = 20,
    Goalkeeper = if_else(Position == "Goalkeeper", 20, NaN),
    Name = paste(`First Name`, `Last Name`) %>% str_squish(),
    
    `Minimum Wage` = 
      case_when(
        TPE <= 350 ~ 1*10^6,
        TPE <= 500 ~ 1.5*10^6,
        TPE <= 650 ~ 2*10^6,
        TPE <= 800 ~ 2.5*10^6,
        TPE <= 950 ~ 3*10^6,
        TPE <= 1100 ~ 3.5*10^6,
        TPE <= 1250 ~ 4*10^6,
        TPE <= 1400 ~ 4.5*10^6,
        TPE <= 1550 ~ 5*10^6,
        TPE <= 1750 ~ 5.5*10^6,
        TRUE ~ 6*10^6
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
  dplyr::mutate(
    Position = ifelse(is.na(Position), "Undefined", Position),
    Position = 
      factor(Position, levels = c("Goalkeeper", "Defender", "Midfielder", "Forward", "Undefined")),
    across(
      .cols = Striker:Goalkeeper,
      as.numeric
    )
  ) %>% 
  arrange(
    Created
  ) %>% 
  relocate(
    `Minimum Wage`,
    .after = `All Traits`
  ) %>% 
  select(
    Username:Active
  )

write.csv(forumData, file = "data/forumData.csv", row.names = FALSE)

RSQLite::dbWriteTable(con, "Daily_Scrape", forumData, overwrite = TRUE)

RSQLite::dbDisconnect(con)


