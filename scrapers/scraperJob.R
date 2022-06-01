
###########################################################################
###########################################################################
###                                                                     ###
###               FORUM SCRAPER SCRIPT FOR GITHUB ACTIONS               ###
###                                                                     ###
###########################################################################
###########################################################################

# remotes::install_github("Canadice/sslrtools")
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
con <- dbConnect(SQLite(), "../database/SSL_Database.db")


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

playerData <- 
  lapply(
    playerLinks,
    FUN = function(x){
      scrape <- try(playerScraper(x), silent=TRUE)
      
      if( 
        inherits(
          scrape,  
          "try-error")
      ){
        print(x)
      } 
      else {
        scrape
      }  
    }
  ) %>% 
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
  )





