
###########################################################################
###########################################################################
###                                                                     ###
###                        FORUM SCRAPER FOR SSL                        ###
###                                                                     ###
###########################################################################
###########################################################################

print(lubridate::today())

### Loads all the required packages
require(XML)
require(parallel)
require(fuzzyjoin)
require(rvest)
require(dplyr)
require(tidyr)

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
source("SSL-Index/app-documents/dataLoader.R")


##################################################################
##                      Scraping the forum                      ##
##################################################################

prospectForum <- 
  "http://sslforums.com/index.php?showforum=42" %>% 
  c(
    .,
    paste(., "&st=15", sep = ""),
    paste(., "&st=30", sep = "")
  )

players <- 
  sapply(
    prospectForum,
    playerLinkScraper
  ) %>% 
  unlist() %>% 
  unname()

playerData <- 
  lapply(
    players,
    playerScraper
  ) %>% 
  do.call(
    what = plyr::rbind.fill,
    args = .
  ) %>% 
  relocate(
    `Date of Birth`,
    .after = Birthplace
  ) %>% 
  mutate(
    across(
      .cols = `TPE Available`:`Defense [R]`,
      as.numeric
    ),
    across(
      .cols = `Aerial Reach`:`Penalty Kick`,
      as.numeric
    ),
    `Natural Fitness` = 20,
    Stamina = 20,
    Team = if_else(is.na(Team), "FA", Team)
  )
  
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



