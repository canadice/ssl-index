
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

countries <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda",
  "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain",
  "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
  "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria",
  "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada",
  "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros",
  "Democratic Republic of the Congo", "Republic of the Congo", "Costa Rica", 
  "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark",
  "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt",
  "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia",
  "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana",
  "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti",
  "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland",
  "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati",
  "Korea, North", "Korea, South", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia",
  "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania",
  "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta",
  "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova",
  "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia",
  "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria",
  "North Macedonia", "Norway", "Oman", "Pakistan", "Palau", "Panama",
  "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal",
  "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia",
  "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe",
  "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore",
  "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan",
  "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan",
  "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", "Trinidad and Tobago",
  "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine",
  "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan",
  "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe",
  "England", "Scotland", "Wales", "Northern Ireland", "USA", "Czech Republic"
)

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
        !is.data.frame(scrape)
      ){
        print(scrape)
      } else {
        print("OK")
        # print(scrape)
        return(scrape)
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
  dplyr::relocate(Username:Team) %>% 
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
      ),
    `All Traits` = `All Traits` %>% str_to_title(),
    Nationality = 
      stringr::str_extract(
        Birthplace, 
        pattern = 
          paste0(
            countries %>% 
              stringr::str_trim(), 
            collapse = "|"
          ) 
      ) %>% 
      dplyr::if_else(
        . == "USA", "United States", .
      ) %>% 
      dplyr::if_else(
        . == "Czech Republic", "Czechia", .
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
  relocate(
    Nationality, 
    .after = `Active`
  ) %>% 
  select(
    Username:Nationality
  )

write.csv(forumData, file = "data/forumData.csv", row.names = FALSE)

RSQLite::dbWriteTable(con, "Daily_Scrape", forumData, overwrite = TRUE)

RSQLite::dbDisconnect(con)


