
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

teamData <- 
  dbGetQuery(con, "SELECT * FROM Team_Information")

countries <- c(
  "United States", "USA",
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
  "United Arab Emirates", "United Kingdom", "Uruguay", "Uzbekistan",
  "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe",
  "England", "Scotland", "Wales", "Northern Ireland", "Czech Republic", "Côte d'Ivoire",
  "Faroe Islands", "Gibraltar", "Catalonia"
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
      scrape <- tryCatch(sslrtools::playerScraper(x), error = function(e) paste(x, "produces this error: ", e))
      
      if( 
        !is.data.frame(scrape)
      ){
        print(scrape)
        return(data.frame(Playerlink = x)) 
      } else {
        # print("OK")
        # print(scrape)
        return(scrape)
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
  ) %>% 
  dplyr::relocate(Username:Team) %>% 
  dplyr::mutate(
    across(
      .cols = `TPE Available`:`Throwing`,
      as.numeric
    ),
    `Natural Fitness` = 20,
    Stamina = 20,
    Goalkeeper = if_else(Position %in% c("Goalkeeper", "GK"), 20, NaN),
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
        TPE <= 1700 ~ 5.5*10^6,
        TRUE ~ 6*10^6
      ),
    `All Traits` = `All Traits` %>% str_to_title(),
    Nationality = 
      stringr::str_extract(
        Birthplace, 
        pattern = 
          paste0(
            countries %>% 
              stringr::str_trim() %>% 
              paste(
                "$",
                sep = ""
              ), 
            collapse = "|"
          )
      ) %>% 
      dplyr::if_else(
        . == "USA", "United States", .
      ) %>% 
      dplyr::if_else(
        . == "Czech Republic", "Czechia", .
      ) %>% 
      dplyr::if_else(
        . == "Catalonia", "Spain", .
      ),
    Team = if_else(Team == "Sydney City", "Prospect", Team)
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
    Position = ifelse(is.na(Position), "Undefined", Position)
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
    Username:Nationality,
    Userlink,
    Playerlink
  ) %>% 
  filter(
    !is.na(Team)
  )

write.csv(forumData, file = "data/forumData.csv", row.names = FALSE)

RSQLite::dbWriteTable(con, "Daily_Scrape", forumData, overwrite = TRUE)

#### Audits the roster pages against the budget
findAuthor <- function(link) {
  forum <- xml2::read_html(link)
  
  authors <- 
    forum %>% 
    rvest::html_elements(".author a") %>% 
    html_text() %>% 
    .[is.na(as.numeric(.))]
  
  team <- 
    forum %>% 
    html_elements(".thead") %>% 
    html_text() %>% 
    .[stringr::str_detect(string = ., pattern = "Roster|Player Updates")] %>% 
    stringr::str_remove_all(pattern = "\\r|\\t|\\n|Roster|Player Updates") %>% 
    stringr::str_squish()
  
  title <- 
    forum %>% 
    html_elements(".subject_new") %>% 
    html_text()
  
  data.frame(
    player = title,
    authors = authors,
    team = rep(team, length(authors))
  ) %>% 
    return()
}

googlesheets4::gs4_deauth()

budget <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1qjHSYloQL2h7Cbgz1g0woRnfvgrChbHJP2tmvaeJCgU/edit#gid=1311763493",
    sheet = "ALL_PLAYERS"
  )

rosterPages <-
  c("https://forum.simulationsoccer.com/forumdisplay.php?fid=148", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=59", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=113", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=60", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=105", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=81", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=66", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=139", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=116", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=152", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=135", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=63", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=68", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=70", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=141", 
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=132"
  )

updatePages <- 
  c(
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=149",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=62",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=114",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=61",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=106",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=82",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=80",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=140",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=117",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=153",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=136",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=64",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=69",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=71",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=142",
    "https://forum.simulationsoccer.com/forumdisplay.php?fid=133"
  )

rosterAudit <- 
  suppressWarnings({
    lapply(
      X = rosterPages,
      FUN = function(x) {
        result <- tryCatch(findAuthor(x), error = function(e) paste(x, "produces this error: ", e))
      }
    ) %>% 
      do.call(
        what = rbind,
        args = .
      ) %>% 
      full_join(
        lapply(
          X = updatePages,
          FUN = function(x) {
            result <- tryCatch(findAuthor(x), error = function(e) paste(x, "produces this error: ", e))
          }
        ) %>% 
          do.call(
            what = rbind,
            args = .
          ),
        by = "authors"
      ) %>% 
      full_join(
        budget %>% 
          select(
            Team,
            `Major/Minor`,
            Username,
            Bank:`Minimum Wage`
          ),
        by = c("authors" = "Username")
      ) %>% 
      group_by(authors) %>% 
      mutate(
        team.roster = team.x,
        team.update = team.y,
        team.budget = str_split(Team, pattern = " & ", simplify = TRUE) %>% .[if_else(`Major/Minor` == "Major", 1, 2)]
      ) %>% 
      relocate(
        team.roster,
        team.update,
        team.budget
      ) %>% 
      group_by(
        team.roster
      ) %>% 
      mutate(
        cap = sum(S13, na.rm = TRUE),
        assigned = n(),
        player = paste(player.x, player.y)
      ) %>% 
      relocate(
        cap,
        assigned,
        S13
      ) %>% 
      filter(
        !is.na(authors)
      ) %>% 
      select(
        cap:team.budget,
        authors,
        player
      )
  })


RSQLite::dbWriteTable(con, "rosterAudit", rosterAudit, overwrite = TRUE)


RSQLite::dbDisconnect(con)


