
############################################################################
############################################################################
###                                                                      ###
###                         FILE AUDITOR                                 ###
###                                                                      ###
############################################################################
############################################################################

require(rvest)
require(stringr)
require(stringi)
require(plyr)
require(dplyr)
require(tidyr)

source("D:/GitHubs/ssl-index/SSL-Index/app-documents/dataLoader.R")

FMAttributes <- 
  read_html("D:/FootballManager2022/screenshots/attributes.html", encoding = "UTF-8") %>% 
  html_elements("table") %>% 
  html_table() %>% 
  .[[1]] %>% 
  mutate(
    Name = 
      Name %>% 
      str_split(
        pattern = " - ", 
        simplify = TRUE
      ) %>% 
      .[,1]
  ) %>% 
  select(
    -"Inf",
    -"Rec"
  )

colnames(FMAttributes) <- 
  c(
    "Name",
    "Club",
    "Position",
    # Attributes
    playerData %>% 
      select(
        Acceleration:Throwing
      ) %>% 
      colnames(),
    # Hidden traits
    "Versatility", "Temperament", "Sportmanship", "Important Matches", "Proffessionalism", "Pressure", "Loyalty", "Injury Proneness", 
    "Dirtiness", "Controversy", "Consistency", "Adaptability", "Ambition" 
  )

audit <- 
  FMAttributes %>% 
  left_join(
    playerData %>% 
      select(
        Name,
        Acceleration:Throwing
      ),
    by = "Name", 
    suffix = c(".FM", ".Forum")
  ) %>% 
  relocate(
    sort(colnames(.))
  ) %>% 
  relocate(
    contains("Name"),
    .before = "Acceleration.FM"
  )
  
comparison <- 
  comparedf(
    FMAttributes %>% 
      arrange(Name) %>% 
      select(
        Name,
        Acceleration:Throwing
      ),
    playerData %>% 
      arrange(Name) %>% 
      select(
        Name,
        Acceleration:Throwing
      )
  ) %>% 
  summary()















