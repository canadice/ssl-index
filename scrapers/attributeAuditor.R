
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
require(arsenal)

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
  ) %>% 
  mutate(
    Name = if_else(str_detect(Name, "GFuel"), "A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ®", Name)
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
  playerData %>% 
  select(
    Name,
    Acceleration:Throwing
  ) %>% 
  left_join(
    FMAttributes,
    by = "Name", 
    suffix = c(".Forum", ".FM")
  ) %>% 
  relocate(
    sort(colnames(.))
  ) %>% 
  relocate(
    contains("Name"),
    .before = "Acceleration.FM"
  ) %>% 
  relocate(
    contains("."),
    .after = "Name"
  )
  
comparison <- 
  comparedf(
    FMAttributes %>% 
      arrange(Name) %>% 
      filter(
        Name %in% playerData$Name
      ) %>% 
      select(
        Name,
        Acceleration:Throwing
      ) %>% 
      mutate(
        across(
          Acceleration:Throwing,
          as.numeric)
      ),
    playerData %>% 
      arrange(Name) %>% 
      select(
        Name,
        Acceleration:Throwing
      ),
    by = "Name"
  ) %>% 
  summary() %>% 
  .$diffs.table %>% 
  arrange(Name) %>% 
  filter(
    !is.na(values.y)
  )

colnames(comparison) <- 
  str_replace(
    colnames(comparison),
    ".x",
    ".FM"
    )

colnames(comparison) <- 
  str_replace(
    colnames(comparison),
    ".y",
    ".Forum"
  )















