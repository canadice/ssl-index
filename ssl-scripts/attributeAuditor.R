
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
require(DBI)
require(dbplyr)
require(RSQLite)

# source("D:/GitHubs/ssl-index/SSL-Index/app-documents/dataLoader.R")

con <- 
  dbConnect(
    SQLite(), 
    "database/SSL_Database.db"
  )

playerData <- 
  dbGetQuery(con, "SELECT * from Daily_Scrape")

dbDisconnect(con)

auditFunction <- function(path) {
  FMAttributes <- 
    read_html(path, encoding = "UTF-8") %>% 
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
      Name = 
        case_when(
          str_detect(Name, "GFuel") ~ "A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ®", 
          str_detect(Name, "Liang") ~ "Kuai Liang",
          str_detect(Name, "Princess") ~ "Princess Changshan",
          TRUE ~ Name)
    ) %>% 
    relocate(
      c(Pun, Ref, TRO),
      .after = `1v1`
    )
  
  colnames(FMAttributes) <- 
    c(
      "Name",
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
    filter(
      !(Team %in% c("FA", "Retired"))
    ) %>% 
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
        select(
          Name,
          Acceleration:Throwing
        ) %>% 
        dplyr::mutate(
          across(
            Acceleration:Throwing,
            ~ as.numeric(.x))
        ),
      playerData %>% 
        filter(
          !(Team %in% c("FA", "Retired"))
        ) %>% 
        arrange(Name) %>% 
        select(
          Name,
          Acceleration:Throwing
        )%>% 
        dplyr::mutate(
          across(
            Acceleration:Throwing,
            ~ as.numeric(.x))
        ),
      by = "Name"
    ) 
  
  auditAttributes <- 
    comparison %>% 
    summary() %>% 
    .$diffs.table %>% 
    arrange(Name) %>% 
    filter(
      !is.na(values.y)
    )
  
  colnames(auditAttributes) <- 
    str_replace(
      colnames(auditAttributes),
      ".x",
      ".FM"
    )
  
  colnames(auditAttributes) <- 
    str_replace(
      colnames(auditAttributes),
      ".y",
      ".Forum"
    )
  
  
  auditPlayers <- 
    comparison %>% 
    summary() %>% 
    .$obs.table %>% 
    arrange(Name) %>% 
    mutate(
      version = if_else(version == "x", "FM", "Forum")
    )
  
  list(
    "Attributes" = auditAttributes,
    "Players" = auditPlayers
  ) %>% 
    return()
}

list <- 
  auditFunction("D:/Football Manager 2022/screenshots/attributes.html")

attributes <- list$Attributes













