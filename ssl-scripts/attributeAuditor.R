
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
require(sslrtools)

playerData <- readAPI(url = "https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true"))

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
      -"Inf"
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
      c(Pun, TRO),
      .after = Tec
    ) %>% 
    select(Name, Acc:Wor)
  
  colnames(FMAttributes) <- 
    c(
      "name",
      # Attributes
      playerData %>% 
        select(
          acceleration:throwing
        ) %>% 
        colnames() %>% 
        sort(),
      # Hidden traits
      "versatility", "temperament", "sportmanship", "important matches", "proffessionalism", "pressure", "loyalty", "injury proneness", 
      "dirtiness", "controversy", "consistency", "adaptability", "ambition" 
    )
  
  audit <- 
    playerData %>% 
    filter(
      !(team %in% c("Free Agent", "Retired"))
    ) %>% 
    select(
      name,
      acceleration:`work rate`
    ) %>% 
    left_join(
      FMAttributes,
      by = "name", 
      suffix = c(".Forum", ".FM")
    ) %>% 
    relocate(
      sort(colnames(.))
    ) %>% 
    relocate(
      contains("name"),
      .before = "acceleration.FM"
    ) %>% 
    relocate(
      contains("."),
      .after = "name"
    )
  
  comparison <- 
    comparedf(
      FMAttributes %>% 
        arrange(name) %>% 
        select(
          name,
          acceleration:`work rate`
        ) %>% 
        dplyr::mutate(
          across(
            acceleration:`work rate`,
            ~ as.numeric(.x))
        ),
      playerData %>% 
        filter(
          !(team %in% c("Free Agent", "Retired"))
        ) %>% 
        arrange(name) %>% 
        select(
          name,
          acceleration:`work rate`
        )%>% 
        dplyr::mutate(
          across(
            acceleration:`work rate`,
            ~ as.numeric(.x))
        ),
      by = "name"
    ) 
  
  auditAttributes <- 
    comparison %>% 
    summary() %>% 
    .$diffs.table %>% 
    arrange(name) %>% 
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
    arrange(name) %>% 
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
  auditFunction("D:/Documents/Sports Interactive/Football Manager 2024/EXPORTS/attributes.html")

attributes <- list$Attributes













