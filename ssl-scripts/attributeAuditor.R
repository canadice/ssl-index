
############################################################################
############################################################################
###                                                                      ###
###                         FILE AUDITOR                                 ###
###                                                                      ###
############################################################################
############################################################################

require(stringi)
require(arsenal)
require(sslrtools)
require(tidyverse)
require(glue)
require(bit64)
require(rvest)

playerData <- readAPI(url = "https://api.simulationsoccer.com/player/getUpdatedBuilds")

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
    select(Name, Club, Acc:Wor)
  
  colnames(FMAttributes) <- 
    c(
      "name", "club",
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
      FMAttributes %>% select(!club),
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
  
  auditTeams <- 
    playerData %>% 
    select(name, team = teamName, league = currentAffiliate) %>% 
    full_join(
      FMAttributes %>% select(name, club),
      by = c("name")
    ) %>% 
    mutate(
      club = case_when(
        club == "Rapid Magyar" ~ "Rapid Magyar SC",
        club == "Seoul Mythic FC" ~ "Seoul MFC",
        TRUE ~ club
      )
    ) |> 
    filter(team != club | club |> is.na()) |> 
    filter(!(team %in% c("Retired", "Free Agent", "Academy") & club |> is.na()))
  
  list(
    "Attributes" = auditAttributes |> 
      mutate(across(where(is.list), ~unlist(.x))),
    "Players" = auditPlayers,
    "Teams" = auditTeams
  ) %>% 
    return()
}

list <- 
  auditFunction("D:/Documents/Sports Interactive/Football Manager 2024/EXPORTS/attributes.html")

attributes <- list$Attributes 
teams <- list$Teams


FMAttributes <- 
  read_html("D:/Documents/Sports Interactive/Football Manager 2024/EXPORTS/attributes.html", encoding = "UTF-8") %>% 
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
  ) |> 
  select(Position, Acc:`Right Foot`, CA) |>
  mutate(
    Position = if_else(Position == "GK", 20, 0),
    `Left Foot` = case_when(
      `Left Foot` == "Very Strong" ~ 20,
      `Left Foot` == "Strong" ~ 15,
      TRUE ~ 10
    ), 
    `Right Foot` = case_when(
      `Right Foot` == "Very Strong" ~ 20,
      `Right Foot` == "Strong" ~ 15,
      TRUE ~ 10
    )
  )

colnames(FMAttributes) <- 
  c(
    "pos_gk", 
    # Attributes
    playerData %>% 
      select(
        acceleration:throwing
      ) %>% 
      colnames() %>% 
      sort(),
    "left foot",
    "right foot",
    "CA"
  )

downloadPlayer <- function(temp){
  jsonTraits <-
    c(
      "Runs With Ball Down Left",
      "Runs With Ball Down Right",
      "Runs With Ball Through Centre",
      "Gets Into Opposition Area",
      "Moves Into Channels",
      "Gets Forward Whenever Possible",
      "Plays Short Simple Passes",
      "Tries Killer Balls Often",
      "Shoots From Distance",
      "Shoots With Power",
      "Places Shots",
      "Curls Ball",
      "Likes To Round Keeper",
      "Likes To Try To Beat Offside Trap",
      "Uses Outside Of Foot",
      "Marks Opponent Tightly",
      "Winds Up Opponents",
      "Argues With Officials",
      "Plays With Back To Goal",
      "Comes Deep To Get Ball",
      "Plays One-Twos",
      "Likes To Lob Keeper",
      "Dictates Tempo",
      "Attempts Overhead Kicks",
      "Looks For Pass Rather Than Attempting To Score",
      "Plays No Through Balls",
      "Stops Play",
      "Knocks Ball Past Opponent",
      "Moves ball to right foot before dribble attempt",
      "Moves ball to left foot before dribble attempt",
      "Dwells on ball",
      "Arrives Late In Opponent's Area",
      "Tries To Play Way Out Of Trouble",
      "Stays Back At All Times",
      "Avoids Using Weaker Foot",
      "Tries Tricks",
      "Tries Long Range Free Kicks",
      "Dives Into Tackles",
      "Does Not Dive Into Tackles",
      "Cuts Inside From Both Wings",
      "Hugs Line",
      "Gets Crowd Going",
      "Tries First Time Shots",
      "Tries Long Range Passes",
      "Likes Ball Played To Feet",
      "Hits Free Kicks With Power",
      "Likes To Beat Opponent Repeatedly",
      "Likes To Switch Ball To Other Flank",
      "Will retire at top",
      "Will play football as long as possible",
      "Possess Long Flat Throw",
      "Runs With Ball Often",
      "Runs With Ball Rarely",
      "Attempts to develop weaker foot",
      "Does Not Move Into Channels",
      "Uses long throw to start counter attack",
      "Refrains From Taking Long Shots",
      "Cuts inside from left wing",
      "Cuts inside from right wing",
      "Crosses Early",
      "Brings Ball Out Of Defense",
      "Plays ball with feet"
    )
  
  hairColor <- c(
    "Light Brown 1" = "LBR1",
    "Light Brown 2" = "LBR2",
    "Light Brown 3" = "LBR3",
    "Dark Brown 1" = "DBR1",
    "Dark Brown 2" = "DBR2",
    "Dark Brown 3" = "DBR3",
    "Blond(e) 1" = "BLN1",
    "Blond(e) 2" = "BLN2",
    "Blond(e) 3" = "BLN3",
    "Black 1" = "BLK1",
    "Black 2" = "BLK2",
    "Black 3" = "BLK3",
    "Red 1" = "RED1",
    "Red 2" = "RED2",
    "Red 3" = "RED3"
  )
  
  positions <- c(
    "LD" = "Left Defender", "CD" = "Central Defender", "RD" = "Right Defender",
    "LWB" = "Left Wing Back", "CDM" = "Defensive Midfielder", "RWB" = "Right Wing Back",
    "LM" = "Left Midfielder", "CM" = "Central Midfielder", "RM" = "Right Midfielder",
    "LAM" = "Left Attacking Midfielder", "CAM" = "Central Attacking Midfielder",
    "RAM" = "Right Attacking Midfielder", "ST" = "Striker"
  )
  
  positionsGK <- c(positions, "GK" = "Goalkeeper")
  
  traits <- 
    temp$traits |> 
    str_split(pattern = ", ", simplify = TRUE) |> 
    str_to_lower()
  
  traits <- 
    ((jsonTraits |> str_to_lower()) %in% traits) |> 
    which()
  
  traits <- 
    sum(as.integer64(2)^(traits-1))
  
  gkAtt <- 
    sapply(
      temp |> 
        select(`aerial reach`:throwing),
      FUN = function(x) {if_else(is.na(x), 5, x)}
    )
  
  mentalAtt <- 
    sapply(
      temp |> 
        select(`aggression`:`work rate`),
      FUN = function(x) {if_else(is.na(x), 5, x)}
    ) 
  
  physicalAtt <- 
    sapply(
      temp |> 
        select(`acceleration`:`strength`),
      FUN = function(x) {if_else(is.na(x), 5, x)}
    )
  
  technicalAtt <- 
    sapply(
      temp |> 
        select(`corners`:`technique`),
      FUN = function(x) {if_else(is.na(x), 5, x)}
    )
  
  positions <- 
    sapply(
      temp |> 
        select(`pos_st`:`pos_gk`),
      FUN = function(x) {if_else(is.na(x)|x == 0, 1, as.numeric(x))}
    ) 
  
  paste(
    '{"GoalKeeperAttributes":{
          ',
    paste(paste('"', names(gkAtt) |> str_to_title(), '"', sep = ""), gkAtt, sep = ":", collapse = ",") |> 
      str_replace_all(pattern = " ", replacement = "") |> 
      str_replace(pattern = "TendencyToRush", replacement = "RushingOut") |> 
      str_replace(pattern = "AerialReach", replacement = "AerialAbility"),
    '},
"MentalAttributes":{
          ',
    paste(paste('"', names(mentalAtt) |> str_to_title(), '"', sep = ""), mentalAtt, sep = ":", collapse = ",") |> 
      str_replace_all(pattern = " ", replacement = "") |> 
      str_replace(pattern = "WorkRate", replacement = "Workrate"),
    '},
"PhysicalAttributes":{
          ',
    paste(paste('"', names(physicalAtt) |> str_to_title(), '"', sep = ""), physicalAtt, sep = ":", collapse = ",") |> 
      str_replace_all(pattern = " ", replacement = "") |> 
      str_replace(pattern = "JumpingReach", replacement = "Jumping"),
    ',"LeftFoot":', temp$`left foot`,
    ',"RightFoot":', temp$`right foot`,
    '},
"TechnicalAttributes":{
          ',
    paste(paste('"', names(technicalAtt) |> str_to_title(), '"', sep = ""), technicalAtt, sep = ":", collapse = ",") |> 
      str_replace_all(pattern = " ", replacement = "") |> 
      str_replace(pattern = "FreeKick", replacement = "Freekicks") |> 
      str_replace(pattern = "LongThrows", replacement = "Longthrows"),
    '},
"Positions":{
          ',
    paste(
      paste('"', 
            positionsGK[
              sapply(
                (names(positions) |> 
                   str_remove_all(pattern = "pos_") |> 
                   str_to_upper()
                ), 
                FUN = function(x) which(names(positionsGK) == x)) |> unlist()
            ], '"', sep = ""), 
      positions, 
      sep = ":", 
      collapse = ","
    ) |> 
      str_replace_all(pattern = " ", replacement = "") |> 
      str_replace_all(pattern = "AttackingMidfielder", replacement = "AttackingMid") |> 
      str_replace_all(pattern = "Wingback", replacement = "WingBack") |> 
      str_replace_all(pattern = "(Left|Right|Central)([A-Za-z]+)(\":\"?[0-9]+)", replacement = "\\2\\1\\3"),
    '},
"HairColour":"', 
    ifelse(temp$hair_color |> nchar() == 4,
           hairColor[hairColor == temp$hair_color] |> 
             names() |> 
             str_replace_all(pattern = " |\\(|\\)", replacement = ""),
           temp$hair_color |> 
             str_replace_all(pattern = " |\\(|\\)", replacement = "")
    ), 
    '","HairLength":"', temp$hair_length |> str_to_title(), 
    '","SkinColour":', temp$skintone, 
    ',"Height":', 
    if_else(
      !is.na(temp$height |> as.numeric()), 
      (temp$height |> as.numeric()*2.54) |> as.character(), 
      temp$height |> as.character()
    ),
    ',"Weight":', 
    if_else(
      !is.na(temp$weight |> as.numeric()), 
      (temp$weight |> as.numeric()*0.453592) |> as.character(), 
      temp$weight |> as.character()
    ),
    ',"PreferredMoves":', traits,
    ', "Nation.Id":', temp$nationalityID,
    ',"Born":', 
    ## OLD
    # ("2004-07-01" |> as_date()) - years(currentSeason$season - (temp$class |> str_extract(pattern = "[0-9]+") |> as.numeric())) ,
    ## NEW: Changed to give all players the same age
    '"2000-01-01",
    "PersonalityAttributes": {
      "Adaptability": 20,
      "Ambition": 20,
      "Loyalty": 20,
      "Pressure": 15,
      "Professional": 15,
      "Sportsmanship": 15,
      "Temperament": 5,
      "Controversy": 5
    },
    "HiddenAttributes": {
      "Consistency": 15,
      "Dirtiness": 5,
      "ImportantMatches": 15,
      "InjuryProness": 1,
      "Versatility": 10
    },
    "PA": 200,',
    '"DocumentType":"Player"}',
    sep = ""
  )
}

data <- 
  playerData |> 
  filter(
    name %in% (attributes$name |> unique())
  )

map(data$name, .f = function(x) {
  current <- data |> 
    filter(name == x)
  
  team <- current$teamName
  
  writeLines(
    downloadPlayer(current),
    file.path("D:/Downloads/Builds", glue("{team}_{x}_build.json"))
  )
})










