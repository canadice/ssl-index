## Loading data
require(dplyr)
require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)


con <- dbConnect(SQLite(), "../database/SSL_Database.db")


##################################################################
##                    Positional Coordinates                    ##
##################################################################
{positionalCoord <- 
  data.frame(
    x = 
      c(
        375, 
        130,375,620,
        130,375,620,
        130,375,620,
        130,375,620,
        375
      ),
    y = 
      c(
        775,
        625,625,625,
        455,455,455,
        310,310,310,
        150,150,150,
        50
      ),
    Position = 
      c(
        "Striker",
        "Attacking Midfielder L",
        "Attacking Midfielder C",
        "Attacking Midfielder R",
        "Midfielder L",
        "Midfielder C",
        "Midfielder R",
        "Wingback L",
        "Defensive Midfielder",
        "Wingback R",
        "Defender L",
        "Defender C",
        "Defender R",
        "Goalkeeper"
      )
  )
}
#################################################################
##                       TPE Cost Matrix                       ##
#################################################################

tpeCost <- 
  data.frame(
    value = 5:20,
    ## -2 comes from an initial error in the costs as 5 is free (starting value)
    cost = c(2,4,6,10,14,18,24,30,36,48,60,72,90,108,133,158)-2
  )
#################################################################
##                       Attribute names                       ##
#################################################################

attributeNames <- 
  c(
    "Aggression",
    "Anticipation",
    "Bravery",
    "Composure",
    "Concentration",
    "Decisions",
    "Determination",
    "Flair",
    "Leadership",
    "OffTheBall",
    "Positioning",
    "Teamwork",
    "Vision",
    "WorkRate",
    "Acceleration",
    "Agility",
    "Balance",
    "JumpingReach",
    "NaturalFitness",
    "Pace",
    "Stamina",
    "Strength",
    "Corners",
    "Crossing",
    "Dribbling",
    "Finishing",
    "FirstTouch",
    "FreeKick",
    "Heading",
    "LongShots",
    "LongThrows",
    "Marking",
    "Passing",
    "PenaltyTaking",
    "Tackling",
    "Technique"
  )

goalieAttributeNames <- 
  c(
    "GoalieAggression",
    "GoalieAnticipation",
    "GoalieBravery",
    "GoalieComposure",
    "GoalieConcentration",
    "GoalieDecisions",
    "GoalieDetermination",
    "GoalieFlair",
    "GoalieLeadership",
    "GoalieOffTheBall",
    "GoaliePositioning",
    "GoalieTeamwork",
    "GoalieVision",
    "GoalieWorkRate",
    "GoalieAcceleration",
    "GoalieAgility",
    "GoalieBalance",
    "GoalieJumpingReach",
    "GoalieNaturalFitness",
    "GoaliePace",
    "GoalieStamina",
    "GoalieStrength",
    "GoalieFirstTouch",
    "GoalieFreeKick",
    "GoaliePassing",
    "GoaliePenaltyTaking",
    "GoalieTechnique",
    "GoalieAerialReach",
    "GoalieCommandOfArea",
    "GoalieCommunication",
    "GoalieEccentricity",
    "GoalieHandling",
    "GoalieKicking",
    "GoalieOneOnOnes",
    "GoalieTendencyToPunch",
    "GoalieTendencyToRush",
    "GoalieReflexes",
    "GoalieThrowing"
  )


##################################################################
##             Loading data sets from Google Sheets             ##
##################################################################


teamInfo <-dbGetQuery(con, "SELECT * from Team_Information")

pitch <- 
  try(
    image_read_svg(
      path = "https://raw.githubusercontent.com/canadice/ssl-index/main/graphics/pitch.svg"
    ),
    silent = TRUE
  )

playerData <- dbGetQuery(con, "SELECT * from Daily_Scrape")  %>%
  mutate(
    DEFENDING =
      (Marking + Tackling + Positioning)/3,
    PHYSICAL =
      (Agility + Balance + Stamina + Strength)/4,
    SPEED =
      (Acceleration + Pace)/2,
    VISION =
      (Passing + Flair + Vision)/3,
    ATTACKING =
      (Finishing + Composure + Off_the_Ball)/3,
    TECHNICAL =
      (Dribbling + First_Touch + Technique)/3,
    AERIAL =
      (Heading + Jumping_Reach)/2,
    MENTAL =
      (Anticipation + Bravery + Concentration + Decisions + Determination + Teamwork)/6
  ) %>%
  mutate(
    across(
      DEFENDING:MENTAL,
      ~ floor(.x)
    )
  )

## Loads game data
keeperGameData <- dbGetQuery(con, "SELECT * from Keeper_Game_Data") %>%
  mutate(
    Season = as.numeric(Season)
  )

playerGameData <- dbGetQuery(con, "SELECT * from Player_Game_Data") %>% 
  mutate(
    Season = as.numeric(Season)
  )

roleMatrix <- dbGetQuery(con, "SELECT * from Duty_and_Role_Matrix")


abilityMatrix <- dbGetQuery(con, "SELECT * from Current_Ability_Calculation_Matrix")

attributes <- dbGetQuery(con, "SELECT * from Attributes_And_Availability")


##################################################################
##                      Loading Index data                      ##
##################################################################

url <- 
  paste(
    "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/S",
    1:3,
    "_standings.html",
    sep = ""
  )

readStandings <- function(x) {
  x %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    .[[1]] %>% 
    dplyr::rename(
      GP = Pld,
      W = Won,
      D = Drn,
      L = Lst,
      GF = For,
      GA = Ag
    ) %>% 
    select(
      -`Inf`,
      -Form
    ) %>% 
    left_join(
      teamInfo %>% 
        select(
          team, 
          color_primary,
          color_secondary
        ),
      by = c("Team" = "team")
    )
}

standings <- lapply(X = url, FUN = readStandings)

### The Github exports from FM are not used. Game Logs are used instead.# 
{
# outfieldUrl <- 
#   paste(
#     "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/S",
#     1:3,
#     "_playerStats.html",
#     sep = ""
#   )
# 
# goalieUrl <-
#   paste(
#     "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/S",
#     1:3,
#     "_goalieStats.html",
#     sep = ""
#   )
# 
# readPlayerStats <- function(x){
#   x %>% 
#     read_html() %>% 
#     html_elements("table") %>% 
#     html_table() %>% 
#     .[[1]] %>% 
#     dplyr::rename(
#       Apps = Apps,
#       Goals = Gls,
#       Assists = Ast,
#       `Minutes Played` = Mins,
#       `Attempted Passes` = `Pas A`,
#       `Successful Passes` = `Ps C`,
#       `Key Passes` = `K Pas`,
#       `Successful Crosses` = `Cr C`,
#       `Attempted Crosses` = `Cr A`,
#       `Chances Created` = CCC,
#       `Tackles Won` = `Tck W`,
#       `Tackle%` = `Tck R`,
#       `Key Tackles` = `K Tck`,
#       `Successful Headers` = Hdrs,
#       `Attempted Headers` = `Hdrs A`,
#       `Header%` = `Hdr %`,
#       `Key Headers` = `K Hdrs`,
#       `Shots on Target` = ShT,
#       `Mistakes Leading to Goals` = `Gl Mst`,
#       Dribbles = `Drb`,
#       Offsides = Off,
#       `Fouls Against` = FA,
#       Interceptions = Itc,
#       `Yellow Cards` = Yel,
#       `Red Cards` = Red,
#       Fouls = Fls,
#       `Penalties Taken` = Pens,
#       `Penalties Scored` = `Pens S`,
#       `Distance Run (km)` = Distance,
#       `Average Rating` = `Av Rat`,
#       `Player of the Match` = `PoM`
#     ) %>% 
#     dplyr::mutate(
#       Starts = (str_split(Apps, pattern = "\\(", simplify = TRUE)[,1] %>% 
#                   as.numeric()),
#       Subs = (str_split(Apps, pattern = "\\(", simplify = TRUE)[,2] %>% 
#                 str_extract(pattern = "[0-9]+") %>% 
#                 as.numeric())
#     ) %>% 
#     dplyr::mutate(
#       Apps = rowSums(data.frame(.$Starts, .$Subs), na.rm = TRUE)
#     ) %>%
#     dplyr::select(
#       -Starts, -Subs
#     ) %>%
#     dplyr::mutate(
#       across(
#         c(
#           `Minutes Played`:`Average Rating`
#         ),
#         .fns = str_replace_all,
#         pattern = "[^\\d\\.]+",
#         replacement = ""
#       )
#     ) %>% 
#     dplyr::mutate(
#       Name = 
#         case_when(
#           str_detect(Name, "GFuel") ~ "FazeBerry GFuel - American",
#           TRUE ~ Name
#         )
#     ) %>% 
#     dplyr::mutate(
#       `Pass%` = (`Successful Passes` %>% as.numeric()/`Attempted Passes` %>% as.numeric()) %>% round(4)*100,
#       `Header%` = (`Successful Headers` %>% as.numeric()/`Attempted Headers` %>% as.numeric()) %>% round(4)*100,
#       # Position = NA,
#       Nationality = 
#         Name %>% 
#         str_split(
#           pattern = " - ", 
#           simplify = TRUE
#         ) %>% 
#         .[,2],
#       Name = 
#         Name %>% 
#         str_split(
#           pattern = " - ", 
#           simplify = TRUE
#         ) %>% 
#         .[,1],
#       `Cross%` = (`Successful Crosses` %>% as.numeric()/`Attempted Crosses` %>% as.numeric()) %>% round(4)*100,
#       # `Header%` =
#       #   `Header%` %>% 
#       #   as.numeric(),
#       `Tackle%` =
#         `Tackle%` %>% 
#         as.numeric(),
#       `Distance Run (km)` = 
#         `Distance Run (km)` %>% 
#         as.numeric(),
#       Club =
#         Club %>%
#         str_split(pattern = "-", simplify = TRUE) %>% 
#         .[,1] %>% 
#         str_squish()
#     ) %>% 
#     dplyr::mutate(
#       across(
#         !contains(
#           c("Name", "Information", "Nationality", "Position", "Club")
#         ),
#         as.numeric
#       ),
#       `Attempted Tackles` = ((`Tackles Won` %>% as.numeric())/(`Tackle%`/100)) %>% round(0)
#     ) %>% 
#     relocate(
#       c(
#         Nationality,
#         Position,
#         Apps,
#         `Minutes Played`,
#         `Distance Run (km)`
#       ),
#       .after = Name
#     ) %>% 
#     relocate(
#       c(
#         `Attempted Passes`,
#         `Pass%`
#       ),
#       .after = `Successful Passes`
#     ) %>% 
#     relocate(
#       c(
#         `Attempted Crosses`,
#         `Cross%`
#       ),
#       .after = `Successful Crosses`
#     ) %>% 
#     relocate(
#       c(
#         `Attempted Headers`
#       ),
#       .after = `Successful Headers`
#     ) %>% 
#     relocate(
#       c(
#         `Attempted Tackles`,
#         `Tackle%`
#       ),
#       .after = `Tackles Won`
#     ) %>% 
#     relocate(
#       `Shots on Target`,
#       .before = `Shots`
#     ) %>% 
#     select(
#       -`Inf`,
#       -`Tck A`,
#       -Rec
#     ) %>% 
#     arrange(
#       `Average Rating` %>% desc()
#     ) %>% 
#     # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
#     suppressWarnings()
# }
# 
# readKeeperStats <- function(x){
#   x %>% 
#     read_html() %>% 
#     html_elements("table") %>% 
#     html_table() %>% 
#     .[[1]] %>% 
#     dplyr::rename(
#       Apps = Apps,
#       `Minutes Played` = Mins,
#       Drawn = D,
#       Conceded = Conc,
#       `Saves Parried` = Svp,
#       `Saves Held`= Svh,
#       `Saves Tipped` = Svt,
#       `Penalties Saved` = `Pens Saved`,
#       `Penalties Faced` = `Pens Faced`,
#       `Average Rating` = `Av Rat`,
#       `Player of the Match` = `PoM`,
#       # `Clean Sheets` = `Clean sheets`
#       `Clean Sheets` = Shutouts
#     ) %>% 
#     dplyr::mutate(
#       across(
#         c(
#           `Minutes Played`:`Average Rating`
#         ),
#         .fns = str_replace_all,
#         pattern = "[^\\d\\.]+",
#         replacement = ""
#       )
#     ) %>% 
#     dplyr::mutate(
#       Nationality = 
#         Name %>% 
#         str_split(
#           pattern = " - ", 
#           simplify = TRUE
#         ) %>% 
#         .[,2],
#       Name = 
#         Name %>% 
#         str_split(
#           pattern = " - ", 
#           simplify = TRUE
#         ) %>% 
#         .[,1],
#       Club =
#         Club %>%
#         str_split(pattern = "-", simplify = TRUE) %>% 
#         .[,1] %>% 
#         str_squish()
#     ) %>% 
#     dplyr::mutate(
#       across(
#         !contains(
#           c("Name", "Information", "Nationality", "Position", "Club")
#         ),
#         as.numeric
#       )
#     ) %>% 
#     dplyr::mutate(
#       `Save%` = ((`Saves Parried`+`Saves Held`+`Saves Tipped`)/(`Saves Parried`+`Saves Held`+`Saves Tipped`+Conceded)) %>% round(4) * 100
#     ) %>% 
#     relocate(
#       `Save%`,
#       .after = `Saves Tipped`
#     ) %>% 
#     relocate(
#       c(
#         Nationality,
#         Position,
#         Apps,
#         `Minutes Played`,
#       ),
#       .after = Name
#     ) %>% 
#     select(
#       -`Inf`,
#       -Rec
#     ) %>% 
#     arrange(
#       `Average Rating` %>% desc()
#     ) %>% 
#     # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
#     suppressWarnings()
# }
# 
# playerStats <- lapply(outfieldUrl, readPlayerStats)
# 
# goalieStats <- lapply(goalieUrl, readKeeperStats)
}

dbDisconnect(con)
