## Loading data
require(dplyr)
require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)
require(tidyr)
require(sslrtools)

## Adding a deauthorization for reading of Google Sheets that are still being used. 
googlesheets4::gs4_deauth()

## Downloads a local file for the database
dbFile <- tempfile(fileext = ".db")

dbUrl <- ("https://github.com/canadice/ssl-index/blob/main/database/SSL_Database.db?raw=true")

download.file(dbUrl, destfile = dbFile, mode = "wb")

con <- 
  dbConnect(
    SQLite(), 
    dbFile
    )


##################################################################
##           Loading tables with information                    ##
##################################################################

## Doesn't necessarily have to load here. Need to check when it is being used.
positionalCoord <- 
  dbGetQuery(con, "SELECT * FROM positionalCoord")

tpeCost <- 
  dbGetQuery(con, "SELECT * FROM tpeCost")


## These are only used when scraping data
roleMatrix <- 
  dbGetQuery(con, "SELECT * from Duty_and_Role_Matrix")

abilityMatrix <- 
  dbGetQuery(con, "SELECT * from Current_Ability_Calculation_Matrix")

## These are used when grouping the attributes in the player builder and update tool
attributes <- 
  dbGetQuery(con, "SELECT * from Attributes_And_Availability") %>% 
  mutate(
    abbr = 
      c(
        "Acc", "Agi", "Bal", "Jum", "Nat", "Pac", "Sta", "Str",
        "Cor", "Cro", "Dri", "Fin", "Fir", "Fre", "Hea", "Lon", "L Th", "Mar", "Pas" , "Pen", "Tck", "Tec",
        "Agg", "Ant", "Bra", "Cmp", "Cnt", "Dec", "Det", "Fla", "Ldr", "OtB", "Pos", "Tea", "Vis", "Wor",
        "Aer", "Cmd", "Com", "Ecc", "Han", "Kic", "1v1", "Pun", "Ref", "TRO", "Thr"
      )
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

teamInfo <-
  dbGetQuery(con, "SELECT * from Team_Information")

pitch <- 
  try(
    image_read_svg(
      path = "https://raw.githubusercontent.com/canadice/ssl-index/main/graphics/pitch.svg"
    ),
    silent = TRUE
  )

playerData <- 
  dbGetQuery(con, "SELECT * from Daily_Scrape") %>%
  group_by(Name) %>% 
  dplyr::mutate(
    DEFENDING =
      sum(c(Marking,Tackling,Positioning) %>% replace_na(5))/3,
    PHYSICAL =
      sum(c(Agility, Balance, Stamina, Strength) %>% replace_na(5))/4,
    SPEED =
      sum(c(Acceleration, Pace)%>% replace_na(5))/2,
    VISION =
      sum(c(Passing, Flair , Vision)%>% replace_na(5))/3,
    ATTACKING =
      sum(c(Finishing , Composure , `Off the Ball`)%>% replace_na(5))/3,
    TECHNICAL =
      sum(c(Dribbling, `First Touch`, Technique)%>% replace_na(5))/3,
    AERIAL =
      sum(c(Heading , `Jumping Reach`)%>% replace_na(5))/2,
    MENTAL =
      sum(c(Anticipation, Bravery, Concentration, Decisions, Determination, Teamwork)%>% replace_na(5))/6
  ) %>% 
  ungroup() %>% 
  mutate(
    Created = as.Date(Created, origin = "1970-01-01"),
    lastPost = as.Date(lastPost, origin = "1970-01-01")
  )

## Loads game data
keeperGameData <- 
  dbGetQuery(con, "SELECT * from gameDataKeeper") %>%
  mutate(
    Season = as.numeric(Season)
  )

playerGameData <- 
  dbGetQuery(con, "SELECT * from gameDataPlayer") %>% 
  mutate(
    Season = as.numeric(Season)
  )

##################################################################
##                      Loading Index data                      ##
##################################################################
# 
# schedule <- 
#   lapply(
#     X = 1:max(playerGameData$Season),
#     FUN = function(x){
#       read_sheet(
#         ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing", 
#         sheet = paste("Season", x)
#       )  
#     }
#   )


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
