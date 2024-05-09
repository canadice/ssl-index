###########################################################################
###########################################################################
###                                                                     ###
###                    SETTING UP A SQLLITE DATABASE                    ###
###                                                                     ###
###########################################################################
###########################################################################

require(dplyr)
require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)
require(RMySQL)
require(tidyverse)

con <- dbConnect(SQLite(), "database/SSL_Database.db")

portalcon <- 
  dbConnect(
    MySQL(),
    dbname = "portaldb",
    host = "localhost",
    port = 3306,
    user = Sys.getenv("SQL_USER"),
    password = Sys.getenv("SQL_PASS")
  )

indexcon <- 
  dbConnect(
    MySQL(),
    dbname = "indexdb",
    host = "localhost",
    port = 3306,
    user = Sys.getenv("SQL_USER"),
    password = Sys.getenv("SQL_PASS")
  )
 
dbConnect(SQLite(), "database/SSL_Database.db")

dbListTables(con)

#################################################################
##              Insert Daily Scrape into DB                    ##
#################################################################

tbl(con, "Daily_Scrape") %>% 
  collect() %>% 
  mutate(
    uid = Userlink %>% str_extract(pattern = "uid=[0-9]+") %>% str_split(pattern = "=", simplify = TRUE) %>% .[,2] %>% as.integer(),
    pid = Playerlink %>% str_extract(pattern = "tid=[0-9]+") %>% str_split(pattern = "=", simplify = TRUE) %>% .[,2] %>% as.integer(),
    footedness = 
      case_when(
        `Preferred Foot` == "Right" ~ "10 | 20",
        `Preferred Foot` == "Left"  ~ "20 | 10",
        TRUE ~ `Preferred Foot`
      ),
    Height = Height %>% as.integer(),
    Weight = Weight %>% as.integer(),
    TPE = TPE %>% as.integer(),
    position = 
      case_when(
        `Preferred Position` == "Striker"                  ~ "ST",
        `Preferred Position` == "Defensive Midfielder"     ~ "CDM",
        `Preferred Position` == "Goalkeeper"               ~ "GK",
        `Preferred Position` == "Defender [C]"             ~ "CD",
        `Preferred Position` == "Wingback [L]"             ~ "LWB",
        `Preferred Position` == "Midfielder [C]"           ~ "CM",
        `Preferred Position` == "Defense [L]"              ~ "LD",
        `Preferred Position` == "Defense [R]"              ~ "RD",
        `Preferred Position` == "Defense [C]"              ~ "CD",
        `Preferred Position` == "Attacking Midfielder [C]" ~ "CAM",
        `Preferred Position` == "Defensive Midfielder [C]" ~ "CDM",
        `Preferred Position` == "Attacking Midfielder [R]" ~ "RAM",
        `Preferred Position` == "Attacking Midfielder [L]" ~ "LAM",
        `Preferred Position` == "Wingback [R]"             ~ "RWB",
        `Preferred Position` == "Midfielder [L]"           ~ "LM",
        `Preferred Position` == "Midfielder C"             ~ "CM",
        `Preferred Position` == "Wingback L"               ~ "LWB",
        `Preferred Position` == "Fullback [R]"             ~ "RWB",
        `Preferred Position` == "Defender [R]"             ~ "RD",
        `Preferred Position` == "Defender C"               ~ "CD",
        `Preferred Position` == 
          "Midfielder [L]/Attacking Midfielder [L]"        ~ "LAM",
        `Preferred Position` == "Fullback [L]"             ~ "LWB",
        `Preferred Position` == "Centre Back"              ~ "CD",
        `Preferred Position` == "Attacking Midfielder R"   ~ "RAM",
        `Preferred Position` == "Attacking Midfielder C"   ~ "CAM",
        `Preferred Position` == "Attacking Midfielder L"   ~ "LAM",
        `Preferred Position` == "Defender L"               ~ "LD",
        `Preferred Position` == "Midfielder L"             ~ "LM",
        `Preferred Position` == "Defender R"               ~ "RD",
        `Preferred Position` == "Wingback R"               ~ "RWB",
        `Preferred Position` == "Midfielder R"             ~ "RM",
        `Preferred Position` == "Winger"                   ~ "RAM",
        TRUE ~ `Preferred Position`
      ),
    skintone = `Skin Tone` %>% as.integer(),
    status_p = if_else(Team == "Retired", 0, 1),
    status_u = if_else(Active == "IA", 0, 1),
    render = if_else(`Player Render` %>% is.na(), Render, `Player Render`)
    # foot_left = if_else(`Preferred Foot` == "Left", 20, 10),
    # foot_right = if_else(`Preferred Foot` == "Right", 20, 10)
  ) %>% 
  select(
    uid,
    pid,
    status_p,
    first = `First Name`,
    last = `Last Name`,
    Name,
    Class,
    Created,
    TPE,
    Team,
    Birthplace,
    Nationality,
    Height,
    Weight,
    hair_color = `Hair Color`,
    hair_length = `Hair Length`,
    skintone,
    render,
    footedness,
    position,
    pos_st = Striker,
    pos_lam = `Attacking Midfielder [L]`,
    pos_cam = `Attacking Midfielder [C]`,
    pos_ram = `Attacking Midfielder [R]`,
    pos_lm = `Midfielder [L]`,
    pos_cm = `Midfielder [C]`,
    pos_rm = `Midfielder [R]`,
    pos_lwb = `Wingback [L]`,
    pos_cdm = `Defensive Midfielder [C]`,
    pos_rwb = `Wingback [R]`,
    pos_ld = `Defense [L]`,
    pos_cd = `Defense [C]`,
    pos_rd = `Defense [R]`,
    pos_gk = Goalkeeper,
    Acceleration:Throwing,
    traits = `All Traits`
  ) %>% 
  mutate(
    across(
      contains("pos_"),
      ~ as.integer(.x) %>% 
        replace_na(0)
    ),
    across(
      Acceleration:Throwing,
      ~ as.integer(.x) 
    )
  ) %>% 
  rename_with(
    tolower
  ) %>% 
  dbWriteTable(
    conn = portalcon,
    name = "playerdata",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )

## Creates an empty approved player data for newly created players to go to
tbl(portalcon, "playerdata") %>% 
  collect() %>% 
  filter(uid == 400) %>% 
  dbWriteTable(
    conn = portalcon,
    name = "approvecreate",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )

## Regression run
tbl(portalcon, "playerdata") %>%
  collect() %>% 
  mutate(
    age = 
      (currentSeason + 1 - 
         (str_extract(class, pattern = "[0-9]+") %>% as.numeric())) %>% 
      unlist()
  ) %>% 
  filter(
    age > 8,
    status_p == 1
  ) %>% 
  mutate(
    perc = 
      case_when(
        # Regression scale based on age
        age < 11 ~ 0.10,
        age < 12 ~ 0.15,
        age < 13 ~ 0.20,
        age < 14 ~ 0.25,
        age < 15 ~ 0.30,
        TRUE ~ 0.40
      ),
    tpe = tpe - ((tpe * perc) %>% ceiling())
  ) %>% 
  select(
    pid,
    tpe
  ) %>% 
  dbWriteTable(
    conn = portalcon,
    name = "regression",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
 )
  
dbExecute(portalcon,
          "update playerdata q
            inner join regression a
          on q.pid = a.pid
          set q.tpe = a.tpe
          where q.pid = a.pid;")

dbExecute(portalcon,
          "ALTER TABLE `portaldb`.`playerdata` 
CHANGE COLUMN `pid` `pid` INT NOT NULL AUTO_INCREMENT ,
ADD PRIMARY KEY (`pid`);
;")



#################################################################
##              Duty and Role Matrix                           ##
#################################################################

# ability <- googlesheets4::read_sheet(
#   ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
#   sheet = "Duty and Role Matrix"
# )

#################################################################
##              Insert Team Information                        ##
#################################################################

teamInfo <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1dCOGjnLrtgYjO43Zz1dYkuQ5ZQRpION3LQiRrooMZaQ/edit#gid=731383221",
  sheet = "Team Information"
)
dbWriteTable(indexcon, "teaminformation", teamInfo, overwrite = TRUE)

#################################################################
##             Insert Attributes and Availability              ##
#################################################################

attributes %>% 
  mutate(
    attribute = str_to_title(attribute),
    explanation = 
      c(
        "The ability to quickly reach their top speed.",
        "The ability to quickly change direction in their movement.",
        "The ability to hold their balance when being subjected to physical pressure.",
        "The maximum height a player can reach when jumping.",
        "Locked at 20.",
        "The top speed.",
        "Locked at 20.",
        "The ability to put physical pressure on another player.",
        "The ability to hit the intended target off a corner.",
        "The ability to hit the intended target when crossing the ball from the wide areas of the pitch.",
        "The ability to control the ball while moving.",
        "The ability to hit the intended target of the goal with a shot.",
        "The ability to control and set up the ball after receiving it.",
        "The ability to hit the intended target with a free kick.",
        "The ability to hit the intended target with a header.",
        "The ability to hit the intended target with a shot from outside the opposition's penalty area.",
        "The length a player can throw the ball and the ability to hit the intended target with a long throw.",
        "The ability to mark an opponent.",
        "The ability to hit their intended target with a pass.",
        "The ability to hit their intended target on a penalty kick.",
        "The ability to get the ball from the opponent without incurring a foul.",
        "The ability to successfully perform advanced technical actions.",
        "The likelihood of the player to get into physical situations and the amount of force they use.",
        "The ability to anticipate teammates' and opposition movements and actions.",
        "The willingness of the player to get into areas where they risk an injury.",
        "The ability to remain unaffected while being under mental pressure.",
        "The ability to keep focus later in the game.",
        "The ability to decide the best action to perform throughout a game.",
        "The willingness of the player to try and perform even when their team is losing or the player is not performing well.",
        "The ability to do something unexpected with the ball.",
        "How inspirational and motivational a player is to their teammates.",
        "The ability to position themselves while their team has possession (Offensive).",
        "The ability to position themselves while the opposing team has possession (Defensive).",
        "The ability to follow tactical instructions and the positions of other players on the pitch.",
        "The ability to see what opportunities exist while in possession of the ball.",
        "The amount of physical effort that is used during the match.",
        "The height a goalkeeper can reach when jumping. Equivalent to outfielders Jumping Reach.",
        "The ability to take control of balls in the box.",
        "The ability to communicate with teammates and control the defensive organization.",
        "The likelihood to do something unexpected or risky.",
        "The ability to keep control of the ball when catching it.",
        "The maximum length of a kick.",
        "The ability to perform well when alone with an opponent.",
        "The ability to react to shots.",
        "The ability to assess when to rush out.",
        "The likelihood to punch instead of catching the ball.",
        "The ability to hit the intended target when throwing the ball."
      )
  ) %>% 
  rename_with(
    .fn = str_to_lower
  ) %>% 
  dbWriteTable(
    conn = portalcon,
    name = "attributes",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )

temp <- 
  tbl(indexcon, "gamedataoutfield") %>% 
  select(apps:`goals outside box`) %>% 
  collect() %>% 
  colnames() 

tibble(
  statistic = 
    c(
      temp[!(temp == "goals outside b")],
      "press%",
      "open play crosses%",
      "shot accuracy%",
      "pen adj xG",
      "name",
      "club",
      "position",
      "division",
      "season",
      "result",
      "opponent"
    )
  ,
  explanation = 
    c(
      "Appearances",
      "Total minutes played",
      "Total kilometers covered",
      "Average match grade given by FM",
      "Player of the Match awards given by FM",
      "Goals scored",
      "Passes that directly led to a goal",
      "Expected number of goals from made shots based on the strength of chances",
      "Shots hitting the goal, post/crossbar or saved by the keeper",
      "Attempted shots",
      "Total penalties taken",
      "Penalties that resulted in a goal",
      "Passes that reached a teammate",
      "Attempted passes to a teammate",
      "Passing %",
      "A pass made to a teammate that results in an attempted shot on goal",
      "Crosses that reached a teammate",
      "Attempted crosses to a teammate",
      "Crossing %",
      "Number of chances created from passes or crosses",
      "Aerial challenges that the player won",
      "Attempted aerial challenges",
      "Heading %",
      "Headers won in key situations",
      "Number of successful dribbles with the ball past an opponent",
      "Tackles made that resulted in the opponent losing the ball",
      "Attempted tackles on an opponent with the ball",
      "Tackling %",
      "Tackles won in key situations",
      "Interceptions of opponent's passes",
      "Balls cleared from the own defensive end to reduce pressure from the opponent",
      "Mistake leading to the opponent's scoring a goal",
      "Accumulated yellow cards",
      "Accumulated red cards",
      "Fouls made by the player, includes if the player has run offside",
      "Fouls made against the player by an opponent. Does not include opponent's offsides as that type of foul is not made against a specific player",
      "Times the player has run offside",
      "Expected number of assists from made passes based on the strength of the attempted shot by the receiver",
      "Difference between the actual and expected number of goals. Positive value means the player has scored more than expected",
      "Shots made from free kicks",
      "A defensive action that deflects the ball",
      "A pass made to a teammate that results in an attempted shot on goal based only on open play",
      "Successful crosses made only in open play, excluding crosses from free kicks and corners",
      "Attempted crosses made only in open play, excluding crosses from free kicks and corners",
      "A defensive action that deflects a shot on target away from the goal",
      "Passes that move the ball up the field",
      "Situation where pressure is applied on an opponent, resulting in them losing the ball",
      "Situation where pressure is applied on an opponent",
      "Goals made from outside the box",
      "Pressure %",
      "Open Play Crossing %",
      "Shooting %",
      "Adjusts the expected goals for penalties as they are high expectation chances not from open play",
      "Player name",
      "Current club",
      "Primary and secondary positions",
      "Current division",
      "Current season",
      "Result of the match",
      "Opponent club"
    )
  ) %>% 
  rename_with(
    .fn = str_to_lower
  ) %>% 
  dbWriteTable(
    conn = indexcon,
    name = "statlegend",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )


#################################################################
##              Create update and tpe database                 ##
#################################################################

data.frame(
  time = now() %>% force_tz("US/Pacific") %>% as.numeric(),
  uid = 1,
  pid = 1,
  attribute = "",
  old = 5,
  new = 5
) %>% 
  dbWriteTable(
    portalcon,
    name = "updatehistory",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )

data.frame(
  time = now() %>% force_tz("US/Pacific") %>% as.numeric(),
  uid = 1,
  pid = 1,
  source = "",
  tpe = 0
) %>% 
  dbWriteTable(
    portalcon,
    name = "tpehistory",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )



#################################################################
##              Create Index Database                          ##
#################################################################
data.frame(
  season = 13,
  startDate = "2024-01-08",
  ended = 0
) %>% 
  dbWriteTable(
    indexcon,
    name = "seasoninfo",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )

#################################################################
##                    Move game table                          ##
#################################################################
gameData <-
  tbl(con, "gameDataPlayer") %>%
  collect() %>% 
  select(
    Name,
    Club,
    Position,
    Result:Division,
    Acc:Wor,
    Apps:Offsides,
    xA:`Attempted Presses`,
    `Goals Outside Box`
  ) %>% 
  mutate(
    across(
      c(
        `Player of the Match`:Assists, 
        `Shots on Target`:`Attempted Passes`, 
        `Key Passes`:`Attempted Crosses`, 
        `Chances Created`:`Attempted Headers`,
        `Key Headers`:`Attempted Tackles`,
        `Key Tackles`:Offsides,
        `FK Shots`:`Attempted Presses`,
        `Goals Outside Box`
      ),
      ~ as.integer(.x)
    ),
    Division = 
      case_when(
        str_detect(Season, pattern = "WSFC") ~ "WSFC",
        Division == "0" ~ "Cup",
        TRUE ~ Division
      ),
    Season = 
      case_when(
        str_detect(Season, pattern = "WSFC") ~ 12,
        TRUE ~ Season %>% as.numeric()
      ),
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  ) %>% 
  rename_with(
    tolower
  ) %>% 
  dbWriteTable(
    conn = indexcon,
    name = "gamedataoutfield",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )

#################################################################
##                Insert Positional Coordinates                ##
#################################################################

#################################################################
##                    Insert TPE Cost table                    ##
#################################################################

tpeCost %>% 
  mutate(
    cumCost = cost,
    sinCost = c(0,2,2,4,4,4,6,6,6,12,12,12,18,18,25,25)
  ) %>% 
  select(
    -cost
  ) %>% 
  dbWriteTable(
    conn = sqlcon,
    name = "tpetable",
    value = .,
    row.names = FALSE,
    overwrite = TRUE
  )

#################################################################
##                    Updating a table                    ##
#################################################################

dbExecute(con, 
          paste('DELETE FROM gameDataKeeper WHERE Season = "13"', 
                sep = "")
          )

dbExecute(con, "UPDATE gameDataKeeper
                SET Result = '2-3' 
                WHERE Club = 'CA Buenos Aires' AND Season = '12' AND Division = '0' AND Matchday = 'Semi Final Leg 2'")



keeperGameData <-
  tbl(con, "gameDataKeeper") %>%
  collect()

dbRemoveTable(
  sqlcon, 
  name = "teaminformation"
)

dbWriteTable(sqlcon, "teaminformation", teamInfo, row.names = FALSE, overwrite = TRUE)

dbDisconnect(con)
