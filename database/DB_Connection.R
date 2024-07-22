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
require(googlesheets4)

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

tpeTable <- tbl(portalcon, "tpetable") %>% 
  collect()

#################################################################
##              Insert Daily Scrape into DB                    ##
#################################################################

playerdata <- 
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
    tpeBank = `TPE Available`,
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
    skintone = `Skin Tone` %>% str_extract(pattern = "\\d+") %>% as.integer(),
    status_p = if_else(Team == "Retired", 0, 1),
    status_u = if_else(Active == "IA", 0, 1),
    render = `Player Render`,
    Affiliate = case_when(
      Team %in% c("Athênai F.C.", "Seoul MFC", "F.C. Kaapstad", "Cairo City", "North Shore United", "Inter London", "Montréal United", "AS Paris") ~ "Minor",
      TRUE ~ "Major"
    ),
    Team = case_when(
      Team == "Athênai F.C." ~ "CA Buenos Aires",
      Team == "Seoul MFC" ~ "CF Catalunya",
      Team == "F.C. Kaapstad" ~ "Hollywood FC",
      Team == "Cairo City" ~ "Tokyo S.C.",
      Team == "North Shore United" ~ "Reykjavik United",
      Team == "Inter London" ~ "A.C. Romana",
      Team == "Montréal United" ~ "Schwarzwälder FV",
      Team == "AS Paris" ~ "União São Paulo",
      Team == "Prospect" ~ "Academy",
      TRUE ~ Team
    ),
    rerollused = 0,
    redistused = 0,
    `left foot` = `footedness` %>% str_split("\\|", simplify = TRUE) %>% .[,1] %>% str_squish() %>% as.numeric(),
    `right foot` = `footedness` %>% str_split("\\|", simplify = TRUE) %>% .[,2] %>% str_squish() %>% as.numeric()
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
    tpeBank,
    Team,
    Affiliate,
    Birthplace,
    Nationality,
    Height,
    Weight,
    hair_color = `Hair Color`,
    hair_length = `Hair Length`,
    skintone,
    render,
    `left foot`,
    `right foot`,
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
    traits = `All Traits`,
    rerollused,
    redistused
  ) %>% 
  mutate(
    across(
      contains("pos_"),
      ~ as.integer(.x) %>% 
        replace_na(0)
    ),
    across(
      Acceleration:Throwing,
      ~ as.integer(.x) %>% 
        replace_na(5)
    )
  ) 

playerdata <- 
  playerdata %>% 
  left_join(
    playerdata %>% select(pid, TPE, Acceleration:Throwing) %>% 
      pivot_longer(!c(pid, TPE)) %>% 
      left_join(tpeTable, by = "value") %>% 
      group_by(pid) %>% 
      summarize(
        tpeUsed = sum(cumCost, na.rm = TRUE) - 2*156
      ) %>% 
      ungroup(),
    by = "pid"
  )
  

playerdata %>% 
  mutate(
    tpeBank = TPE - tpeUsed
  ) %>% 
  relocate(
    tpeUsed,
    .after = TPE
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

dbExecute(portalcon,
          "ALTER TABLE `portaldb`.`playerdata` 
CHANGE COLUMN `pid` `pid` INT NOT NULL AUTO_INCREMENT ,
ADD PRIMARY KEY (`pid`);
;")

## Adds current TPE to the tpe log
portalQuery(paste("INSERT INTO tpehistory (time, uid, pid, source, tpe) SELECT ", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), ", 1, pid, 'Initial TPE', tpe FROM playerdata WHERE status_p = 1;"))

data <- portalQuery(
  paste(
    "SELECT pid, `pos_st`, `pos_lam`, `pos_cam`, `pos_ram`, `pos_lm`, `pos_cm`, `pos_rm`, `pos_lwb`, 
    `pos_cdm`, `pos_rwb`, `pos_ld`, `pos_cd`, `pos_rd`, `pos_gk`, `acceleration`, `agility`, 
    `balance`, `jumping reach`, `natural fitness`, `pace`, `stamina`, `strength`, `corners`, 
    `crossing`, `dribbling`, `finishing`, `first touch`, `free kick`, `heading`, `long shots`, 
    `long throws`, `marking`, `passing`, `penalty taking`, `tackling`, `technique`, `aggression`, 
    `anticipation`, `bravery`, `composure`, `concentration`, `decisions`, `determination`, `flair`, 
    `leadership`, `off the ball`, `positioning`, `teamwork`, `vision`, `work rate`, `aerial reach`, 
    `command of area`, `communication`, `eccentricity`, `handling`, `kicking`, `one on ones`, `reflexes`, 
    `tendency to rush`, `tendency to punch`, `throwing`, `traits`, `left foot`, `right foot`
     FROM playerdata;")
) %>% 
  pivot_longer(!pid, values_transform = as.character) %>% 
  mutate(
    value = case_when(
      value == "" ~ "NO TRAITS",
      TRUE ~ value
    )
  )

portalQuery(
  paste(
    "INSERT INTO updatehistory (time, uid, pid, attribute, old, new) VALUES",
    paste(
      "(",
      paste(
        paste0("'", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), "'"),
        1,
        data$pid,
        paste0("'", data$name %>% str_to_upper(), "'"),
        if_else(data$name %>% str_detect("pos_"), '0', if_else(data$name == "traits", "'NO TRAITS'", '5')),
        paste0("'", data$value %>% str_replace_all(pattern = "'", replacement = "''"), "'"),
        sep = ","
      ),
      ")",
      collapse = ","
    ),
    ";"
  )
)




## Regression run
tbl(portalcon, "playerdata") %>%
  collect() %>% 
  mutate(
    age = 
      (currentSeason$season + 1 - 
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
          set q.tpe = a.tpe, q.tpebank = q.tpebank - q.tpe + a.tpe
          where q.pid = a.pid;")

dbExecute(portalcon,
          "ALTER TABLE `portaldb`.`regression` 
CHANGE COLUMN `pid` `pid` DOUBLE NOT NULL ,
ADD PRIMARY KEY (`pid`);
;")

#################################################################
##                BANK                                         ##
#################################################################

bankSheet <- 
  read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1hY1ArnfTICZx0lZF3PTGA922u49avs6X5hKNyldbAn0/edit?gid=1797607253#gid=1797607253",
    sheet = "Shorrax Import Player Pool"
  ) %>% 
  left_join(
    playerdata %>% 
      select(
        pid,
        Name
      ),
    by = "Name"
  ) %>% 
  filter(
    !(pid %>% is.na())
  )

portalQuery(
  paste(
    "INSERT INTO banktransactions (time, pid, source, transaction, status, uid) VALUES",
    paste(
      "(",
      paste(
        paste0("'", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), "'"),
        bankSheet$pid,
        paste0("'", "Initial Import", "'"),
        bankSheet$Balance,
        1,
        1,
        sep = ","
      ),
      ")",
      collapse = ","
    ),
    ";"
  )
)

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
        "The likelihood to punch instead of catching the ball.",
        "The ability to react to shots.",
        "The ability to assess when to rush out.",
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
      "opponent",
      "won",
      "lost",
      "drawn",
      "clean sheets",
      "conceded",
      "saves parried",
      "saves held",
      "saves tipped",
      "save%",
      "penalties faced",
      "penalties saved",
      "xsave%",
      "xg prevented"
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
      "Opponent club",
      "Number of wins",
      "Number of losses",
      "Number of draws",
      "Games with no conceded goals",
      "Goals conceded",
      "Saves where the ball has come back into play",
      "Saves where the ball is held by the keeper",
      "Saves where the ball is gone out of play",
      "Save %",
      "Penalties faced",
      "Penalties saved",
      "Expected save % based on opponent's strength of shot",
      "Expected number of goals saved based on opponent's strength of shot"
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

dbExecute(portalcon,
          "ALTER TABLE `portaldb`.`updatehistory` 
CHANGE COLUMN `time` `time` DOUBLE NOT NULL ,
CHANGE COLUMN `uid` `uid` DOUBLE NULL ,
CHANGE COLUMN `pid` `pid` DOUBLE NOT NULL ,
CHANGE COLUMN `attribute` `attribute` VARCHAR(32) NOT NULL ,
ADD PRIMARY KEY (`time`, `pid`, `attribute`);
;")



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

dbExecute(portalcon,
          "ALTER TABLE `portaldb`.`tpehistory` 
CHANGE COLUMN `time` `time` DOUBLE NOT NULL ,
CHANGE COLUMN `uid` `uid` DOUBLE NOT NULL ,
CHANGE COLUMN `pid` `pid` DOUBLE NOT NULL ,
CHANGE COLUMN `source` `source` VARCHAR(32) NOT NULL ,
ADD PRIMARY KEY (`time`, `pid`, `source`);
;
")


#################################################################
##              Create Index Database                          ##
#################################################################
data.frame(
  season = 13,
  startDate = "2024-01-08",
  ended = 1
) %>% 
  add_row(
    season = 14,
    startDate = "2024-03-11",
    ended = 1
  ) %>% 
  add_row(
    season = 15, 
    startDate = "2024-05-13",
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
  filter(
    (Season == "15" & Division == "2" & Matchday == "13") | (Season == "15" &
    Matchday %in% c("14", "Final"))
  ) %>% 
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
  mutate(
    name = name %>% str_replace_all(pattern = "'", replacement = "\\\\'"),
    across(
      where(is.character),
      ~ paste0("'", .x, "'")
    )
  ) %>% 
  mutate(
    across(
      everything(),
      ~ replace_na(.x, 0)
    )
  )

for(i in 75:nrow(gameData)){
  indexQuery(
    paste(
      "INSERT INTO gamedataoutfield (
  name, club, position, result, opponent, matchday, season, division, acc, aer, agg, agi, ant, bal, bra, cmd, com, cmp, cnt, cor, cro, `dec`, det, dri, ecc, fin, fir, fla, fre, han, hea, jum, kic, ldr, lon, `l th`, mar, nat, otb, `1v1`, pac, pas, pen, pos, pun, ref, tro, sta, str, tck, tea, tec, thr, vis, wor, apps, `minutes played`, `distance run (km)`, `average rating`, `player of the match`, goals, assists, xg, `shots on target`, shots, `penalties taken`, `penalties scored`, `successful passes`, `attempted passes`, `pass%`, `key passes`, `successful crosses`, `attempted crosses`, `cross%`, `chances created`, `successful headers`, `attempted headers`, `header%`, `key headers`, dribbles, `tackles won`, `attempted tackles`, `tackle%`, `key tackles`, interceptions, clearances, `mistakes leading to goals`, `yellow cards`, `red cards`, fouls, `fouls against`, offsides, xa, `xg overperformance`, `goals outside b`, `fk shots`, blocks, `open play key passes`, `successful open play crosses`, `attempted open play crosses`, `shots blocked`, `progressive passes`, `successful presses`, `attempted presses`, `goals outside box`, gid
) VALUES ",
      paste(
        "(", paste(gameData[i,] %>% t(), collapse = ", "), ",", 0, ")", collapse = ","
      ),
      ";"
    )  
  )
}




gameData <-
  tbl(con, "gameDataKeeper") %>%
  filter(
    (Season == "15" & Division == "2" & Matchday == "13") | (Season == "15" &
                                                               Matchday %in% c("14", "Final"))
  ) %>% 
  collect() %>% 
  select(
    Name,
    Club,
    Position,
    Result:Division,
    Apps:`xSave%`,
    `xG Prevented`
  ) %>% 
  mutate(
    across(
      c(
        `Player of the Match`:`Saves Tipped`, 
        `Penalties Faced`:`Penalties Saved`
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
  mutate(
    name = name %>% str_replace_all(pattern = "'", replacement = "\\\\'"),
    across(
      where(is.character),
      ~ paste0("'", .x, "'")
    )
  ) %>% 
  mutate(
    across(
      everything(),
      ~ replace_na(.x, 0)
    )
  )

for(i in 1:nrow(gameData)){
  indexQuery(
    paste(
      "INSERT INTO gamedatakeeper (
  name, club, position, result, opponent, matchday, season, division, apps, `minutes played`, `average rating`, `player of the match`, won, lost, drawn, `clean sheets`, conceded, `saves parried`, `saves held`, `saves tipped`, `save%`, `penalties faced`, `penalties saved`, `xsave%`, `xg prevented`, gid
) VALUES ",
      paste(
        "(", paste(gameData[i,] %>% t(), collapse = ", "), ",", 0, ")", collapse = ","
      ),
      ";"
    )  
  )
}


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

dbExecute(portalcon,
          "ALTER TABLE `portaldb`.`tpetable` 
CHANGE COLUMN `value` `value` BIGINT NOT NULL ,
ADD PRIMARY KEY (`value`);
;
")

#################################################################
##                    Updating a table                    ##
#################################################################

dbExecute(con, 
          paste('DELETE FROM gameDataPlayer WHERE Season = "14" AND Matchday = "Quarter Final Leg 1"', 
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
