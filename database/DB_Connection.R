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

con <- dbConnect(SQLite(), "database/SSL_Database.db")
 
dbConnect(SQLite(), "database/SSL_Database.db")

dbListTables(con)


#################################################################
##              Set up Queries for other files                    ##
#################################################################

PlayerDataRegularSeason <- dbGetQuery(con, 
"SELECT 
Name,
Club,
Season,
sum(`Minutes Played`) as `Minutes Played` ,
sum(`Distance Run (km)`) as `Distance Run (km)` ,
avg(`Average Rating`) as `Average Rating` ,
sum(`Player of the Match`) as `Player of the Match` ,
sum(`Goals`) as Goals ,
sum(`Assists`) as Assists ,
sum(`xG`) as xG ,
sum(`Shots on Target`) as `Shots on Target` ,
sum(`Shots`) as Shots ,
sum(`Penalties Taken`) as `Penalties Taken` ,
sum(`Penalties Scored`) as `Penalties Scored` ,
sum(`Successful Passes`) as `Successful Passes` ,
sum(`Attempted Passes`) as `Attempted Passes` ,
avg(`Pass%`) as `Pass%` ,
sum(`Key Passes`) as `Key Passes` ,
sum(`Successful Crosses`) as `Successful Crosses` ,
sum(`Attempted Crosses`) as `Attempted Crosses` ,
avg(`Cross%`) as `Cross%` ,
sum(`Chances Created`) as `Chances Created` ,
sum(`Successful Headers`) as `Successful Headers` ,
sum(`Attempted Headers`) as `Attempted Headers` ,
avg(`Header%`) as `Header%` ,
sum(`Key Headers`) as `Key Headers` ,
sum(`Dribbles`) as Dribbles ,
sum(`Tackles Won`) as `Tackles Won` ,
sum(`Attempted Tackles`) as `Attempted Tackles` ,
avg(`Tackle%`) as `Tackle%` ,
sum(`Key Tackles`) as `Key Tackles` ,
sum(`Interceptions`) as Interceptions ,
sum(`Clearances`) as Clearances ,
sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
sum(`Yellow Cards`) as `Yellow Cards` ,
sum(`Red Cards`) as `Red Cards` ,
sum(`Fouls`) as Fouls ,
sum(`Fouls Against`) as `Fouls Against` ,
sum(`Offsides`) as Offsides,
count(Matchday) as GamesPlayed
  FROM Player_Game_Data  group by Name, Season"
)

PlayerDataPlayoff <- dbGetQuery(con,'SELECT 
Name,
Club,
Season,
sum(`Minutes Played`) as `Minutes Played` ,
sum(`Distance Run (km)`) as `Distance Run (km)` ,
avg(`Average Rating`) as `Average Rating` ,
sum(`Player of the Match`) as `Player of the Match` ,
sum(`Goals`) as Goals ,
sum(`Assists`) as Assists ,
sum(`xG`) as xG ,
sum(`Shots on Target`) as `Shots on Target` ,
sum(`Shots`) as Shots ,
sum(`Penalties Taken`) as `Penalties Taken` ,
sum(`Penalties Scored`) as `Penalties Scored` ,
sum(`Successful Passes`) as `Successful Passes` ,
sum(`Attempted Passes`) as `Attempted Passes` ,
avg(`Pass%`) as `Pass%` ,
sum(`Key Passes`) as `Key Passes` ,
sum(`Successful Crosses`) as `Successful Crosses` ,
sum(`Attempted Crosses`) as `Attempted Crosses` ,
avg(`Cross%`) as `Cross%` ,
sum(`Chances Created`) as `Chances Created` ,
sum(`Successful Headers`) as `Successful Headers` ,
sum(`Attempted Headers`) as `Attempted Headers` ,
avg(`Header%`) as `Header%` ,
sum(`Key Headers`) as `Key Headers` ,
sum(`Dribbles`) as Dribbles ,
sum(`Tackles Won`) as `Tackles Won` ,
sum(`Attempted Tackles`) as `Attempted Tackles` ,
avg(`Tackle%`) as `Tackle%` ,
sum(`Key Tackles`) as `Key Tackles` ,
sum(`Interceptions`) as Interceptions ,
sum(`Clearances`) as Clearances ,
sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
sum(`Yellow Cards`) as `Yellow Cards` ,
sum(`Red Cards`) as `Red Cards` ,
sum(`Fouls`) as Fouls ,
sum(`Fouls Against`) as `Fouls Against` ,
sum(`Offsides`) as Offsides,
count(Matchday) as GamesPlayed
  FROM Player_Game_Data  where MatchDay LIKE  "%Cup%" group by Name, Season')


PlayerSumRegular <- dbGetQuery(con, 
                               "SELECT 
Name,
sum(`Minutes Played`) as `Minutes Played` ,
sum(`Distance Run (km)`) as `Distance Run (km)` ,
avg(`Average Rating`) as `Average Rating` ,
sum(`Player of the Match`) as `Player of the Match` ,
sum(`Goals`) as Goals ,
sum(`Assists`) as Assists ,
sum(`xG`) as xG ,
sum(`Shots on Target`) as `Shots on Target` ,
sum(`Shots`) as Shots ,
sum(`Penalties Taken`) as `Penalties Taken` ,
sum(`Penalties Scored`) as `Penalties Scored` ,
sum(`Successful Passes`) as `Successful Passes` ,
sum(`Attempted Passes`) as `Attempted Passes` ,
avg(`Pass%`) as `Pass%` ,
sum(`Key Passes`) as `Key Passes` ,
sum(`Successful Crosses`) as `Successful Crosses` ,
sum(`Attempted Crosses`) as `Attempted Crosses` ,
avg(`Cross%`) as `Cross%` ,
sum(`Chances Created`) as `Chances Created` ,
sum(`Successful Headers`) as `Successful Headers` ,
sum(`Attempted Headers`) as `Attempted Headers` ,
avg(`Header%`) as `Header%` ,
sum(`Key Headers`) as `Key Headers` ,
sum(`Dribbles`) as Dribbles ,
sum(`Tackles Won`) as `Tackles Won` ,
sum(`Attempted Tackles`) as `Attempted Tackles` ,
avg(`Tackle%`) as `Tackle%` ,
sum(`Key Tackles`) as `Key Tackles` ,
sum(`Interceptions`) as Interceptions ,
sum(`Clearances`) as Clearances ,
sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
sum(`Yellow Cards`) as `Yellow Cards` ,
sum(`Red Cards`) as `Red Cards` ,
sum(`Fouls`) as Fouls ,
sum(`Fouls Against`) as `Fouls Against` ,
sum(`Offsides`) as Offsides,
count(Matchday) as GamesPlayed
  FROM Player_Game_Data  group by Name"
)
                               
     
PlayerSumPlayoff <- dbGetQuery(con, 
                               "SELECT 
Name,
sum(`Minutes Played`) as `Minutes Played` ,
sum(`Distance Run (km)`) as `Distance Run (km)` ,
avg(`Average Rating`) as `Average Rating` ,
sum(`Player of the Match`) as `Player of the Match` ,
sum(`Goals`) as Goals ,
sum(`Assists`) as Assists ,
sum(`xG`) as xG ,
sum(`Shots on Target`) as `Shots on Target` ,
sum(`Shots`) as Shots ,
sum(`Penalties Taken`) as `Penalties Taken` ,
sum(`Penalties Scored`) as `Penalties Scored` ,
sum(`Successful Passes`) as `Successful Passes` ,
sum(`Attempted Passes`) as `Attempted Passes` ,
avg(`Pass%`) as `Pass%` ,
sum(`Key Passes`) as `Key Passes` ,
sum(`Successful Crosses`) as `Successful Crosses` ,
sum(`Attempted Crosses`) as `Attempted Crosses` ,
avg(`Cross%`) as `Cross%` ,
sum(`Chances Created`) as `Chances Created` ,
sum(`Successful Headers`) as `Successful Headers` ,
sum(`Attempted Headers`) as `Attempted Headers` ,
avg(`Header%`) as `Header%` ,
sum(`Key Headers`) as `Key Headers` ,
sum(`Dribbles`) as Dribbles ,
sum(`Tackles Won`) as `Tackles Won` ,
sum(`Attempted Tackles`) as `Attempted Tackles` ,
avg(`Tackle%`) as `Tackle%` ,
sum(`Key Tackles`) as `Key Tackles` ,
sum(`Interceptions`) as Interceptions ,
sum(`Clearances`) as Clearances ,
sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
sum(`Yellow Cards`) as `Yellow Cards` ,
sum(`Red Cards`) as `Red Cards` ,
sum(`Fouls`) as Fouls ,
sum(`Fouls Against`) as `Fouls Against` ,
sum(`Offsides`) as Offsides,
count(Matchday) as GamesPlayed
  FROM Player_Game_Data  where MatchDay LIKE  \"%Cup%\" group by Name"
)                          


KeeperDataRegularSeason <-dbGetQuery(con,
                                     "SELECT 
                                     Name,
                                     Club,
                                     Season,
                                     sum(`Minutes Played`) as `Minutes Played` ,
                                     avg(`Average Rating`) as `Average Rating` ,
                                     sum(`Player of the Match`) as `Player of the Match` ,
                                     sum(`Won`) as Won ,
                                     sum(`Lost`) as Lost ,
                                     sum(`Drawn`) as Drawn ,
                                     sum(`Clean Sheets`) as `Clean Sheets` ,
                                     sum(`Conceded`) as Conceded ,
                                     sum(`Saves Parried`) as `Saves Parried` ,
                                     sum(`Saves Held`) as `Saves Held` ,
                                     sum(`Saves Tipped`) as `Saves Tipped` ,
                                     avg(`Save%`) as `Save%` ,
                                     sum(`Penalties Faced`) as `Penalties Faced` ,
                                     sum(`Penalties Saved`) as `Penalties Saved` ,
                                     avg(`xSave%`) as `xSave%` ,
                                     sum(`Result`) as Result ,
                                     sum(`Opponent`) as Opponent ,
                                     sum(`Matchday`) as Matchday ,
                                     count(`Season`) as GamesPlayed 
                                     FROM Keeper_Game_Data group by Name, Season"                                
                                     
)


KeeperDataPlayoff <-dbGetQuery(con,
                                     "SELECT 
                                     Name,
                                     Club,
                                     Season,
                                     sum(`Minutes Played`) as `Minutes Played` ,
                                     avg(`Average Rating`) as `Average Rating` ,
                                     sum(`Player of the Match`) as `Player of the Match` ,
                                     sum(`Won`) as Won ,
                                     sum(`Lost`) as Lost ,
                                     sum(`Drawn`) as Drawn ,
                                     sum(`Clean Sheets`) as `Clean Sheets` ,
                                     sum(`Conceded`) as Conceded ,
                                     sum(`Saves Parried`) as `Saves Parried` ,
                                     sum(`Saves Held`) as `Saves Held` ,
                                     sum(`Saves Tipped`) as `Saves Tipped` ,
                                     avg(`Save%`) as `Save%` ,
                                     sum(`Penalties Faced`) as `Penalties Faced` ,
                                     sum(`Penalties Saved`) as `Penalties Saved` ,
                                     avg(`xSave%`) as `xSave%` ,
                                     sum(`Result`) as Result ,
                                     sum(`Opponent`) as Opponent ,
                                     sum(`Matchday`) as Matchday ,
                                     count(`Season`) as GamesPlayed 
                                     FROM Keeper_Game_Data  where  MatchDay like \"%Cup%\" group by Name, Season"                                
                                     
)

KeeperSumPlayoff <-dbGetQuery(con,
                               "SELECT 
                                     Name,
                                     sum(`Minutes Played`) as `Minutes Played` ,
                                     avg(`Average Rating`) as `Average Rating` ,
                                     sum(`Player of the Match`) as `Player of the Match` ,
                                     sum(`Won`) as Won ,
                                     sum(`Lost`) as Lost ,
                                     sum(`Drawn`) as Drawn ,
                                     sum(`Clean Sheets`) as `Clean Sheets` ,
                                     sum(`Conceded`) as Conceded ,
                                     sum(`Saves Parried`) as `Saves Parried` ,
                                     sum(`Saves Held`) as `Saves Held` ,
                                     sum(`Saves Tipped`) as `Saves Tipped` ,
                                     avg(`Save%`) as `Save%` ,
                                     sum(`Penalties Faced`) as `Penalties Faced` ,
                                     sum(`Penalties Saved`) as `Penalties Saved` ,
                                     avg(`xSave%`) as `xSave%` ,
                                     sum(`Result`) as Result ,
                                     sum(`Opponent`) as Opponent ,
                                     sum(`Matchday`) as Matchday ,
                                     count(`Season`) as GamesPlayed 
                                     FROM Keeper_Game_Data where  MatchDay like \"%Cup%\" group by Name"                                
                               
)

KeeperSumRegular <-dbGetQuery(con,
                              "SELECT 
                                     Name,
                                     sum(`Minutes Played`) as `Minutes Played` ,
                                     avg(`Average Rating`) as `Average Rating` ,
                                     sum(`Player of the Match`) as `Player of the Match` ,
                                     sum(`Won`) as Won ,
                                     sum(`Lost`) as Lost ,
                                     sum(`Drawn`) as Drawn ,
                                     sum(`Clean Sheets`) as `Clean Sheets` ,
                                     sum(`Conceded`) as Conceded ,
                                     sum(`Saves Parried`) as `Saves Parried` ,
                                     sum(`Saves Held`) as `Saves Held` ,
                                     sum(`Saves Tipped`) as `Saves Tipped` ,
                                     avg(`Save%`) as `Save%` ,
                                     sum(`Penalties Faced`) as `Penalties Faced` ,
                                     sum(`Penalties Saved`) as `Penalties Saved` ,
                                     avg(`xSave%`) as `xSave%` ,
                                     sum(`Result`) as Result ,
                                     sum(`Opponent`) as Opponent ,
                                     sum(`Matchday`) as Matchday ,
                                     count(`Season`) as GamesPlayed 
                                     FROM Keeper_Game_Data group by Name"                                
                              
)


#################################################################
##              Insert Daily Scrape into DB                    ##
#################################################################

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

# teamInfo <- googlesheets4::read_sheet(
#   ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
#   sheet = "Team Information"
# )

#################################################################
##             Insert Attributes and Availability              ##
#################################################################

#################################################################
##              Insert KeeperGameData                          ##
#################################################################

#################################################################
##              Insert PlayerGameData                          ##
#################################################################

#################################################################
##                Insert Positional Coordinates                ##
#################################################################

#################################################################
##                    Insert TPE Cost table                    ##
#################################################################

## Updating a table
# dbExecute(con, "UPDATE Player_Game_Data SET Result = '0-3' WHERE Season = '4' AND Matchday = '11' AND Club = 'Adowa Accra FC'")
# dbExecute(con, "UPDATE Player_Game_Data SET Name = 'Liang Kuai' WHERE Name = 'Kuai Liang'")
# 
# tbl(con, "Player_Game_Data") %>%
#   filter(
#     Name == "Liang Kuai"
#   ) %>% 
#   collect()

# dbWriteTable(con, "Team_Information", teamInfo, overwrite = TRUE)

dbDisconnect(con)
