
###########################################################################
###########################################################################
###                                                                     ###
###                   GAME-BY-GAME PARSER FOR THE SSL                   ###
###                                                                     ###
###########################################################################
###########################################################################

require(rvest)
require(stringr)
require(sslrtools)
require(stringi)
require(plyr)
require(dplyr)
require(tidyr)

# remotes::install_github("TimTeaFan/dplyover")
require(dplyover)
require(DBI)
require(dbplyr)
require(RSQLite)


# source("D:/GitHubs/ssl-index/SSL-Index/app-documents/dataLoader.R")

con <- 
  dbConnect(
    SQLite(), 
    "database/SSL_Database.db"
  )


goalieFunction <- function(season){
  
  getQuery <- 
    paste(
      "SELECT Name,
        Club,
        sum(Apps) as Apps,
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
        sum(`Save%`) as `Save%` ,
        sum(`Penalties Faced`) as `Penalties Faced` ,
        sum(`Penalties Saved`) as `Penalties Saved` ,
        avg(`xSave%`) as `xSave%` 
      FROM gameDataKeeper
      WHERE Season = ", season, 
      " GROUP BY Name, Club",
      sep = ""
    )
  
  aggregateGoalie <- 
    dbGetQuery(con, getQuery)
    # 
    # googlesheets4::read_sheet(
    #   ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
    #   sheet = "KeeperGameData"
    # ) %>% 
    # filter(
    #   Season == season
    # ) %>% 
    # group_by(
    #   Name,
    #   Club
    # ) %>% 
    # dplyr::summarize(
    #   across(
    #     where(is.numeric),
    #     ~ sum(.x, na.rm = TRUE)
    #   )
    # ) %>% 
    # mutate(
    #   `Average Rating` = `Average Rating`/(Apps-1),
    #   `xSave%` = `xSave%`/(Apps -1)
    # )
  
  FMGoalie <- 
    {
      read_html("D:/Football Manager 2022/screenshots/playerTemp.html", encoding = "UTF-8") %>% 
        html_table() %>% 
        .[[1]] %>% 
        dplyr::select(
          Name:Mins,
          `Av Rat`:PoM,
          Won:`xSv %`
        ) %>% 
        dplyr::filter(
          Position == "GK"
        ) %>% 
        dplyr::rename(
          Apps = Apps,
          `Minutes Played` = Mins,
          Drawn = D,
          Conceded = Conc,
          `Saves Parried` = Svp,
          `Saves Held`= Svh,
          `Saves Tipped` = Svt,
          `Penalties Saved` = `Pens Saved`,
          `Penalties Faced` = `Pens Faced`,
          `Average Rating` = `Av Rat`,
          `Player of the Match` = `PoM`,
          # `Clean Sheets` = `Clean sheets`
          `Clean Sheets` = Shutouts,
          `xSave%`= `xSv %`
        ) %>% 
        dplyr::mutate(
          across(
            c(
              `Minutes Played`:`xSave%`
            ),
            .fns = str_replace_all,
            pattern = "[^\\d\\.]+",
            replacement = ""
          ),
          Club = 
            case_when(
              Club == "Accra FC" ~ "Adowa Accra FC",
              Club == "São Paulo" ~ "União São Paulo",
              Club == "Red Star" ~ "Red Star Laos",
              TRUE ~ Club
            )
        ) %>% 
        dplyr::mutate(
          Nationality = 
            Name %>% 
            str_split(
              pattern = " - ", 
              simplify = TRUE
            ) %>% 
            .[,2],
          Name = 
            Name %>% 
            str_split(
              pattern = " - ", 
              simplify = TRUE
            ) %>% 
            .[,1],
          Club =
            Club %>%
            str_split(pattern = "-", simplify = TRUE) %>% 
            .[,1] %>% 
            str_squish()
        ) %>% 
        dplyr::mutate(
          across(
            !contains(
              c("Name", "Information", "Nationality", "Position", "Club")
            ),
            as.numeric
          )
        ) %>% 
        dplyr::mutate(
          `Save%` = ((`Saves Parried`+`Saves Held`+`Saves Tipped`)/(`Saves Parried`+`Saves Held`+`Saves Tipped`+Conceded)) %>% round(4) * 100
        ) %>% 
        relocate(
          `Save%`,
          .after = `Saves Tipped`
        ) %>% 
        relocate(
          c(
            Nationality,
            Club,
            Position,
            Apps,
            `Minutes Played`,
          ),
          .after = Name
        ) %>% 
        arrange(
          `Average Rating` %>% desc()
        ) %>% 
        # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
        suppressWarnings() %>% 
        mutate(
          Club = 
            case_when(
              Club == "Football Club de Rio" ~ "FC Rio",
              TRUE ~ Club
            )
        ) %>% 
        arrange(Name)
    }
  
  currentGoalie <- 
    FMGoalie %>% 
    full_join(
      aggregateGoalie,
      by = c("Name", "Club")
    ) %>% 
    group_by(
      Name,
      Club
    ) %>% 
    mutate(
      across2(
        .xcols = ends_with(".x"),
        .ycols = ends_with(".y"),
        .fns = 
          list(
            diff = ~ sum(.x, -.y, na.rm = TRUE)
          ),
        .names = "{xcol}"
      )
    ) %>% 
    select(
      !contains(".y")
    ) %>% 
    rename_with(
      ~ str_replace(.x, ".x", "")
    ) %>% 
    filter(
      `Minutes Played`>0
    ) %>% 
    left_join(
      aggregateGoalie %>% 
        select(
          Name,
          Club,
          `Average Rating`,
          `xSave%`,
          Apps
        ),
      by = c("Name","Club"),
      suffix = c("Day", "Season")
    ) %>% 
    group_by(Name) %>% 
    mutate(
      `Average RatingDay` = 
        case_when(
          is.na(`Average RatingSeason`) ~ `Average RatingDay`,
          TRUE ~ (
            (`Average RatingDay` + `Average RatingSeason`) * 
              (`AppsSeason` + 1) -
              `Average RatingSeason`*`AppsSeason`
          )
        ) %>% round(2),
      `Save%` = ((`Saves Parried`+`Saves Held`+`Saves Tipped`)/(`Saves Parried`+`Saves Held`+`Saves Tipped`+Conceded)) %>% round(4) * 100,
      `xSave%Day` = 
        case_when(
          is.na(`xSave%Season`) ~ `xSave%Day`,
          TRUE ~ (
            ((`xSave%Day` + `xSave%Season`) -
              `xSave%Season`/2)*2
          )
        ) %>% round(2) 
    ) %>% 
    select(
      !contains("Season"),
      `Average Rating` = `Average RatingDay`,
      `xSave%` = `xSave%Day`,
      `Apps` = `AppsDay`
    ) %>% 
    return()
  
}

outfieldFunction <- function(season){
  getQuery <- 
    paste(
      "SELECT Name,
        Club,
        sum(Apps) as Apps,
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
        sum(`Pass%`) as `Pass%` ,
        sum(`Key Passes`) as `Key Passes` ,
        sum(`Successful Crosses`) as `Successful Crosses` ,
        sum(`Attempted Crosses`) as `Attempted Crosses` ,
        sum(`Cross%`) as `Cross%` ,
        sum(`Chances Created`) as `Chances Created` ,
        sum(`Successful Headers`) as `Successful Headers` ,
        sum(`Attempted Headers`) as `Attempted Headers` ,
        sum(`Header%`) as `Header%` ,
        sum(`Key Headers`) as `Key Headers` ,
        sum(`Dribbles`) as Dribbles ,
        sum(`Tackles Won`) as `Tackles Won` ,
        sum(`Attempted Tackles`) as `Attempted Tackles` ,
        sum(`Tackle%`) as `Tackle%` ,
        sum(`Key Tackles`) as `Key Tackles` ,
        sum(`Interceptions`) as Interceptions ,
        sum(`Clearances`) as Clearances ,
        sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
        sum(`Yellow Cards`) as `Yellow Cards` ,
        sum(`Red Cards`) as `Red Cards` ,
        sum(`Fouls`) as Fouls ,
        sum(`Fouls Against`) as `Fouls Against` ,
        sum(`Offsides`) as Offsides
      FROM gameDataPlayer
      WHERE Season = ", season, 
      " GROUP BY Name, Club",
      sep = ""
    )
  
  aggregateOutfield <-
    dbGetQuery(con, getQuery) 
  
  # aggregateOutfield %>%
  #   filter(Name == "Mike Rup") %>%
  #   write.csv2(file = "Mike Rup Reset.csv", row.names = FALSE)

  ### Tam Kove had a reset in the loan deal in S10 making their stats reset after MD2
  
  # loanReset <- function(name){
  #   data <- read.csv2(paste(name,"Reset.csv"))
  #   
  #     (aggregateOutfield[aggregateOutfield$Name == name,
  #                       sapply(X = aggregateOutfield,
  #                              FUN =  is.numeric)] -
  #     { data %>% select(Apps:Offsides) }) %>% 
  #       return()
  # }
  # 
  # aggregateOutfield[aggregateOutfield$Name == "Budget Busquets",
  #                   sapply(X = aggregateOutfield,
  #                          FUN =  is.numeric)] <- 
  #   loanReset("Budget Busquets")
  # 
  # aggregateOutfield[aggregateOutfield$Name == "Rafael Ramos",
  #                   sapply(X = aggregateOutfield,
  #                          FUN =  is.numeric)] <- 
  #   loanReset("Rafael Ramos")
  # aggregateOutfield[aggregateOutfield$Name == "Caleb Hayden",
  #                   sapply(X = aggregateOutfield,
  #                          FUN =  is.numeric)] <- 
  #   loanReset("Caleb Hayden")
  # aggregateOutfield[aggregateOutfield$Name == "Mike Rup",
  #                   sapply(X = aggregateOutfield,
  #                          FUN =  is.numeric)] <- 
  #   loanReset("Mike Rup")


  #   
    # 
    # googlesheets4::read_sheet(
    #   ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
    #   sheet = "PlayerGameData"
    # ) %>% 
    # filter(
    #   Season == season
    # ) %>% 
    # group_by(
    #   Name,
    #   Club
    # ) %>% 
    # dplyr::summarize(
    #   across(
    #     where(is.numeric),
    #     ~ sum(.x, na.rm = TRUE)
    #   )
    # ) %>% 
    # mutate(
    #   `Average Rating` = `Average Rating`/(Apps-1)
    # )
  
  FMOutfield <- 
    {
      read_html("D:/Football Manager 2022/screenshots/playerTemp.html", encoding = "UTF-8") %>% 
        html_table() %>% 
        .[[1]] %>% 
        dplyr::select(
          `Inf`:Wor
        ) %>% 
        dplyr::rename(
          Apps = Apps,
          Goals = Gls,
          Assists = Ast,
          `Minutes Played` = Mins,
          `Attempted Passes` = `Pas A`,
          `Successful Passes` = `Ps C`,
          `Key Passes` = `K Pas`,
          `Successful Crosses` = `Cr C`,
          `Attempted Crosses` = `Cr A`,
          `Chances Created` = CCC,
          `Tackles Won` = `Tck W`,
          `Tackle%` = `Tck R`,
          `Key Tackles` = `K Tck`,
          `Successful Headers` = Hdrs,
          `Attempted Headers` = `Hdrs A`,
          `Header%` = `Hdr %`,
          `Key Headers` = `K Hdrs`,
          `Shots on Target` = ShT,
          `Mistakes Leading to Goals` = `Gl Mst`,
          Dribbles = `Drb`,
          Offsides = Off,
          `Fouls Against` = FA,
          Interceptions = Itc,
          `Yellow Cards` = Yel,
          `Red Cards` = Red,
          Fouls = Fls,
          `Penalties Taken` = Pens,
          `Penalties Scored` = `Pens S`,
          `Distance Run (km)` = Distance,
          `Average Rating` = `Av Rat`,
          `Player of the Match` = `PoM`,
          Clearances = Clear
        ) %>% 
        dplyr::mutate(
          Starts = 
            case_when(
              str_detect(Apps, pattern = "\\(") ~
                (str_split(Apps, pattern = "\\(", simplify = TRUE)[,1] %>% 
                   str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
                   as.numeric()
                 ),
              TRUE ~ Apps %>% as.numeric()
            ),
          Subs = 
            case_when(
              str_detect(Apps, pattern = "\\(") ~
                (str_split(Apps, pattern = "\\(", simplify = TRUE)[,ncol(str_split(Apps, pattern = "\\(", simplify = TRUE))] %>% 
                   str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
                   as.numeric()
                ),
              TRUE ~ 0
            ),
          Club = 
            case_when(
              Club == "Accra FC" ~ "Adowa Accra FC",
              Club == "São Paulo" ~ "União São Paulo",
              Club == "Red Star" ~ "Red Star Laos",
              TRUE ~ Club
            )
        ) %>% 
        dplyr::mutate(
          Apps = rowSums(data.frame(.$Starts, .$Subs), na.rm = TRUE)
        ) %>%
        dplyr::select(
          -Starts, -Subs
        ) %>% 
        mutate(
          across(
            c(
              `Minutes Played`:`Offsides`
            ),
            .fns = str_replace_all,
            pattern = "[^\\d\\.]+",
            replacement = ""
          ),
          Name =
            case_when(
              str_detect(Name, "Formula-The") ~ "A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ® - American",
              TRUE ~ Name
            )
        ) %>% 
        # mutate(
        #   Name =
        #     case_when(
        #       str_detect(Name, "GFuel") ~ "FazeBerry GFuel - American",
        #       TRUE ~ Name
        #     )
        # ) %>%
        mutate(
          `Pass%` = (`Successful Passes` %>% as.numeric()/`Attempted Passes` %>% as.numeric()) %>% round(4)*100,
          `Header%` = (`Successful Headers` %>% as.numeric()/`Attempted Headers` %>% as.numeric()) %>% round(4)*100,
          # Position = NA,
          # Nationality =
          #   case_when(
          #     str_detect(Name, "GFuel") ~
          #       Name %>%
          #       str_split(
          #         pattern = " - ",
          #         simplify = TRUE
          #       ) %>%
          #       .[,3],
          #     TRUE ~ Name %>%
          #       str_split(
          #         pattern = " - ",
          #         simplify = TRUE
          #       ) %>%
          #       .[,2]
          #   ),
          Nationality =
            Name %>%
            str_split(
              pattern = " - ",
              simplify = TRUE
            ) %>%
            .[,2],
          Name =
            Name %>%
              str_split(
                pattern = " - ",
                simplify = TRUE
              ) %>%
              .[,1],
          `Cross%` = (`Successful Crosses` %>% as.numeric()/`Attempted Crosses` %>% as.numeric()) %>% round(4)*100,
          # `Header%` =
          #   `Header%` %>% 
          #   as.numeric(),
          `Tackle%` =
            `Tackle%` %>% 
            as.numeric(),
          `Distance Run (km)` = 
            `Distance Run (km)` %>% 
            as.numeric(),
          Club =
            Club %>%
            str_split(pattern = "-", simplify = TRUE) %>% 
            .[,1] %>% 
            str_squish()
        ) %>% 
        mutate(
          across(
            !contains(
              c("Name", "Information", "Nationality", "Position", "Club")
            ),
            as.numeric
          ),
          `Attempted Tackles` = ((`Tackles Won` %>% as.numeric())/(`Tackle%`/100)) %>% round(0),
          Name = 
            case_when(
              str_detect(Name, "GFuel") ~ "A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ®",
              Name == "SSL BOT" & Position == "DM" ~ "SSL BOT RIO",
              TRUE ~ Name
            )
        ) %>% 
        relocate(
          c(
            Nationality,
            Club,
            Position,
            Apps,
            `Minutes Played`,
            `Distance Run (km)`
          ),
          .after = Name
        ) %>% 
        relocate(
          c(
            `Attempted Passes`,
            `Pass%`
          ),
          .after = `Successful Passes`
        ) %>% 
        relocate(
          c(
            `Attempted Crosses`,
            `Cross%`
          ),
          .after = `Successful Crosses`
        ) %>% 
        relocate(
          c(
            `Attempted Headers`
          ),
          .after = `Successful Headers`
        ) %>% 
        relocate(
          c(
            `Attempted Tackles`,
            `Tackle%`
          ),
          .after = `Tackles Won`
        ) %>% 
        relocate(
          `Shots on Target`,
          .before = `Shots`
        ) %>% 
        select(
          -`Inf`,
          # -`Tck A`,
          -Rec
        ) %>% 
        arrange(
          `Average Rating` %>% desc()
        ) %>% 
        # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
        suppressWarnings() %>% 
        mutate(
          Club = 
            case_when(
              Club == "Football Club de Rio" ~ "FC Rio",
              TRUE ~ Club
            )
        ) %>% 
        arrange(Name)
    }
  
  currentOutfield <- 
    FMOutfield %>% 
    full_join(
      aggregateOutfield,
      by = c("Name", "Club")
    ) %>% 
    group_by(
      Name,
      Club
    ) %>% 
    mutate(
      across2(
        .xcols = ends_with(".x"),
        .ycols = ends_with(".y"),
        .fns = 
          list(
            diff = ~ sum(.x, -.y, na.rm = TRUE)
          ),
        .names = "{xcol}"
      )
    ) %>% 
    select(
      !contains(".y")
    ) %>% 
    rename_with(
      ~ str_replace(.x, ".x", "")
    ) %>% 
    filter(
      `Minutes Played`>0
    ) %>% 
    left_join(
      aggregateOutfield %>% 
        select(
          Name,
          Club,
          `Average Rating`,
          Apps
        ),
      by = c("Name","Club"),
      suffix = c("Day", "Season")
    ) %>% 
    group_by(Name) %>% 
    mutate(
      `Average RatingDay` = 
        case_when(
          is.na(`Average RatingSeason`) ~ `Average RatingDay`,
          TRUE ~ (
            (`Average RatingDay` + `Average RatingSeason`) * 
              (`AppsSeason` + 1) -
              `Average RatingSeason`*`AppsSeason`
          )
        ) %>% round(2),
      `Pass%` = (`Successful Passes` %>% as.numeric()/`Attempted Passes` %>% as.numeric()) %>% round(4)*100,
      `Header%` = (`Successful Headers` %>% as.numeric()/`Attempted Headers` %>% as.numeric()) %>% round(4)*100,
      `Cross%` = (`Successful Crosses` %>% as.numeric()/`Attempted Crosses` %>% as.numeric()) %>% round(4)*100,
      `Tackle%` = (`Tackles Won` %>% as.numeric()/`Attempted Tackles` %>% as.numeric()) %>% round(4)*100,
      Clearances = if_else(is.na(Clearances), 0, Clearances)
    ) %>% 
    select(
      !contains("Season"),
      `Average Rating` = `Average RatingDay`,
      `Apps` = `AppsDay`
    ) %>% 
    mutate(
      Apps = 
        case_when(
          `Minutes Played` > 45 ~ 1,
          TRUE ~ 0.5
        )
    ) %>% 
    return()
}

goalieOutput <- function(season, matchday){
  schedule <- 
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit#gid=0",
      sheet = paste("Season", season)
    ) %>% 
    mutate(
      Matchday = unlist(Matchday),
      Division = unlist(Division),
      `In-game Date` = `In-game Date` %>% as.Date()
    ) %>% 
    filter(
      `In-game Date` == matchday
    ) 
  
  matchGoalie <- 
    {
      goalieFunction(season = season) %>% 
        left_join(
          schedule,
          by = c("Club" = "Home")
        ) %>% 
        left_join(
          schedule,
          by = c("Club" = "Away")
        ) %>% 
        mutate(
          Result = 
            case_when(
              is.na(Result.x) & is.na(Away) ~ stringi::stri_reverse(Result.y),
              is.na(Result.x) ~ Result.y,
              TRUE ~ Result.x
            ),
          Opponent = 
            case_when(
              is.na(Away) ~ Home,
              TRUE ~ Away
            ),
          Matchday = 
            case_when(
              is.na(Matchday.x) ~ Matchday.y,
              TRUE ~ Matchday.x
            ),
          Division = 
            case_when(
              is.na(Division.x) ~ Division.y,
              TRUE ~ Division.x
            ),
          Season = season,
          `Average Rating` = 
            case_when(
              `Average Rating` > 10 ~ 10,
              TRUE ~ `Average Rating`
            )
        ) %>% 
        select(
          !contains(".x") & !contains(".y"),
          -Away,
          -Home
        ) %>% 
        mutate(
          Division = case_when(Division == "Cup" ~ "0",
                               TRUE ~ Division)
        )
    }
  
  return(matchGoalie)
}

outfieldOutput <- function(season, matchday){
  schedule <- 
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit#gid=0",
      sheet = paste("Season", season)
    ) %>% 
    mutate(
      Matchday = unlist(Matchday),
      Division = unlist(Division),
      `In-game Date` = `In-game Date` %>% as.Date()
    ) %>% 
    filter(
      `In-game Date` == matchday
    ) 
  
  matchOutfield <- 
    {
      outfieldFunction(season = season) %>% 
        left_join(
          schedule,
          by = c("Club" = "Home")
        ) %>% 
        left_join(
          schedule,
          by = c("Club" = "Away")
        ) %>% 
        mutate(
          Result = 
            case_when(
              is.na(Result.x) & is.na(Away) ~ stringi::stri_reverse(Result.y),
              is.na(Result.x) ~ Result.y,
              TRUE ~ Result.x
            ),
          Opponent = 
            case_when(
              is.na(Away) ~ Home,
              TRUE ~ Away
            ),
          Matchday = 
            case_when(
              is.na(Matchday.x) ~ Matchday.y,
              TRUE ~ Matchday.x
            ),
          Division = 
            case_when(
              is.na(Division.x) ~ Division.y,
              TRUE ~ Division.x
            ),
          Season = season,
          `Average Rating` = 
            case_when(
              `Average Rating` > 10 ~ 10,
              TRUE ~ `Average Rating`
            ),
          xG =
            if_else(xG < 0, 0, xG)
        ) %>% 
        select(
          !contains(".x") & !contains(".y"),
          -Away,
          -Home
        ) %>% 
        relocate(
          Acc:Wor,
          .after = Division
        ) %>% 
        mutate(
          Division = case_when(Division == "Cup" ~ "0",
                               TRUE ~ Division)
        )
    }
  
  return(matchOutfield)
}

### Start here

season <- 12

date <- "2023-10-03" %>% as.Date()

{
  ## Adding a deauthorization for reading of Google Sheets that are still being used. 
  googlesheets4::gs4_deauth()
  
  matchGoalie <- goalieOutput(season, date) %>% 
    unique()
  
  matchOutfield <- outfieldOutput(season, date) %>% 
    unique()
  
  table(matchOutfield$Result) %>% print()
  
  table(matchOutfield$Opponent) %>% print()
  
  ## Checks sum of minutes played per player per team
  sum(matchOutfield$`Minutes Played`)/length(unique(matchOutfield$Club))/11
}
## Writing to the database
dbAppendTable(con, "gameDataPlayer", matchOutfield)
dbAppendTable(con, "gameDataKeeper", matchGoalie)

# dbAppendTable(con, "Player_Game_Data", matchOutfield)
# dbAppendTable(con, "Keeper_Game_Data", matchGoalie)

dbDisconnect(con)












