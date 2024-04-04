
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
  
  aggregateGoalie <-
    tbl(con, "gameDataKeeper") %>%
    filter(Season == season) %>%
    select(
      Name,
      Club,
      Apps:`xSave%`,
      `xG Prevented`
    ) %>%
    group_by(Name, Club) %>%
    summarize(
      across(
        c(`Average Rating`, `xSave%`),
        ~ mean(.x, na.rm = TRUE)
      ),
      across(
        !c(`Average Rating`, `xSave%`),
        ~ sum(.x, na.rm = TRUE)
      )
    ) %>%
    relocate(
      `xSave%`,
      .before = `xG Prevented` 
    ) %>% 
    relocate(
      `Average Rating`,
      .after = `Minutes Played`
    ) %>% 
    collect()
  # 
  # getQuery <- 
  #   paste(
  #     "SELECT Name,
  #       Club,
  #       sum(Apps) as Apps,
  #       sum(`Minutes Played`) as `Minutes Played` ,
  #       avg(`Average Rating`) as `Average Rating` ,
  #       sum(`Player of the Match`) as `Player of the Match` ,
  #       sum(`Won`) as Won ,
  #       sum(`Lost`) as Lost ,
  #       sum(`Drawn`) as Drawn ,
  #       sum(`Clean Sheets`) as `Clean Sheets` ,
  #       sum(`Conceded`) as Conceded ,
  #       sum(`Saves Parried`) as `Saves Parried` ,
  #       sum(`Saves Held`) as `Saves Held` ,
  #       sum(`Saves Tipped`) as `Saves Tipped` ,
  #       sum(`Save%`) as `Save%` ,
  #       sum(`Penalties Faced`) as `Penalties Faced` ,
  #       sum(`Penalties Saved`) as `Penalties Saved` ,
  #       avg(`xSave%`) as `xSave%`,
  #       sum(`xG Prevented`) as `xG Prevented`
  #     FROM gameDataKeeper
  #     WHERE Season = '", season, "'",
  #     " GROUP BY Name, Club",
  #     sep = ""
  #   )
  # 
  # aggregateGoalie <- 
  #   dbGetQuery(con, getQuery)
  
  # aggregateGoalie %>%
  #   filter(Club %in% c("Schwarzwälder FV")) %>%
  #   write.csv2(file = "S13K1 Reset.csv", row.names = FALSE)

  # aggregateGoalie %>%
  #   filter(Club %in% c("F.C. Kaapstad", "CF Catalunya")) %>%
  #   write.csv2(file = "S13K2 Reset.csv", row.names = FALSE)

  # aggregateGoalie %>%
  #   filter(Club %in% c("A.C. Romana")) %>%
  #   write.csv2(file = "S13K3 Reset.csv", row.names = FALSE)

  # aggregateGoalie %>%
  #   filter(Club %in% c("Cairo City")) %>%
  #   write.csv2(file = "S13K4 Reset.csv", row.names = FALSE)

  loanReset <- function(name, teams){
    data <- read.csv2(paste(name,"Reset.csv"))
    
    (aggregateGoalie[aggregateGoalie$Club %in% teams,
                       3:ncol(aggregateGoalie)] -
        { data %>% select(3:ncol(.)) }) %>%
      round(5) %>% 
      return()
  }
  
  
  
  FMGoalie <- 
    {
      read_html("D:/Documents/Sports Interactive/Football Manager 2024/EXPORTS/statistics.html", encoding = "UTF-8") %>% 
        html_table() %>% 
        .[[1]] %>% 
        dplyr::select(
          Name:Mins,
          `Av Rat`:PoM,
          Won:`xGP`
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
          `xSave%`= `xSv %`,
          `xG Prevented` = xGP
        ) %>% 
        dplyr::mutate(
          across(
            c(
              `Minutes Played`:`xG Prevented`
            ),
            .fns = str_replace_all,
            pattern = "[^-\\d\\.]+",
            replacement = ""
          ),
          Club = 
            case_when(
              Club == "Accra FC" ~ "Adowa Accra FC",
              Club == "São Paulo" ~ "União São Paulo",
              Club == "Red Star" ~ "Red Star Laos",
              Club == "E. Europe" ~ "Eastern Europe",
              Club == "Walland" ~ "Cymru",
              Club == "Reykjavik U." ~ "Reykjavik United",
              Club == "Montréal U." ~ "Montréal United",
              Club == "North Shore" ~ "North Shore United",
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
      ~ str_replace(.x, "\\.x", "")
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
          is.na(`Average RatingSeason`)  | `Average RatingSeason` == 0 ~ `Average RatingDay`,
          TRUE ~ (
            (`Average RatingDay` + `Average RatingSeason`) * 
              (`AppsSeason` + 1) -
              `Average RatingSeason`*`AppsSeason`
          )
        ) %>% round(2),
      `Save%` = ((`Saves Parried`+`Saves Held`+`Saves Tipped`)/(`Saves Parried`+`Saves Held`+`Saves Tipped`+Conceded)) %>% round(4) * 100,
      `xSave%Day` = 
        case_when(
          is.na(`xSave%Season`) | `xSave%Season` == 0 ~ `xSave%Day`,
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

  aggregateOutfield <-
    tbl(con, "gameDataPlayer") %>%
    filter(Season == season) %>%
    select(
      Name:`Offsides`, xA:`Attempted Presses`, `Goals Outside Box`,
      -Nationality, -Position
    ) %>%
    group_by(Name, Club) %>%
    summarize(
      across(
        `Average Rating`,
        ~ mean(.x, na.rm = TRUE)
      ),
      across(
        !(contains("Rating")),
        ~ sum(.x, na.rm = TRUE)
      )
    ) %>%
    collect()
  
  teamReset <- function(name, teams){
    data <- read.csv2(paste(name,"Reset.csv"))

    
    aggregateOutfield[
      aggregateOutfield$Club %in% teams &
        aggregateOutfield$Name %in% data$Name,
      3:ncol(aggregateOutfield)
    ] <- 
      (aggregateOutfield[
        aggregateOutfield$Club %in% teams & 
          aggregateOutfield$Name %in% data$Name,
        3:ncol(aggregateOutfield)] -
      { data %>% select(3:ncol(.)) }) %>%
      round(5)
    
    return(aggregateOutfield)
  }
  
  
  FMOutfield <- 
    {
      read_html("D:/Documents/Sports Interactive/Football Manager 2024/EXPORTS/statistics.html", encoding = "UTF-8") %>% 
        html_table() %>% 
        .[[1]] %>% 
        dplyr::select(
          `Inf`:`Right Foot`
        ) %>% 
        dplyr::rename(
          Apps = Apps,
          `Minutes Played` = Mins,
          `Distance Run (km)` = Distance,
          `Average Rating` = `Av Rat`,
          `Player of the Match` = `PoM`,
          Goals = Gls,
          Assists = Ast,
          `xG Overperformance` = `xG-OP`,
          
          `Shots on Target` = ShT,
          
          `Blocks` = Blk,
          
          `Penalties Taken` = Pens,
          `Penalties Scored` = `Pens S`,
          
          `Attempted Passes` = `Pas A`,
          `Successful Passes` = `Ps C`,
          `Key Passes` = `K Pas`,
          `Open Play Key Passes` = `OP-KP`,
          
          `Successful Open Play Crosses` = `OP-Crs C`,
          `Attempted Open Play Crosses` = `OP-Crs A`,
          `Successful Crosses` = `Cr C`,
          `Attempted Crosses` = `Cr A`,
          
          `Chances Created` = CCC,
          
          `Successful Headers` = Hdrs,
          `Attempted Headers` = `Hdrs A`,
          `Header%` = `Hdr %`,
          `Key Headers` = `K Hdrs`,
          
          Dribbles = `Drb`,
          
          `Attempted Tackles` = `Tck A`,
          `Tackles Won` = `Tck C`,
          `Tackle%` = `Tck R`,
          `Key Tackles` = `K Tck`,
          
          Interceptions = Itc,
          `Shots Blocked` = `Shts Blckd`,
          Clearances = Clear,
          `Mistakes Leading to Goals` = `Gl Mst`,
          `Yellow Cards` = Yel,
          `Red Cards` = Red,
          Fouls = Fls,
          `Fouls Against` = FA,
          Offsides = Off,
          
          `Progressive Passes` = `Pr Passes`,
          
          `Successful Presses` = `Pres C`,
          `Attempted Presses` = `Pres A`
        ) %>% 
        dplyr::mutate(
          # Starts = 
          #   case_when(
          #     str_detect(Apps, pattern = "\\(") ~
          #       (str_split(Apps, pattern = "\\(", simplify = TRUE)[,1] %>% 
          #          str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
          #          as.numeric()
          #        ),
          #     TRUE ~ Apps %>% as.numeric()
          #   ),
          # Subs = 
          #   case_when(
          #     str_detect(Apps, pattern = "\\(") ~
          #       (str_split(Apps, pattern = "\\(", simplify = TRUE)[,ncol(str_split(Apps, pattern = "\\(", simplify = TRUE))] %>% 
          #          str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
          #          as.numeric()
          #       ),
          #     TRUE ~ 0
          #   ),
          Club = 
            case_when(
              Club == "Accra FC" ~ "Adowa Accra FC",
              Club == "São Paulo" ~ "União São Paulo",
              Club == "Red Star" ~ "Red Star Laos",
              Club == "Walland" ~ "Cymru",
              Club == "E. Europe" ~ "Eastern Europe",
              Club == "Reykjavik U." ~ "Reykjavik United",
              Club == "Montréal U." ~ "Montréal United",
              Club == "North Shore" ~ "North Shore United",
              TRUE ~ Club
            ),
          `Left Foot` = 
            case_when(
              `Left Foot` == "Very Strong" ~ 20,
              `Left Foot` == "Strong" ~ 15,
              TRUE ~ 10
            ),
          `Right Foot` = 
            case_when(
              `Right Foot` == "Very Strong" ~ 20,
              `Right Foot` == "Strong" ~ 15,
              TRUE ~ 10
            )
        ) %>% 
        # dplyr::mutate(
        #   Apps = rowSums(data.frame(.$Starts, .$Subs), na.rm = TRUE)
        # ) %>%
        # dplyr::select(
        #   -Starts, -Subs
        # ) %>% 
        mutate(
          across(
            c(
              `Minutes Played`:`Attempted Presses`
            ),
            .fns = str_replace_all,
            pattern = "[^-\\d\\.]+",
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
          `Pass%` = 
            (`Successful Passes` %>% as.numeric()/
               `Attempted Passes` %>% as.numeric()) %>% 
            round(4)*100,
          `Header%` = 
            (`Successful Headers` %>% as.numeric()/
               `Attempted Headers` %>% as.numeric()) %>% 
            round(4)*100,
          `Cross%` = 
            (`Successful Crosses` %>% as.numeric()/
               `Attempted Crosses` %>% as.numeric()) %>% 
            round(4)*100,
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
          # -Rec
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
  
  aggregateOutfield <- 
    aggregateOutfield[, 
                      colnames(FMOutfield)[
                        colnames(FMOutfield) %in% 
                          colnames(aggregateOutfield)
                        ]
                      ]
  
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
      !contains("\\.y")
    ) %>% 
    rename_with(
      ~ str_replace(.x, "\\.x", "")
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
          is.na(`Average RatingSeason`) | `Average RatingSeason` == 0 ~ `Average RatingDay`,
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
            if_else(xG < 0, 0, xG),
          xA =
            if_else(xA < 0, 0, xA)
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

season <- "14"

date <- "2024-04-10" %>% as.Date()

{
  ## Adding a deauthorization for reading of Google Sheets that are still being used. 
  googlesheets4::gs4_deauth()
  
  matchGoalie <- goalieOutput(season, date) %>% 
    unique()
  
  matchOutfield <- outfieldOutput(season, date) %>% 
    unique()
  
  table(matchOutfield$Result) %>% print()
  
  table(
    paste(matchOutfield$Club, matchOutfield$Opponent, sep = "-")
    ) %>% print()
  
  ## Checks sum of minutes played per player per team
  sum(matchOutfield$`Minutes Played`)/length(unique(matchOutfield$Club))/11
}
## Writing to the database
dbAppendTable(con, "gameDataPlayer", matchOutfield)
dbAppendTable(con, "gameDataKeeper", matchGoalie)

# ### Filters in case MAJ and MIN are in same file
# matchGoalie <- matchGoalie %>% filter(!is.na(Opponent))
# matchOutfield <- matchOutfield %>% filter(!is.na(Opponent))


dbDisconnect(con)












