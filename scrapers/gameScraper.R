
###########################################################################
###########################################################################
###                                                                     ###
###                   GAME-BY-GAME PARSER FOR THE SSL                   ###
###                                                                     ###
###########################################################################
###########################################################################

require(rvest)
require(stringr)
require(stringi)
require(plyr)
require(dplyr)
require(tidyr)
require(dplyover)


source("D:/GitHubs/ssl-index/SSL-Index/app-documents/dataLoader.R")

goalieFunction <- function(season){
  aggregateGoalie <- 
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
      sheet = "KeeperGameData"
    ) %>% 
    filter(
      Season == season
    ) %>% 
    group_by(
      Name,
      Club
    ) %>% 
    dplyr::summarize(
      across(
        where(is.numeric),
        ~ sum(.x, na.rm = TRUE)
      )
    ) %>% 
    mutate(
      `Average Rating` = `Average Rating`/(Apps-1),
      `xSave%` = `xSave%`/(Apps -1)
    )
  
  FMGoalie <- 
    {
      read_html("D:/FootballManager2022/screenshots/goalieTemp.html", encoding = "UTF-8") %>% 
        html_table() %>% 
        .[[1]] %>% 
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
        mutate(
          across(
            c(
              `Minutes Played`:`xSave%`
            ),
            .fns = str_replace_all,
            pattern = "[^\\d\\.]+",
            replacement = ""
          )
        ) %>% 
        mutate(
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
        mutate(
          across(
            !contains(
              c("Name", "Information", "Nationality", "Position", "Club")
            ),
            as.numeric
          )
        ) %>% 
        mutate(
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
        select(
          -`Inf`,
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
      Apps == 1
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
        ),
      `Save%` = ((`Saves Parried`+`Saves Held`+`Saves Tipped`)/(`Saves Parried`+`Saves Held`+`Saves Tipped`+Conceded)) %>% round(4) * 100,
      `xSave%Day` = 
        case_when(
          is.na(`xSave%Season`) ~ `xSave%Day`,
          TRUE ~ (
            (`xSave%Day` + `xSave%Season`) * 
              (`AppsSeason` + 1) -
              `xSave%Season`*`AppsSeason`
          )
        ) 
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
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
      sheet = "PlayerGameData"
    ) %>% 
    filter(
      Season == season
    ) %>% 
    group_by(
      Name,
      Club
    ) %>% 
    dplyr::summarize(
      across(
        where(is.numeric),
        ~ sum(.x, na.rm = TRUE)
      )
    ) %>% 
    mutate(
      `Average Rating` = `Average Rating`/(Apps-1)
    )
  
  FMOutfield <- 
    {
      read_html("D:/FootballManager2022/screenshots/playerTemp.html", encoding = "UTF-8") %>% 
        html_table() %>% 
        .[[1]] %>% 
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
          Starts = (str_split(Apps, pattern = "\\(", simplify = TRUE)[,1] %>% 
                      as.numeric()),
          Subs = (str_split(Apps, pattern = "\\(", simplify = TRUE)[,2] %>% 
                    str_extract(pattern = "[0-9]+") %>% 
                    as.numeric())
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
          Nationality = 
            case_when(
              str_detect(Name, "GFuel") ~ 
                Name %>% 
                str_split(
                  pattern = " - ", 
                  simplify = TRUE
                ) %>% 
                .[,3],
              TRUE ~ Name %>% 
                str_split(
                  pattern = " - ", 
                  simplify = TRUE
                ) %>% 
                .[,2]
            ),
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
      Apps == 1
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
        ),
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
        )
    }
  
  return(matchOutfield)
}

### Start here

season <- 3

date <- "2022-05-18" %>% as.Date()

matchGoalie <- goalieOutput(season, date) %>% 
  unique()

matchOutfield <- outfieldOutput(season, date) %>% 
  unique()

## Writing data to Google Sheet for easier distribution
googlesheets4::gs4_auth(path = ".secrets/client_secret.json")

## Writes the current scrape data to the sheet
googlesheets4::sheet_append(
  data = matchGoalie,
  ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
  sheet = "KeeperGameData"
)

## Writes the current scrape data to the sheet
googlesheets4::sheet_append(
  data = matchOutfield,
  ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
  sheet = "PlayerGameData"
)











