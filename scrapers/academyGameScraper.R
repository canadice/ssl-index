
###########################################################################
###########################################################################
###                                                                     ###
###                   Statistics Parser for the SSL Academy                ###
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
  
  FMGoalie <- 
    {
      read_html("D:/Football Manager 2022/screenshots/academy.html", encoding = "UTF-8") %>% 
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
  
  dbWriteTable(con, "academyGoalie", FMGoalie, overwrite = TRUE)
}

outfieldFunction <- function(season){
  
  FMOutfield <- 
    {
      read_html("D:/Football Manager 2022/screenshots/academy.html", encoding = "UTF-8") %>% 
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
  
  dbWriteTable(con, "academyOutfield", FMOutfield, overwrite = TRUE)
  
}


### Start here
goalieFunction(season = 11)
outfieldFunction(season = 11)

dbDisconnect(con)












