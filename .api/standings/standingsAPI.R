#### Standings Functions ####
#* Return the standings of the given division and season
#* @param division The division to get the standings from
#* @param season The season to pull
#* @serializer htmlwidget
#* @get /getStandings
function(division = 1, season = NULL){
  
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  if(season %>% is.null()){
    season <- tbl(con, "gameDataPlayer") %>%
      select(Season) %>%
      filter(Season == max(Season, na.rm = TRUE)) %>%
      collect() %>%
      unlist() %>%
      unique()
  }
  
  
  createStandings <- function(sheet) {
    sheet <- 
      sheet %>%
      mutate(
        across(
          Division:Matchday,
          unlist
        )
      ) %>%
      filter(
        Division == division,
        !(Result %>% is.na())
      ) %>%
      mutate(
        ## Do not convert them to numeric here as there are letters that sometimes indicates a win
        HomeScore =
          stringr::str_split(
            Result,
            pattern = "-",
            simplify = TRUE
          )[,1],
        AwayScore =
          stringr::str_split(
            Result,
            pattern = "-",
            simplify = TRUE
          )[,2]
      ) %>%
      mutate(
        HomePoints =
          case_when(
            str_detect(HomeScore, "e|p") ~ 3,
            HomeScore %>% as.numeric() > AwayScore %>% as.numeric() ~ 3,
            HomeScore %>% as.numeric() == AwayScore %>% as.numeric() ~ 1,
            TRUE ~ 0
          ),
        AwayPoints =
          case_when(
            str_detect(AwayScore, "e|p") ~ 3,
            HomeScore %>% as.numeric() < AwayScore %>% as.numeric() ~ 3,
            HomeScore %>% as.numeric() == AwayScore %>% as.numeric() ~ 1,
            TRUE ~ 0
          )
      ) %>%
      select(
        -`IRL Date`,
        -`In-game Date`,
        -Result
      ) %>%
      pivot_longer(
        c(HomePoints, AwayPoints),
        names_to = c("set", ".value"),
        names_pattern = "(....)(.*)$"
      ) %>%
      mutate(
        Team =
          case_when(
            set == "Home" ~ Home,
            TRUE ~ Away
          ),
        GF =
          case_when(
            set == "Home" ~ HomeScore,
            TRUE ~ AwayScore
          ),
        GA =
          case_when(
            set == "Home" ~ AwayScore,
            TRUE ~ HomeScore
          ),
        Matchday = unlist(Matchday)
      ) %>%
      select(
        -contains("Home"),
        -contains("Away"),
        -set
      ) %>%
      group_by(
        Team
      ) %>%
      summarize(
        GP = n(),
        W = sum(Points == 3),
        D = sum(Points == 1),
        L = sum(Points == 0),
        GF = sum(as.numeric(GF)),
        GA = sum(as.numeric(GA)),
        GD = GF-GA,
        Points = sum(Points)
      ) %>%
      arrange(
        desc(Points),
        desc(GD)
      ) %>%
      ungroup() %>%
      mutate(
        Pos = 1:n()
      ) %>%
      left_join(
        tbl(con, "Team_Information") %>%
          select(
            team,
            logo
          ) %>%
          collect(),
        by = c("Team" = "team")
      ) %>%
      relocate(
        c(Pos, logo),
        .before = Team
      ) %>%
      dplyr::rename(
        ` ` = logo,
        Pts = Points
      )
  }
  
  
  ## Downloads the shedule data only if 
  if(file.exists(paste(division, season, "standingsData.RData"))){
    load(file = paste(division, season, "standingsData.RData"))
    
    # Only updates after three hours
    if(difftime(Sys.time(), now, units = "secs") < 10800){
      # DO NOTHING
    } else {
      sheet <-
        read_sheet(
          ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing",
          sheet = paste("Season", season)
        ) %>% 
        createStandings()
      
      now <- Sys.time()
      
      save("sheet", "now", file = paste(division, season, "standingsData.RData"))
    }
  } else {
    sheet <-
      read_sheet(
        ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing",
        sheet = paste("Season", season)
      ) %>% 
      createStandings()
    
    now <- Sys.time()
    
    save("sheet", "now", file = paste(division, season, "standingsData.RData"))
  }
  
  dbDisconnect(con)
  
  table <- 
    reactable(
      sheet,
      pagination = FALSE,
      fullWidth = FALSE,
      style = list(
        fontFamily = "Roboto",
        background = "hsl(0deg 0% 20%)",
        color = "hsl(0deg 0% 100%)"
      ),
      defaultColDef =
        colDef(
          maxWidth = 60,
          align = "center",
          style = function(value, index){
            list(
              background =
                ifelse(division == 1 & index > 6 & season > 4, "#6D071A", 
                       ifelse(division == 2 & index < 3, "#01731A", "hsl(0deg 0% 20%)"))#,
              # color =
              #   ifelse(division == 1 & index > 6 & season > 4, "#000000", 
              #          ifelse(division == 2 & index < 3, "#000000", "#ffffff"))
              # color =
              #   ifelse(index > 6, "white", "black"),
              # borderTop =
              #   ifelse(index == 7 & season > 4, "solid", "none")
            )
          }
        ),
      columns =
        list(
          ` ` =
            colDef(
              width = 50,
              align = "center",
              class = "cell",
              cell = function(value){
                logo <-
                  img(
                    class = "logo",
                    src = value,
                    height = 30
                  )
                
                div(class = "club", logo)
              }
            ),
          Team =
            colDef(
              width = 150,
              align = "left",
              style =
                function(value, index){
                  list(
                    background =
                      ifelse(division == 1 & index > 6 & season > 4, "#6D071A", 
                             ifelse(division == 2 & index < 3, "#01731A", "hsl(0deg 0% 20%)")),
                    # color =
                    #   ifelse(division == 1 & index > 6 & season > 4, "#000000", 
                    #          ifelse(division == 2 & index < 3, "#000000", "#ffffff")),
                    # color =
                    #   ifelse(index > 6, "white", "black"),
                    # borderTop =
                    #   ifelse(index == 7 & season > 4, "solid", "none"),
                    fontWeight = "bold"
                  )
                }
            )
        )
    )
  
  htmlwidgets::onRender(table, 'function(){
                        var link = document.createElement( "link" );
link.href = "https://simulation-soccer-league.github.io/ssl-forum/css/style.css";
link.type = "text/css";
link.rel = "stylesheet";
link.media = "screen,print";

document.getElementsByTagName( "head" )[0].appendChild( link );
                        document.querySelector("body").style.background = null
                        document.querySelector("#htmlwidget_container").style.margin = "0 auto"
                        }')
}

#* Return the standings data for a specified season
#* @param season The season to pull
#* @serializer json
#* @get /getStandingsData
function(season = NULL){
  
  sheet <-
    read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing",
      sheet = paste("Season", season)
    ) %>% 
    mutate(
      across(
        Division:Matchday,
        unlist
      )
    ) %>% 
    filter(
      Division %in% 1:2,
      !(Result %>% is.na())
    ) %>% 
    mutate(
      ## Do not convert them to numeric here as there are letters that sometimes indicates a win
      HomeScore =
        stringr::str_split(
          Result,
          pattern = "-",
          simplify = TRUE
        )[,1],
      AwayScore =
        stringr::str_split(
          Result,
          pattern = "-",
          simplify = TRUE
        )[,2]
    ) %>%
    mutate(
      HomePoints =
        case_when(
          str_detect(HomeScore, "e|p") ~ 3,
          HomeScore %>% as.numeric() > AwayScore %>% as.numeric() ~ 3,
          HomeScore %>% as.numeric() == AwayScore %>% as.numeric() ~ 1,
          TRUE ~ 0
        ),
      AwayPoints =
        case_when(
          str_detect(AwayScore, "e|p") ~ 3,
          HomeScore %>% as.numeric() < AwayScore %>% as.numeric() ~ 3,
          HomeScore %>% as.numeric() == AwayScore %>% as.numeric() ~ 1,
          TRUE ~ 0
        )
    )%>%
    select(
      -`IRL Date`,
      -`In-game Date`,
      -Result
    ) %>%
    pivot_longer(
      c(HomePoints, AwayPoints),
      names_to = c("set", ".value"),
      names_pattern = "(....)(.*)$"
    ) %>%
    mutate(
      Team =
        case_when(
          set == "Home" ~ Home,
          TRUE ~ Away
        ),
      GF =
        case_when(
          set == "Home" ~ HomeScore,
          TRUE ~ AwayScore
        ),
      GA =
        case_when(
          set == "Home" ~ AwayScore,
          TRUE ~ HomeScore
        ),
      Matchday = unlist(Matchday)
    ) %>%
    select(
      -contains("Home"),
      -contains("Away"),
      -set
    ) %>% 
    group_by(
      Division
    ) %>% 
    group_split() %>%
    lapply(
      X = .,
      FUN = function(division){
        division %>% 
          group_by(
            Team
          ) %>% 
          summarize(
            Division = unique(Division),
            GP = n(),
            W = sum(Points == 3),
            D = sum(Points == 1),
            L = sum(Points == 0),
            GF = sum(as.numeric(GF)),
            GA = sum(as.numeric(GA)),
            GD = GF-GA,
            Points = sum(Points)
          ) %>%
          arrange(
            desc(Points),
            desc(GD)
          ) %>%
          ungroup() %>% 
          mutate(
            Pos = 1:n()
          ) %>% 
          dplyr::rename(
            Pts = Points
          )
      }
    ) %>% 
    return()
}