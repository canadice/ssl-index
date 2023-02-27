#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

require(plumber, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(googlesheets4, quietly = TRUE)
require(geomtextpath, quietly = TRUE)
require(stringr, quietly = TRUE)
require(plotly, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(tidyselect, quietly = TRUE)
require(sslrtools, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(DBI, quietly = TRUE)
require(dbplyr, quietly = TRUE)
require(RSQLite, quietly = TRUE)
require(jsonlite, quietly = TRUE)
require(reactable, quietly = TRUE)
require(htmltools, quietly = TRUE)
require(knitr, quietly = TRUE)
require(rmarkdown, quietly = TRUE)
# require(googlesheets4)

googlesheets4::gs4_deauth()

if(Sys.info()["sysname"] == "Linux"){
  Sys.setenv(HOME = "/root")  
}


#* Allows acces from cross domain places
# Enable CORS Filtering
#' @filter cors
cors <- function(req, res) {
  safe_domains <- c("https://api.simulationsoccer.com", 
                    "https://simsoccer.jcink.net",
                    "http://sslforums.com")
  
  if (any(grepl(pattern = paste0(safe_domains,collapse="|"), req$HTTP_REFERER,ignore.case=T))) {
    res$setHeader("Access-Control-Allow-Origin", sub("/$","",req$HTTP_REFERER)) #Have to remove last slash, for some reason
    
    if (req$REQUEST_METHOD == "OPTIONS") {
      res$setHeader("Access-Control-Allow-Methods","GET,HEAD,PUT,PATCH,POST,DELETE") #This is how node.js does it
      res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
      res$status <- 200
      return(list())
    } else {
      plumber::forward()
    }
  } else {
    plumber::forward()
  }
}


#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Checks the R and package versions
#* @get /status
function(req, res) {
  res$setHeader("Content-Type", "application/json")
  res$status <- 200L
  res$body <- jsonlite::toJSON(
    sessioninfo::session_info(), auto_unbox = TRUE, null = "null"
    # Sys.getenv("HOME")
  )
  res
}

#* Search for a player in the game log data
#* @param player The player to search for
#* @get /playerLog
#* @serializer print
function(player = "") {
  ## Downloads a local file for the database
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  player <- str_to_title(player)
  
  playerGameData <- 
    tbl(con, "gameDataPlayer") %>% 
    filter(
      Name %like% player
    ) 
  
  if(playerGameData %>% select(Name) %>% collect() %>% nrow() == 0){
    print("The chosen player does not exist in the league. Please check the spelling.")
  } else {
    playerGameData <- 
      playerGameData %>% 
      dplyr::mutate(
        Season = as.numeric(Season),
        Division = as.character(Division)
      ) %>% 
      select(
        Min = `Minutes Played`,
        Rat = `Average Rating`,
        G = Goals,
        A = Assists,
        xG,
        `Pas%` = `Pass%`,
        `Hdr%` = `Header%`,
        Int = Interceptions,
        Red = `Red Cards`,
        Opp = Opponent,
        Res = Result
      ) %>% 
      dplyr::mutate(
        xG = round(xG, 2)
      ) %>% 
      collect() %>% 
      slice_tail(n = 5)
    
    dbDisconnect(con)
    
    paste(
      paste(colnames(playerGameData), collapse = "\t"),
      paste(rep("----", times = 10), collapse = ""),
      paste(playerGameData[1,], collapse = "\t"),
      paste(playerGameData[2,], collapse = "\t"),
      paste(playerGameData[3,], collapse = "\t"),
      paste(playerGameData[4,], collapse = "\t"),
      paste(playerGameData[5,], collapse = "\t"),
      sep = "\n"
    ) %>% 
      print()
  }
  
}

#* Gathers player statistics for a given season and competition
#* @param player The player to search for
#* @param league The specific league or competition to search for
#* @param season The specific season to get the stats from
#* @get /playerStats
#* @serializer json
function(player = "", league = NULL, season = NULL, res) {
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  player <- str_to_title(player)
  
  playerGameData <- 
    tbl(con, "gameDataPlayer") %>% 
    filter(
      Name %like% player
    )
  
  keeperGameData <- 
    tbl(con, "gameDataKeeper") %>% 
    filter(
      Name %like% player
    )
  
  if(season %>% is.null()){
    playerGameData <- 
      playerGameData %>% 
      filter(
        Season == max(Season, na.rm = TRUE)
      )
    
    keeperGameData <- 
      keeperGameData %>% 
      filter(
        Season == max(Season, na.rm = TRUE)
      )
  } else {
    playerGameData <- 
      playerGameData %>% 
      filter(
        Season == season
      ) 
    
    keeperGameData <- 
      keeperGameData %>% 
      filter(
        Season == season
      ) 
  }  
  
  if(league %>% is.null()){
    #Do nothing
  } else {
    playerGameData <- 
      playerGameData %>% 
      filter(
        Division %like% league
      ) 
    
    keeperGameData <- 
      keeperGameData %>% 
      filter(
        Division %like% league
      ) 
  } 
  
  if(playerGameData %>% select(Name) %>% collect() %>% nrow() == 0){
    res$status <- 400  
    list(error = "The chosen player does not exist in the league the chosen season. Please make another query.")
  } else {
    playerGameData <- 
      playerGameData %>% 
      dplyr::summarize(
        Name,
        Position,
        across(
          c(Apps, 
            Goals, 
            Assists, 
            xG, 
            `Key Passes`,
            `Key Headers`, 
            `Key Tackles`,
            `Chances Created`,
            `Interceptions`,
            `Clearances`,
            `Distance Run (km)`,
            `Penalties Scored`,
            `Fouls`,
            `Yellow Cards`,
            `Red Cards`,
            `Player of the Match`
          ),
          sum,
          na.rm = TRUE
        ),
        across(
          `Average Rating`,
          mean,
          na.rm = TRUE
        )
      ) %>% 
      dplyr::mutate(
        `Average Rating` = round(`Average Rating`, 2)
      ) %>% 
      collect()
    
    keeperGameData <- 
      keeperGameData %>% 
      dplyr::summarize(
        Name,
        across(
          c(Won, 
            Lost, 
            `Clean Sheets`, 
            `Conceded`,
            `Saves Parried`, 
            `Saves Held`,
            `Saves Tipped`
          ),
          sum,
          na.rm = TRUE
        ),
        across(
          c(
            `xSave%`
          ),
          mean,
          na.rm = TRUE
        )
      ) %>% 
      dplyr::mutate(
        `xSave%` = round(`xSave%`, 2),
        `Save%` = 
          ((`Saves Parried` + `Saves Held` + `Saves Tipped`) / 
             (`Saves Parried` + `Saves Held` + `Saves Tipped` + Conceded)) %>% round(3)*100
      ) %>% 
      collect()
    
    dbDisconnect(con)
    
    playerGameData %>% 
      left_join(
        keeperGameData,
        by = "Name"
      ) %>% 
      dplyr::mutate(
        across(
          Won:last_col(),
          as.numeric
        )
      ) %>%
      dplyr::mutate(
        across(
          Won:last_col(),
          ~ replace_na(.x, 0)
        )
      ) 
  }
  
}

#* Visualize a players attributes using plotly
#* @param player The player to search for
#* @param season The season to visualize
#* @get /playerGraph
#* @serializer htmlwidget
function(player = "", season = NULL) {
  ## Downloads a local file for the database
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  player <- str_to_title(player)
  
  playerGameData <- 
    tbl(con, "gameDataPlayer") %>% 
    dplyr::mutate(
      Season = as.numeric(Season),
      Division = as.character(Division)
    ) %>% 
    dplyr::filter(
      Name %like% player
    ) 
  
  if(season %>% is.null()){
    playerGameData <- 
      playerGameData %>% 
      filter(
        Season == max(Season, na.rm = TRUE)
      ) 
  } else {
    playerGameData <- 
      playerGameData %>% 
      filter(
        Season == season
      )
  }
  
  if(playerGameData %>% select(Name) %>% collect() %>% nrow() == 0){
    stop("The chosen player does not exist in the league for that season. Please check the spelling.")
  } else {
    visData <- 
      playerGameData %>% 
      collect() %>% 
      tail(1) %>% 
      dplyr::select(
        Acc:Wor
      ) %>%  
      dplyr::mutate(
        DEFENDING =
          (Mar + Tck + Pos)/3,
        PHYSICAL =
          (Agi + Bal + Sta + Str)/4,
        SPEED =
          (Acc + Pac)/2,
        VISION =
          (Pas + Fla + Vis)/3,
        ATTACKING =
          (Fin + Cmp + OtB)/3,
        TECHNICAL =
          (Dri + Fir + Tec)/3,
        AERIAL =
          (Hea + Jum)/2,
        MENTAL =
          (Ant + Bra + Cnt + Dec + Det + Tea)/6
      ) %>% 
      dplyr::select(
        DEFENDING:MENTAL
      ) %>% 
      pivot_longer(
        where(is.numeric),
        names_to = "attributeIndex",
        values_to = "Rating"
      ) %>% 
      mutate(
        text = paste(attributeIndex, Rating, sep = ": ")
      ) %>% 
      ## Adds a duplicated first observation to allow the lines to connect
      add_row(
        .[1,]
      )
    
    dbDisconnect(con)
    
    fig <- 
      plot_ly(
        data = visData,
        r = 0,
        theta = ~attributeIndex,
        width = 400,
        height = 250
      ) %>% 
      add_trace(
        type = 'scatterpolar',
        mode = 'lines',
        r = ~Rating,
        theta = ~attributeIndex,
        text = ~text,
        fill = 'none',
        hoverinfo = "text",
        line = 
          list(
            color = "#ffffff",
            width = 3
          ),
        data = visData
      ) %>% 
      add_trace(
        type = 'barpolar',
        width = 360,
        hoverinfo = "none",
        r = 10,
        marker = 
          list(
            color = "#B81D13"
          )
      ) %>% 
      add_trace(
        type = 'barpolar',
        width = 360,
        hoverinfo = "none",
        r = 5,
        marker = 
          list(
            color = "#EFB700"
          )
      ) %>% 
      add_trace(
        type = 'barpolar',
        width = 360,
        hoverinfo = "none",
        r = 5,
        marker = 
          list(
            color = "#008450"
          )
      ) 
    
    
    fig %>%
      plotly::config(
        modeBarButtonsToRemove =
          c("zoom", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
            "resetScale2d", "hoverClosestCartesian",
            "hoverCompareCartesian", "toggleSpikelines"
          )
      ) %>%
      layout(
        autosize = TRUE,
        dragmode= FALSE,
        polar =
          list(
            radialaxis =
              list(
                visible = FALSE,
                range = c(0,20)
              )
          ),
        ## Legend is put to false so the plot is the same size
        showlegend = FALSE,
        paper_bgcolor='#ffffff00',
        plot_bgcolor='#ffffff00'
      )
  }
  
}

#* Visualize a players attributes using ggplot2
#* @param player The player to search for
#* @param season The season to visualize
#* @get /playerGraphSimple
#* @serializer png
function(player = "", season = NULL) {
  ## Downloads a local file for the database
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  player <- str_to_title(player)
  
  if(is.null(season)){
    data <- 
      tbl(con, "Daily_Scrape") %>% 
      dplyr::filter(
        Name %like% player
      ) %>% 
      collect() %>% 
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
      select(
        Name,
        DEFENDING:MENTAL
      )

  } else {
    data <- 
      tbl(con, "gameDataPlayer") %>% 
      dplyr::mutate(
        Season = as.numeric(Season),
        Division = as.character(Division)
      ) %>% 
      dplyr::filter(
        Name %like% player
      ) %>% 
      filter(
        Season == season
      ) %>% 
      collect() %>% 
      tail(1) %>% 
      dplyr::select(
        Name,
        Acc:Wor
      ) %>%  
      dplyr::mutate(
        DEFENDING =
          (Mar + Tck + Pos)/3,
        PHYSICAL =
          (Agi + Bal + Sta + Str)/4,
        SPEED =
          (Acc + Pac)/2,
        VISION =
          (Pas + Fla + Vis)/3,
        ATTACKING =
          (Fin + Cmp + OtB)/3,
        TECHNICAL =
          (Dri + Fir + Tec)/3,
        AERIAL =
          (Hea + Jum)/2,
        MENTAL =
          (Ant + Bra + Cnt + Dec + Det + Tea)/6
      ) %>% 
      dplyr::select(
        Name,
        DEFENDING:MENTAL
      )
  }
  
  if(data %>% select(Name) %>% collect() %>% nrow() == 0){
    stop("The chosen player does not exist in the league for that season. Please check the spelling.")
  } else {
    visData <- 
      data %>% 
      pivot_longer(
        where(is.numeric),
        names_to = "attributeIndex",
        values_to = "Rating"
      ) %>% 
      mutate(
        text = paste(attributeIndex, Rating, sep = ": ")
      ) 
    
    dbDisconnect(con)
    
    p <- 
      ggplot(visData) + aes(x = attributeIndex, y = Rating) +
      geom_col(aes(y = rep(20, times = 8)), width = 1, fill = "#008450") +
      geom_col(aes(y = rep(15, times = 8)), width = 1, fill = "#EFB700") +
      geom_col(aes(y = rep(10, times = 8)), width = 1, fill = "#B81D13") +
      geom_col(width = 1, fill = "#E0E1E9", color = "grey50") +
      # geom_line(aes(group = 1), size = 2, color = "black") + 
      theme_bw() +
      scale_y_continuous(limits = c(0, 20), breaks = NULL) +
      coord_curvedpolar(theta = "x", direction = -1) +
      labs(x = NULL, 
           y = NULL, 
           title = data$Name %>% unique(), 
           subtitle = 
             if_else(season %>% is.null(), "", paste("S",season, sep = "")
                     )
           )+
      theme(
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 26, hjust = 0.5, face = "bold", color = "white"),
        plot.subtitle = element_text(size = 22, hjust = 0.5, face = "bold", color = "white"),
        axis.text.x = element_text(size = 20, vjust = 0.5, face = "bold", color = "white"),
        panel.background = element_rect(fill = "black"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    print(p)
  }
  
  
}

#* Shows league leaders in different statistics
#* @param league The specific league or competition to search for
#* @param season The specific season to get the stats from
#* @get /leaders
#* @serializer json
function(league = NULL, season = NULL) {
  ## Downloads a local file for the database
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  playerGameData <- 
    tbl(con, "gameDataPlayer") 
  
  keeperGameData <- 
    tbl(con, "gameDataKeeper")
  
  if(season %>% is.null()){
    playerGameData <- 
      playerGameData %>% 
      filter(
        Season == max(Season, na.rm = TRUE)
      )
    
    keeperGameData <- 
      keeperGameData %>% 
      filter(
        Season == max(Season, na.rm = TRUE)
      )
  } else {
    playerGameData <- 
      playerGameData %>% 
      filter(
        Season == season
      ) 
    
    keeperGameData <- 
      keeperGameData %>% 
      filter(
        Season == season
      ) 
  }  
  
  if(league %>% is.null()){
    #Do nothing
  } else {
    playerGameData <- 
      playerGameData %>% 
      filter(
        Division %like% league
      ) 
    
    keeperGameData <- 
      keeperGameData %>% 
      filter(
        Division %like% league
      ) 
  }
    
  filteredData <- 
    playerGameData %>% 
    group_by(Name) %>% 
    dplyr::summarize(
      across(
        c(Apps, 
          Goals, 
          Assists, 
          xG, 
          `Key Passes`,
          `Key Headers`, 
          `Key Tackles`,
          `Chances Created`,
          `Interceptions`,
          `Distance Run (km)`,
          `Penalties Scored`,
          `Yellow Cards`,
          `Red Cards`,
          `Player of the Match`
        ),
        sum,
        na.rm = TRUE
      ),
      across(
        `Average Rating`,
        mean,
        na.rm = TRUE
      )
    ) %>% 
    dplyr::mutate(
      `Average Rating` = round(`Average Rating`, 2)
    ) %>% 
    collect()
  
  
  temp <- 
    keeperGameData %>% 
    group_by(Name) %>% 
    dplyr::summarize(
      across(
        c(Won, 
          Lost, 
          `Clean Sheets`, 
          `Conceded`,
          `Saves Parried`, 
          `Saves Held`,
          `Saves Tipped`
        ),
        sum,
        na.rm = TRUE
      ),
      across(
        c(
          `xSave%`
          ),
        mean,
        na.rm = TRUE
      )
    ) %>% 
    dplyr::mutate(
      `xSave%` = round(`xSave%`, 2),
      `Save%` = 
        ((`Saves Parried` + `Saves Held` + `Saves Tipped`) / 
           (`Saves Parried` + `Saves Held` + `Saves Tipped` + Conceded)) %>% round(3)*100
    ) %>% 
    collect()
  
  dbDisconnect(con)
  
  filteredData %>% 
    left_join(
      temp,
      by = "Name"
    ) %>% 
    mutate(
      across(
        Won:last_col(),
        ~ replace_na(.x, 0)
      )
    )
  
}

#* Gather the latest AC and Affiliate Task for a given player
#* @param player The player to search for
#* @get /cappedPT
function(player = NULL){
  tryCatch({
    con <-
      dbConnect(
        SQLite(),
        "../database/SSL_Database.db"
      )
    
    username <- 
      tbl(con, "Daily_Scrape") %>% 
      filter(
        Name == player
      ) %>% 
      select(
        Username
      ) %>% 
      collect() %>% 
      c()
    
    recentAC <- activityCheckLinks()
    recentAffiliate <- affiliateLinks()
    
    postAC <- lapply(X = recentAC, activityCheckPosts) %>% 
      do.call(rbind, args = .)
    
    postAffiliate <- lapply(X = recentAffiliate, affiliatePosts) %>% 
      do.call(rbind, args = .)
    
    output <- NA
    
    if(username %in% postAC$User){
      output[1] <- 
        paste(
          "You have posted in the most recent AC! Here is the link to your post:", 
          postAC %>% 
            filter(User == username) %>% 
            select(Link)
        )
    } else {
      output[1] <- 
        paste(
          "You have not posted in the recent thread! Here is the link to it:", 
          postAC$Link[1] %>% 
            stringr::str_extract(pattern = ".+&showtopic=[0-9]+")
          
        )
    }
    
    
    if(username %in% postAffiliate$User){
      output[2] <- 
        paste(
          "You have posted in the most recent AC! Here is the link to your post", 
          postAffiliate %>% 
            filter(User == username) %>% 
            select(Link)
        )
    } else {
      output[2] <- 
        paste(
          "You have not posted in the recent thread! Here is the link to it:", 
          postAffiliate$Link[1] %>% 
            stringr::str_extract(pattern = ".+&showtopic=[0-9]+")
        )
    }
    
    dbDisconnect(con)
    
    output
  }, error = function(e) {
    res$status = 400  # the response object that is always available in plumber functions
    return(list(error = e, traceback = ...))
  })
}

#* Return interactive plot using plotly
#* @serializer htmlwidget
#* @get /plotly
function() {
  tryCatch({
    p <- ggplot(data = diamonds,
                aes(x = cut, fill = clarity)) +
      geom_bar(position = "dodge")
    
    ggplotly(p)
  }, error = function(e) {
    res$status = 400  # the response object that is always available in plumber functions
    return(list(error = e, traceback = ...))
  })
}

#* Return the player data for a given user
#* @param username The username of the user
#* @serializer json
#* @get /getPlayer
function(username = NULL) {
  if(is.null(username)){
    res$status = 400  # the response object that is always available in plumber functions
    return("You have not sent in a username.")
  }
  
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  player <- 
    tbl(con, "Daily_Scrape") %>% 
    filter(
      Username == username
    ) %>% 
    filter(
      # Finds the latest player created for the username
      Created == max(Created, na.rm = TRUE)
    ) %>% 
    collect()
  
  dbDisconnect(con)
  
  return(player)
}

#* Return a list of players in the SSL
#* @serializer json
#* @get /listPlayers
function() {
  con <-
    dbConnect(
      SQLite(),
      "../database/SSL_Database.db"
    )
  
  players <- 
    tbl(con, "Daily_Scrape") %>%
    filter(
      Team != "Retired"
    ) %>% 
    select(Username, Name) %>% 
    arrange(Name) %>% 
    collect() 
  
  dbDisconnect(con)
  
  playerNames <- as.list(players$Name)
  
  names(playerNames) <- players$Username
    
  return(playerNames %>% toJSON(named = TRUE))
}

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
  
  
  ## Downloads the shedule data only if 
  if(file.exists(paste(division, season, "standingsData.RData"))){
    load(file = paste(division, season, "standingsData.RData"))
    
    if(difftime(Sys.time(), now, units = "secs") < 600){
      # DO NOTHING
    } else {
      sheet <-
        read_sheet(
          ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing",
          sheet = paste("Season", season)
        ) 
      
      now <- Sys.time()
      
      save("sheet", "now", file = paste(division, season, "standingsData.RData"))
    }
  } else {
    sheet <-
      read_sheet(
        ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing",
        sheet = paste("Season", season)
      ) 
    
    now <- Sys.time()
    
    save("sheet", "now", file = paste(division, season, "standingsData.RData"))
  }
  
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

# Programmatically alter your API
#* @plumber
function(pr) {
  handler_error <- function(req, res, err){
    res$status <- 500
    list(error = paste("Custom Error Message", err))
  }
  
  pr %>%
    # Overwrite the default serializer to return unboxed JSON
    pr_set_serializer(serializer_unboxed_json()) %>% 
    pr_set_debug(TRUE) %>% 
    pr_get("/error", function() log("a")) %>%
    pr_set_error(handler_error)
}



  
