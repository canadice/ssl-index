#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

require(plumber)
require(ggplot2)
require(geomtextpath)
require(stringr)
require(plotly)
require(tidyr)
require(tidyselect)
require(sslrtools)
require(dplyr)
require(DBI)
require(dbplyr)
require(RSQLite)
# require(googlesheets4)

# mydrop = Sys.getenv("DROPLET_ID")
# plumberDeploy::do_deploy_api(mydrop, path = "ssl", localPath = ".api/", port = 8000, docs = TRUE, overwrite = TRUE)
# rm(list = ls(envir = analogsea:::analogsea_sessions), envir = analogsea:::analogsea_sessions)

#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* @get /
function() {
  Sys.Date()
}

#* Search for a player in the game log data
#* @param player The player to search for
#* @get /playerLog
#* @serializer print
function(player = "") {
  ## Downloads a local file for the database
  dbFile <- tempfile(fileext = ".db")
  
  dbUrl <- ("https://github.com/canadice/ssl-index/blob/main/database/SSL_Database.db?raw=true")
  
  download.file(dbUrl, destfile = dbFile, mode = "wb")
  
  con <-
    dbConnect(
      SQLite(),
      dbFile
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
    
    paste(
      "Last 5 games played", "\n",
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
  ## Downloads a local file for the database
  dbFile <- tempfile(fileext = ".db")
  
  dbUrl <- ("https://github.com/canadice/ssl-index/blob/main/database/SSL_Database.db?raw=true")
  
  download.file(dbUrl, destfile = dbFile, mode = "wb")
  
  con <-
    dbConnect(
      SQLite(),
      dbFile
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
    
    playerGameData %>% 
      left_join(
        keeperGameData,
        by = "Name"
      ) %>% 
      mutate(
        across(
          Won:last_col(),
          ~ replace_na(.x, 0)
        )
      )
  }
  
}

#* Search for a player in the game log data
#* @param player The player to search for
#* @param season The season to visualize
#* @get /playerGraph
#* @serializer htmlwidget
function(player = "", season = NULL) {
  ## Downloads a local file for the database
  dbFile <- tempfile(fileext = ".db")
  
  dbUrl <- ("https://github.com/canadice/ssl-index/blob/main/database/SSL_Database.db?raw=true")
  
  download.file(dbUrl, destfile = dbFile, mode = "wb")
  
  con <-
    dbConnect(
      SQLite(),
      dbFile
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

#* Search for a player in the game log data
#* @param player The player to search for
#* @param season The season to visualize
#* @get /playerGraphSimple
#* @serializer png
function(player = "", season = NULL) {
  ## Downloads a local file for the database
  dbFile <- tempfile(fileext = ".db")
  
  dbUrl <- ("https://github.com/canadice/ssl-index/blob/main/database/SSL_Database.db?raw=true")
  
  download.file(dbUrl, destfile = dbFile, mode = "wb")
  
  con <-
    dbConnect(
      SQLite(),
      dbFile
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
    
    p <- 
      ggplot(visData) + aes(x = attributeIndex, y = Rating) +
      geom_col(aes(y = rep(20, times = 8)), width = 1, fill = "#008450") +
      geom_col(aes(y = rep(15, times = 8)), width = 1, fill = "#EFB700") +
      geom_col(aes(y = rep(10, times = 8)), width = 1, fill = "#B81D13") +
      geom_col(width = 1, fill = "#E0E1E9", color = "grey70") +
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
        plot.title = element_text(size = 26, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 22, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 20, vjust = 0.5, face = "bold", color = "black"), 
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
  dbFile <- tempfile(fileext = ".db")
  
  dbUrl <- ("https://github.com/canadice/ssl-index/blob/main/database/SSL_Database.db?raw=true")
  
  download.file(dbUrl, destfile = dbFile, mode = "wb")
  
  con <-
    dbConnect(
      SQLite(),
      dbFile
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

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return interactive plot using plotly
#* @serializer png
#* @get /plotly
function() {
  tryCatch({
    p <- ggplot(data = diamonds,
                aes(x = cut, fill = clarity)) +
      geom_bar(position = "dodge")
    
    print(p)
  }, error = function(e) {
    res$status = 400  # the response object that is always available in plumber functions
    return(list(error = e, traceback = ...))
  })
}


# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
