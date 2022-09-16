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
require(plotly)
require(tidyr)
require(sslrtools)
require(dplyr)
require(DBI)
require(dbplyr)
require(RSQLite)
# require(googlesheets4)

# mydrop = 316926952
# plumberDeploy::do_deploy_api(mydrop, path = "ssl", localPath = ".api/", port = 8000, docs = TRUE, overwrite = TRUE)

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
  
  playerGameData <- 
    tbl(con, "gameDataPlayer") %>% 
    filter(
      Name == player
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
        Dist = `Distance Run (km)`,
        Rat = `Average Rating`,
        G = Goals,
        A = Assists,
        xG,
        `Pas%` = `Pass%`,
        `Crs%`= `Cross%`,
        `Hdr%` = `Header%`,
        `Tck%` = `Tackle%`,
        YC = `Yellow Cards`,
        RC = `Red Cards`,
        Res = Result,
        Opp = Opponent
      ) %>%  
      collect() %>% 
      slice_tail(n = 5)
    
    paste(
      paste(colnames(playerGameData), collapse = "\t"),
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
  
  playerGameData <- 
    tbl(con, "gameDataPlayer") %>% 
    dplyr::mutate(
      Season = as.numeric(Season),
      Division = as.character(Division)
    ) %>% 
    dplyr::filter(
      Name == player
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
  
  playerGameData <- 
    tbl(con, "gameDataPlayer") %>% 
    dplyr::mutate(
      Season = as.numeric(Season),
      Division = as.character(Division)
    ) %>% 
    dplyr::filter(
      Name == player
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
      ) 
    
    barplot(
      names.arg = visData$attributeIndex, 
      height = visData$Rating,
      col = "#BD9523",
      cex.names = 0.8,
      ylim = c(0,20),
      las = 2
      )
  }
  
  
}


#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
