
###########################################################################
###########################################################################
###                                                                     ###
###                 INDIVIDUAL STATS FOR THE SSL INDEX                  ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
playerStatsUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = ns("season"),
            label = "Select season",
            choices = 
              c(
                1:max(playerGameData$Season) %>% 
                  sort(decreasing = TRUE),
                "ALL"
              )
              
          ),
          selectInput(
            inputId = ns("division"),
            label = "Select division",
            choices = 
              c(
                "ALL" = 'NULL',
                "Cup" = 0,
                1,
                2
              )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          offset = 0,
          tabBox(
            width = NULL,
            selected = "Player Stats",
            tabPanel(
              "Player Stats",
              div(
                id = ns("playerDownload"),
                downloadButton(ns("downloadData"), "Download")
              ),
              h4("Outfield", align = "center"),
              reactableOutput(
                outputId = ns("playerStats")
              ) %>% withSpinner(),
              h4("Goalkeeper", align = "center"),
              reactableOutput(
                outputId = ns("goalieStats")
              ) %>% withSpinner()
            ),
            tabPanel(
              "Outfield Leaders",
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  reactableOutput(
                    outputId = ns("leaderGoals")
                  )
                ),
                column(
                  width = 5,
                  reactableOutput(
                    outputId = ns("leaderAssists")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  reactableOutput(
                    outputId = ns("leaderSoT")
                  )
                ),
                column(
                  width = 5,
                  reactableOutput(
                    outputId = ns("leaderInterceptions")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  reactableOutput(
                    outputId = ns("leaderFouls")
                  )
                ),
                column(
                  width = 5,
                  reactableOutput(
                    outputId = ns("leaderPoM")
                  )
                )
              )
            ),
            tabPanel(
              "Goalkeeper Leaders",
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  reactableOutput(
                    outputId = ns("leaderWins")
                  )
                ),
                column(
                  width = 5,
                  reactableOutput(
                    outputId = ns("leaderSavePerc")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  reactableOutput(
                    outputId = ns("leaderCleanSheets")
                  )
                ),
                column(
                  width = 5,
                  reactableOutput(
                    outputId = ns("leaderConceded")
                  )
                )
              )
            )
          )
        )
      )
    )
 )
}

## Backend module for player similarities
playerStatsSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      con <- 
        dbConnect(
          SQLite(), 
          dbFile
        )
      
      goalLeadersColDef <- 
        colDef(
          minWidth = 250,
          maxWidth = 250,
          class = "cell",
          cell = 
            function(value, index){
              Club <- activeKeeperData() %>% 
                filter(
                  Name == value
                ) %>% 
                .$Club
              
              if(Club %>% str_detect(",")){
                clubs <- str_split(Club, pattern = ",", simplify = TRUE) %>% c() %>% rev()
                
                list <- 
                  tagList(
                    lapply(
                      clubs,
                      function(X){
                        div(
                          style = "display: inline-block; width: 25px; float:right;", 
                          img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                        )
                      }
                    )
                  )
                
              } else {
                image <- img(src = sprintf("%s.png", Club), style = "height: 25px;", alt = Club)  
                
                list <- 
                  tagList(
                    div(style = "display: inline-block; width: 25px; float:right;", image)
                  )
              }
              
              tagList(
                list,
                div(style = "display: inline-block; width: 10px;"),
                div(style = "display: inline-block; width: 200px;", value)
              )
            }
        )
        
        
      outLeadersColDef <- 
        colDef(
          minWidth = 250,
          maxWidth = 250,
          class = "cell",
          cell = 
            function(value, index){
              Club <- activePlayerData() %>% 
                filter(
                  Name == value
                ) %>% 
                .$Club
              
              if(Club %>% str_detect(",")){
                clubs <- str_split(Club, pattern = ",", simplify = TRUE) %>% c() %>% rev()
                
                list <- 
                  tagList(
                    lapply(
                      clubs,
                      function(X){
                        div(
                          style = "display: inline-block; width: 25px; float:right;", 
                          img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                        )
                      }
                    )
                  )
                
              } else {
                image <- img(src = sprintf("%s.png", Club), style = "height: 25px;", alt = Club)  
                
                list <- 
                  tagList(
                    div(style = "display: inline-block; width: 25px; float:right;", image)
                  )
              }
              
             tagList(
                list,
                div(style = "display: inline-block; width: 10px;"),
                div(style = "display: inline-block; width: 200px;", value)
              )
            }
        )
      
      ## Bypassing that select does not allow a NULL option
      careerFilter <- 
        reactive({
          if(input$division == 'NULL'){
            NULL
          } else {
            input$division
          }
        })
      
      
      ##----------------------------------------------------------------
      ##                        Loading the data                       -
      ##----------------------------------------------------------------
      
      ## Creating the data from the chosen season
      
      activePlayerData <- reactive({
        if(input$season == "ALL"){
          res <-  
            GET(
              url = "https://api.simulationsoccer.com/ssl/getCareerStatistics",
              query = 
                list(
                  gameType = careerFilter())
            )
          
          fromJSON(res$content %>% rawToChar()) %>% 
            mutate(
              Season = 
                case_when(
                  str_detect(Season, ",") ~ 
                    str_split(
                      Season, 
                      pattern = ",", 
                      simplify = TRUE
                    ) %>% 
                    apply(
                      X = ., 
                      MARGIN = 1, 
                      FUN = 
                        function(x) {
                          x[c(1, max(min(which(x == "")-1, 10)))]
                        }, 
                      simplify = TRUE
                    ) %>% 
                    t() %>% 
                    apply(
                      X = ., 
                      MARGIN = 1, 
                      FUN = 
                        function(x) {
                          x %>% 
                            as.numeric() %>% 
                            paste0(collapse = " - ")
                          }
                      ),
                  TRUE ~ Season %>% as.integer() %>% as.character()
                )
            ) %>% 
            filter(
              !str_detect(Name, pattern = "BOT")
            )
        } else {
          data <- 
            playerGameData %>% 
            filter(
              Season == input$season
            )
          
          if((careerFilter() %>% is.null())){
            # Do nothing
          } else {
            data <- 
              data %>% 
              filter(
                Division == input$division
              )
          }
          
          # temp <- 
          data %>% 
            select(
              -(Result:Wor)
            ) %>% 
            group_by(
              Name
            ) %>% 
            dplyr::summarize(
              Nationality = last(Nationality),
              Club = last(Club),
              `Average Rating` = mean(`Average Rating`, na.rm = TRUE),
              across(
                where(is.numeric),
                ~ sum(.x, na.rm = TRUE)
              )
            ) %>% 
            dplyr::mutate(
              `Pass%` = (`Successful Passes` / `Attempted Passes`) %>% round(3)*100,
              `Cross%` = (`Successful Crosses` / `Attempted Crosses`) %>% round(3)*100,
              `Header%` = (`Successful Headers` / `Attempted Headers`) %>% round(3)*100,
              `Tackle%` = (`Tackles Won` / `Attempted Tackles`) %>% round(3)*100,
              `Average Rating` = `Average Rating` %>% round(2)
            )
        }
        
        
      })
      
      activeKeeperData <- reactive({
        
        if(input$season == "ALL"){
          res <-  
            GET(
              url = "https://api.simulationsoccer.com/ssl/getCareerStatistics",
              query = 
                list(
                  playerType = "keeper",
                  gameType = careerFilter())
            )
          
          fromJSON(res$content %>% rawToChar()) %>% 
            mutate(
              Season = 
                case_when(
                  str_detect(Season, ",") ~ 
                    str_split(
                      Season, 
                      pattern = ",", 
                      simplify = TRUE
                    ) %>% 
                    apply(
                      X = ., 
                      MARGIN = 1, 
                      FUN = 
                        function(x) {
                          x[c(1, max(min(which(x == "")-1, 10)))]
                        }, 
                      simplify = TRUE
                    ) %>% 
                    t() %>% 
                    apply(
                      X = ., 
                      MARGIN = 1, 
                      FUN = 
                        function(x) {
                          x %>% 
                            as.numeric() %>% 
                            paste0(collapse = " - ")
                        }
                    ),
                  TRUE ~ Season %>% as.integer() %>% as.character()
                )
           ) %>% 
            filter(
              !str_detect(Name, pattern = "BOT")
            )
        } else {
          
          data <- 
            keeperGameData %>% 
            filter(
              Season == input$season
            )
          
          if(careerFilter() %>% is.null()){
            # Do nothing
          } else {
            data <- 
              data %>% 
              filter(
                Division == input$division
              )
          }
          
          # temp <- 
          data %>% 
            select(
              -(Result:Division)
            ) %>% 
            group_by(
              Name
            ) %>% 
            dplyr::summarize(
              Nationality = last(Nationality),
              Club = last(Club),
              `Average Rating` = mean(`Average Rating`, na.rm = TRUE),
              `xSave%` = mean(`xSave%`, na.rm = TRUE) %>% round(2),
              across(
                where(is.numeric),
                ~ sum(.x, na.rm = TRUE)
              )
            ) %>% 
            dplyr::mutate(
              `Save%` = 
                ((`Saves Parried` + `Saves Held` + `Saves Tipped`) / 
                (`Saves Parried` + `Saves Held` + `Saves Tipped` + Conceded)) %>% round(3)*100,
              `Average Rating` = `Average Rating` %>% round(2)
            ) %>% 
            relocate(
              `xSave%`,
              .after = `Penalties Saved`
            )
        }
      })
      
      output$playerStats <- renderReactable({
        activePlayerData() %>% 
          arrange(
            `Average Rating` %>% desc()
          ) %>% 
          reactable(
            pagination = TRUE,
            defaultPageSize = 10,
            paginationType = "numbers",
            theme = pff(font_color = "#000"),
            searchable = TRUE,
            columns = 
              list(
                Name = colDef(
                  minWidth = 250,
                  style = list(position = "sticky", left = 0, background = "#F8F8F8", zIndex = 1),
                  headerStyle = list(position = "sticky", left = 0, zIndex = 1),
                  cell = 
                    function(value, index){
                      Nation <- activePlayerData() %>% 
                        arrange(
                          `Average Rating` %>% desc()
                        ) %>%  
                        .$Nationality %>% 
                        .[index]
                      
                      Club <- activePlayerData() %>% 
                        arrange(
                          `Average Rating` %>% desc()
                        ) %>%  
                        .$Club %>% 
                        .[index]
                      
                      if(Club %>% str_detect(",")){
                        clubs <- str_split(Club, pattern = ",", simplify = TRUE) %>% c() %>% rev()
                        
                        list <- 
                          tagList(
                            lapply(
                              clubs,
                              function(X){
                                div(
                                  style = "display: inline-block; width: 25px; float:right;", 
                                  img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                                )
                              }
                            )
                          )
                        
                      } else {
                        image <- img(src = sprintf("%s.png", Club), style = "height: 25px;", alt = Club)  
                        
                        list <- 
                          tagList(
                            div(style = "display: inline-block; width: 25px; float:right;", image)
                          )
                      }
                      
                      tagList(
                        div(style = "display: inline-block; width: 250px;", value),
                        list,
                        div(style = "font-size: 1rem", Nation)
                      )
                    }
                ),
                Club = 
                  colDef(
                    # maxWidth = 50,
                    # align = "center",
                    # class = "cell",
                    # cell = function(value){
                    #   image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)
                    #   tagList(
                    #     div(style = "display: inline-block; width: 25px;", image)
                    #   )
                    # }
                    show = FALSE,
                    searchable = TRUE
                  ),
                Nationality = colDef(show = FALSE)
              )
          )
      })
      
      output$goalieStats <- renderReactable({
        activeKeeperData() %>% 
          arrange(
            `Average Rating` %>% desc()
          ) %>% 
          reactable(
            pagination = TRUE,
            defaultPageSize = 10,
            paginationType = "numbers",
            theme = pff(font_color = "#000"),
            searchable = TRUE,
            columns = 
              list(
                Name = colDef(
                  minWidth = 250,
                  style = list(position = "sticky", left = 0, background = "#F8F8F8", zIndex = 1),
                  headerStyle = list(position = "sticky", left = 0, zIndex = 1),
                  cell = 
                    function(value, index){
                      Nation <- activeKeeperData() %>% 
                        arrange(
                          `Average Rating` %>% desc()
                        ) %>%  
                        .$Nationality %>% 
                        .[index]
                      
                      Club <- activeKeeperData() %>% 
                        arrange(
                          `Average Rating` %>% desc()
                        ) %>%  
                        .$Club %>% 
                        .[index]
                      
                      if(Club %>% str_detect(",")){
                        clubs <- str_split(Club, pattern = ",", simplify = TRUE) %>% c() %>% rev()
                        
                        list <- 
                          tagList(
                            lapply(
                              clubs,
                              function(X){
                                div(
                                  style = "display: inline-block; width: 25px; float:right;", 
                                  img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                                )
                              }
                            )
                          )
                        
                      } else {
                        image <- img(src = sprintf("%s.png", Club), style = "height: 25px;", alt = Club)  
                        
                        list <- 
                          tagList(
                            div(style = "display: inline-block; width: 25px; float:right;", image)
                          )
                      }
                      
                      tagList(
                        div(style = "display: inline-block; width: 250px;", value),
                        list,
                        div(style = "font-size: 1rem", Nation)
                      )
                    }
                ),
                Club = 
                  colDef(
                    # maxWidth = 50,
                    # align = "center",
                    # class = "cell",
                    # cell = function(value){
                    #   image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)
                    #   tagList(
                    #     div(style = "display: inline-block; width: 25px;", image)
                    #   )
                    # }
                    show = FALSE,
                    searchable = TRUE
                  ),
                Nationality = colDef(show = FALSE)
              )
          )
      })
      
      ### Leaders
      {
        output$leaderGoals <- renderReactable({
          activePlayerData() %>% 
            select(
              Name, 
              Goals
            ) %>% 
            arrange(
              desc(Goals)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = outLeadersColDef
                )
            )
        })
        
        output$leaderAssists <- renderReactable({
          activePlayerData() %>% 
            select(
              Name, 
              Assists
            ) %>% 
            arrange(
              desc(Assists)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = outLeadersColDef
                )
            )
        })
        
        output$leaderSoT <- renderReactable({
          activePlayerData() %>% 
            select(
              Name, 
              `Shots on Target`
            ) %>% 
            arrange(
              desc(`Shots on Target`)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = outLeadersColDef
                )
            )
        })
        
        output$leaderInterceptions <- renderReactable({
          activePlayerData() %>% 
            select(
              Name, 
              Interceptions
            ) %>% 
            arrange(
              desc(Interceptions)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = outLeadersColDef
                )
            )
        })
        
        output$leaderPoM <- renderReactable({
          activePlayerData() %>% 
            select(
              Name, 
              `Player of the Match`
            ) %>% 
            arrange(
              desc(`Player of the Match`)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = outLeadersColDef
                )
            )
        })
        
        output$leaderFouls <- renderReactable({
          activePlayerData() %>% 
            select(
              Name, 
              Fouls
            ) %>% 
            arrange(
              desc(Fouls)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = outLeadersColDef
                )
            )
        })
        
        output$leaderSavePerc <- renderReactable({
          activeKeeperData() %>% 
            select(
              Name, 
              `Save%`
            ) %>% 
            arrange(
              desc(`Save%`)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = goalLeadersColDef
                )
            )
        })
        
        output$leaderWins <- renderReactable({
          activeKeeperData() %>% 
            select(
              Name, 
              Won
            ) %>% 
            arrange(
              desc(Won)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = goalLeadersColDef
                )
            )
        })
        
        output$leaderCleanSheets <- renderReactable({
          activeKeeperData() %>% 
            select(
              Name, 
              `Clean Sheets`
            ) %>% 
            arrange(
              desc(`Clean Sheets`)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = goalLeadersColDef
                )
            )
        })
        
        output$leaderConceded <- renderReactable({
          activeKeeperData() %>% 
            select(
              Name, 
              Conceded
            ) %>% 
            arrange(
              desc(Conceded)
            ) %>% 
            slice_head(n = 10) %>% 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
              theme = pff(font_color = "#000"),
              columns = 
                list(
                  Name = goalLeadersColDef
                )
            )
        })
      }
      
      ### Download data button
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("SSL Player Statistics.zip", sep = "")
        },
        content = function(file) {
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)
          
          
          write.csv(activePlayerData(), file.path(temp_directory, "players.csv"), row.names = FALSE, fileEncoding = "UTF-8")
          write.csv(activeKeeperData(), file.path(temp_directory, "keepers.csv"), row.names = FALSE, fileEncoding = "UTF-8")
          
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
          
        },
        contentType = "application/zip"
      )
      
    }
  )
}

