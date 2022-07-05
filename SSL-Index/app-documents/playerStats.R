
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
              1:max(playerGameData$Season) %>% 
              sort(decreasing = TRUE)
          ),
          selectizeInput(
            inputId = ns("gameFilter"),
            label = "Filter type of Game",
            choices =
              c(
                "Cup",
                "League"
              ),
            multiple = TRUE
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
              h4("Outfield", align = "center"),
              reactableOutput(
                outputId = ns("playerStats")
              ),
              h4("Goalkeeper", align = "center"),
              reactableOutput(
                outputId = ns("goalieStats")
              )
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
            ),
            
            ##----------------------------------------------------------------
            ##                        League Records                         -
            ##----------------------------------------------------------------
            tags$head(
              tags$style(
                HTML(
                  '.info-box {min-height: 65px;} 
                  .info-box-icon {background: transparent; height: 65px; line-height: 65px;} 
                  .info-box-content {padding-top: 0px; padding-bottom: 0px;}'
                )
              ),
              ## Imports all 5.7.2 Font Awesome Icons
              tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);")
            ),
            
            tabPanel(
              title = "League Records",
              column(
                width = 4,
                h3("Record for"),
                actionLink(
                  ns("topGoal"),
                  uiOutput(
                    outputId = ns("topGoal")
                  )
                ),
                actionLink(
                  ns("topAssist"),
                  uiOutput(
                    outputId = ns("topAssist")
                  )  
                ),
                actionLink(
                  ns("topxG"),
                  uiOutput(
                    outputId = ns("topxG")
                  )  
                ),
                actionLink(
                  ns("topKeyPasses"),
                  uiOutput(
                    outputId = ns("topKeyPasses")
                  )  
                ),
                actionLink(
                  ns("topChancesCreated"),
                  uiOutput(
                    outputId = ns("topChancesCreated")
                  )  
                ),
                actionLink(
                  ns("topPotM"),
                  uiOutput(
                    outputId = ns("topPotM")
                  )  
                ),
                actionLink(
                  ns("topDistanceRun"),
                  uiOutput(
                    outputId = ns("topDistanceRun")
                  )  
                ),
                actionLink(
                  ns("topYellows"),
                  uiOutput(
                    outputId = ns("topYellows")
                  )  
                ),
                actionLink(
                  ns("topReds"),
                  uiOutput(
                    outputId = ns("topReds")
                  )  
                ),
                actionLink(
                  ns("topInterceptions"),
                  uiOutput(
                    outputId = ns("topInterceptions")
                  )  
                )
              ),
              column(
                width = 8,
                reactableOutput(
                  outputId = ns("leagueRecord")
                )
              )
            ),
            tabPanel(
              title = "Cup Records",
              column(
                width = 4,
                h1("Cup Records"),
                uiOutput(
                  outputId = ns("topGoalC")
                ),
                uiOutput(
                  outputId = ns("topAssistC")
                ),
                uiOutput(
                  outputId = ns("topPassC")
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
      
      ##----------------------------------------------------------------
      ##                        Loading the data                       -
      ##----------------------------------------------------------------
      
      ## Creating the data from the chosen season
      
      activePlayerData <- reactive({
        data <- 
          playerGameData %>% 
          filter(
            Season == input$season
          )
        
        if(!is.null(input$gameFilter)){
          if(input$gameFilter %>% length() == 2){
            # Do nothing
          } else if("Cup" %in% input$gameFilter){
            data <- 
              data %>% 
              filter(
                str_detect(Matchday, "Cup")
              )
          } else {
            data <- 
              data %>% 
              filter(
                str_detect(Matchday, "Cup", negate = TRUE)
              )
          }
        }
        
        # temp <- 
        data %>% 
          select(
            -Season
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
      })
      
      activeKeeperData <- reactive({
        data <- 
          keeperGameData %>% 
          filter(
            Season == input$season
          )
        
        if(!is.null(input$gameFilter)){
          if(input$gameFilter %>% length() == 2){
            # Do nothing
          } else if("Cup" %in% input$gameFilter){
            data <- 
              data %>% 
              filter(
                str_detect(Matchday, "Cup")
              )
          } else {
            data <- 
              data %>% 
              filter(
                str_detect(Matchday, "Cup", negate = TRUE)
              )
          }
        }
        
        # temp <- 
        data %>% 
          select(
            -Season
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
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            searchable = TRUE,
            columns = 
              list(
                Name = colDef(
                  minWidth = 250,
                  style = list(position = "sticky", left = 0, background = "#F8F8F8", zIndex = 1),
                  headerStyle = list(position = "sticky", left = 0, background = "#F8F8F8", zIndex = 1)
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
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
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            searchable = TRUE,
            columns = 
              list(
                Name = colDef(
                  minWidth = 250,
                  style = list(position = "sticky", left = 0, background = "#F8F8F8",zIndex = 1),
                  headerStyle = list(position = "sticky", left = 0, background = "#F8F8F8", zIndex = 1)
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderGoals <- renderReactable({
        activePlayerData() %>% 
          select(
            Name, 
            Club,
            Goals
          ) %>% 
          arrange(
            desc(Goals)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderAssists <- renderReactable({
        activePlayerData() %>% 
          select(
            Name, 
            Club,
            Assists
          ) %>% 
          arrange(
            desc(Assists)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderSoT <- renderReactable({
        activePlayerData() %>% 
          select(
            Name, 
            Club,
            `Shots on Target`
          ) %>% 
          arrange(
            desc(`Shots on Target`)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderInterceptions <- renderReactable({
        activePlayerData() %>% 
          select(
            Name, 
            Club,
            Interceptions
          ) %>% 
          arrange(
            desc(Interceptions)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderPoM <- renderReactable({
        activePlayerData() %>% 
          select(
            Name, 
            Club,
            `Player of the Match`
          ) %>% 
          arrange(
            desc(`Player of the Match`)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderFouls <- renderReactable({
        activePlayerData() %>% 
          select(
            Name, 
            Club,
            Fouls
          ) %>% 
          arrange(
            desc(Fouls)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderSavePerc <- renderReactable({
        activeKeeperData() %>% 
          select(
            Name, 
            Club,
            `Save%`
          ) %>% 
          arrange(
            desc(`Save%`)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderWins <- renderReactable({
        activeKeeperData() %>% 
          select(
            Name, 
            Club,
            Won
          ) %>% 
          arrange(
            desc(Won)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderCleanSheets <- renderReactable({
        activeKeeperData() %>% 
          select(
            Name, 
            Club,
            `Clean Sheets`
          ) %>% 
          arrange(
            desc(`Clean Sheets`)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      output$leaderConceded <- renderReactable({
        activeKeeperData() %>% 
          select(
            Name, 
            Club,
            Conceded
          ) %>% 
          arrange(
            desc(Conceded)
          ) %>% 
          slice_head(n = 10) %>% 
          reactable(
            pagination = FALSE,
            sortable = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            columns = 
              list(
                Name = colDef(
                  minWidth = 200,
                  maxWidth = 200
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value, index){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  )
              )
          )
      })
      
      
      ##----------------------------------------------------------------
      ##                        Career Records                         -
      ##----------------------------------------------------------------
      
      output$topGoal <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = Goals,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Goals")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "goals.",
                sep = " "
                ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topAssist <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = Assists,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Assists")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-shoe-prints", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "assists.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topxG <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = xG,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Expected Goals")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "expected goals.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topKeyPasses <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Key Passes`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Key Passes")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-key", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "passes.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topChancesCreated <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Chances Created`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Chances Created")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-eye", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "chances.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topInterceptions <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "Interceptions",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Interceptions")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-ban", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "interceptions.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topPotM <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Player of the Match`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Player of the Match")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-award", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "awards.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topYellows <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Yellow Cards`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Yellow Cards")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-square", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "yellow.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topReds <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Red Cards`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Red Cards")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-exclamation", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "red.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topDistanceRun <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Distance Run (km)`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Distance Run (km)")
            ),
          color = "navy",
          width = NULL,
          icon = icon("person-running", style="font-size: 36px; color: white", verify_fa = FALSE),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "km.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      #Penalties Scored, Key Headers, Key Tackles
      
      output$topGoalC <- renderUI({
        
        leader <- 
          tbl(con, "Player_Game_Data") %>% 
          filter(
            (Matchday %like% "%Cup%")
          ) %>% 
          group_by(Name) %>% 
          summarize(
            Stat = sum(Goals)
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Goals")
            ),
          color = "orange",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "Goals.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        )
        
      })
      
      output$topAssistC <- renderUI({
        
        leader <- 
          tbl(con, "Player_Game_Data") %>% 
          filter(
            (Matchday %like% "%Cup%")
          ) %>%
          group_by(Name) %>% 
          summarize(
            Stat = sum(Assists)
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Assists")
            ),
          color = "orange",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "Assists.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        )
        
      })
      
      output$topPassC <- renderUI({
        
        leader <- 
          tbl(con, "Player_Game_Data") %>% 
          filter(
            (Matchday %like% "%Cup%")
          ) %>%
          group_by(Name) %>% 
          summarize(
            Stat = sum(`Successful Passes`)
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Successful Passes")
            ),
          color = "orange",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "Passes.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        )
        
      })
      
      
      ##---------------------------------------------------------------
      ##                    Selected stat observers                   -
      ##---------------------------------------------------------------
      
      currentStat <- 
        reactiveValues(
          league = "Goals",
          cup = "Goals"
          )
      
      observeEvent(
        input$topGoal, 
        {
          currentStat$league <- "Goals"
        }
      )
      
      observeEvent(
        input$topPotM, 
        {
          currentStat$league <- "`Player of the Match`"
        }
      )
      
      observeEvent(
        input$topInterceptions, 
        {
          currentStat$league <- "Interceptions"
        }
      )
      
      observeEvent(
        input$topChancesCreated, 
        {
          currentStat$league <- "`Chances Created`"
        }
      )
      
      observeEvent(
        input$topDistanceRun, 
        {
          currentStat$league <- "`Distance Run (km)`"
        }
      )
      
      observeEvent(
        input$topYellows, 
        {
          currentStat$league <- "`Yellow Cards`"
        }
      )
      
      observeEvent(
        input$topReds, 
        {
          currentStat$league <- "`Red Cards`"
        }
      )
      
      observeEvent(
        input$topAssist, 
        {
          currentStat$league <- "Assists"
        }
      )
      
      observeEvent(
        input$topxG, 
        {
          currentStat$league <- "xG"
        }
      )
      
      observeEvent(
        input$topKeyPasses, 
        {
          currentStat$league <- "`Key Passes`"
        }
      )
      
      
      leagueRecords <- reactive({
        tbl(con, "Player_Game_Data") %>% 
          filter(
            !(Matchday %like% "%Cup%")
          ) %>% 
          group_by(Name) %>% 
          summarize(
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
              sum
            )
          ) %>% 
          collect()
      })
      
      output$leagueRecord <- renderReactable({
          leagueRecords() %>% 
          select(
            Name, 
            Apps,
            currentStat$league
          ) %>% 
          arrange(
            across(
              currentStat$league,
              desc
            )
          ) %>% 
          slice_head(n = 20) %>% 
          mutate(
            Rank = 1:n()
          ) %>% 
          relocate(
            Rank,
            .before = Name
          ) %>% 
          reactable(
            pagination = FALSE,
            defaultColDef = 
              colDef(
                maxWidth = 150
              ),
            columns = 
              list(
                Rank = 
                  colDef(
                    width = 60
                  ),
                Name = 
                  colDef(
                    maxWidth = 350
                  )
              )
          )
      })
      
     
    }
  )
}

