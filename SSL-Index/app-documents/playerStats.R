
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
          selectInput(
            inputId = ns("division"),
            label = "Select division",
            choices = 
              c(
                "ALL",
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
        
        if(input$division == "ALL"){
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
      })
      
      activeKeeperData <- reactive({
        data <- 
          keeperGameData %>% 
          filter(
            Season == input$season
          )
        
        if(input$division == "ALL"){
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
      
    }
  )
}

