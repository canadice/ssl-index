
############################################################################
############################################################################
###                                                                      ###
###                  VISUALIZES DATA FROM A SINGLE GAME                  ###
###                                                                      ###
############################################################################
############################################################################

### UI module
gameDataUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        "You need to select the filters from left to right, starting with a season."
      ),
      #################################################################
      ##                           Filters                           ##
      #################################################################
      fluidRow(
        column(
          width = 3,
          uiOutput(outputId = ns("selectSeason"), inline = TRUE)
        ),
        column(
          width = 3,
          uiOutput(outputId = ns("selectDivision"), inline = TRUE)
        ),
        column(
          width = 3,
          uiOutput(outputId = ns("selectMatchday"), inline = TRUE)
        ),
        column(
          width = 3,
          uiOutput(outputId = ns("selectClub"), inline = TRUE)
        )
      ),
      
      #################################################################
      ##                       Team Comparison                       ##
      #################################################################
      column(
        width = 4,
        plotlyOutput(
          outputId = ns("teamComparison")
        )
      ),
      column(
        width = 8,
        fluidRow(
          column(
            width = 4,
            uiOutput(
              ns("statSelect")
            ),
            uiOutput(
              ns("playerSelect")
            )
          ),
          column(
            width = 8,
            plotlyOutput(
              ns("statRankVis")
            )
          )
        ),
        br(),
        fluidRow(
          reactableOutput(
            ns("playerGameStat")
          )
        )
      )
      
    )
  )
}

## Backend module 
gameDataSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ### Testing
      # input <- NA
      # input$season = 1
      # input$division = 1
      # input$matchday = "1"
      # input$club = "Athênai F.C."
      # data <-
      #   readAPI(
      #    url = "http://143.198.159.1/ssl/gameStats",
      #    query =
      #        list(
      #            season = input$season,
      #            division = input$division,
      #            matchday = input$matchday,
      #            team = input$club
      #          )
      #   )
      
      
      #################################################################
      ##              Various UIs used to select a game              ##
      #################################################################
      
      output$selectSeason <- renderUI({
        selectizeInput(
          inputId = session$ns("season"),
          label = "Select a season",
          choices =
            c(
              readAPI(
                url = "http://143.198.159.1/ssl/getSeasons"
              )$Season
            ),
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      })
      
      output$selectDivision <- renderUI({
        # if(!isTruthy(input$season)){
        #   #Do nothing
        # } else {
          selectizeInput(
            inputId = session$ns("division"),
            label = "Select a Division <i>(0 is Cup)</i>" %>% HTML(),
            choices =
              c(
                readAPI(
                  url = "http://143.198.159.1/ssl/getDivisions",
                  query = list(season = input$season)
                )$Division
              ),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        # }
      })
      
      output$selectMatchday <- renderUI({
        # if(!isTruthy(input$season)){
        #   #Do nothing
        # } else {
          selectizeInput(
            inputId = session$ns("matchday"),
            label = "Select a Matchday",
            choices =
              c(
                readAPI(
                  url = "http://143.198.159.1/ssl/getMatchdays",
                  query = list(season = input$season, division = input$division)
                )$Matchday
              ),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        # }
      })
      
      output$selectClub <- renderUI({
        # if(!isTruthy(input$season)){
        #   #Do nothing
        # } else {
          selectizeInput(
            inputId = session$ns("club"),
            label = "Select a Club from the Matchday",
            choices =
              c(
                readAPI(
                  url = "http://143.198.159.1/ssl/getTeams",
                  query = list(season = input$season, division = input$division, matchday = input$matchday)
                )$Club
              ),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        # }
      })
      
      
      #################################################################
      ##                    Imports the game data                    ##
      #################################################################
      
      gameData <- reactive({
        if(!all(isTruthy(input$season), isTruthy(input$division),isTruthy(input$matchday),isTruthy(input$club))){
          # DO NOTHING
        } else {
          readAPI(
            url = "http://143.198.159.1/ssl/gameStats",
            query = 
              list(
                season = input$season,
                division = input$division,
                matchday = input$matchday,
                team = input$club
              )
          )
        }
      })
      
      
      ##################################################################
      ##                  Visualizes team aggregates                  ##
      ##################################################################
      
      output$teamComparison <- renderPlotly({
        if(gameData() %>% is.null()){
          #DO NOTHING
        } else {
          data <- gameData()$team 
          
          visData <- 
            data %>%
            ### testing
            # data$team %>%
            select(
              Club,
              `Average Rating`,
              Goals,
              Assists,
              xG,
              `Shots on Target`,
              Shots,
              `Saves Parried`, 
              `Saves Held`,
              `Saves Tipped`,
              `Distance Run (km)`,
              `Pass%`,
              `Key Passes`,
              `Cross%`,
              `Chances Created`,
              `Header%`,
              `Key Headers`,
              `Tackle%`,
              `Key Tackles`,
              Interceptions,
              Clearances,
              Fouls,
              Offsides,
              `Yellow Cards`,
              `Red Cards`,
            ) %>% 
            mutate(
              Saves = `Saves Parried` + `Saves Held` + `Saves Tipped`
            ) %>% 
            relocate(
              Saves, 
              .after = `Saves Tipped`
            ) %>% 
            select(
              -contains("Saves ")
            ) %>% 
            pivot_longer(
              cols = -Club
            ) %>% 
            mutate(
              name = factor(name, levels = unique(name) %>% rev())
            ) %>% 
            group_by(name) %>% 
            mutate(
              value_comp = value/sum(value)
            ) %>% 
            mutate(
              value_comp = if_else(is.na(value_comp), 0.5, value_comp)
            )
          
          teams <- unique(visData$Club)
          
          plot_ly(
            data = visData,
            height = 600,
            x = ~value_comp,
            y = ~name,
            color = ~Club,
            customdata = 
              ~paste(
                Club, "<br><br>",
                # name, "<br>",
                value
              ),
            colors = 
              c(teamInfo$color_primary[teamInfo$team %in% teams[1]],teamInfo$color_secondary[teamInfo$team %in% teams[2]]),
            hovertemplate = paste(
              "<b>%{customdata}</b>",
              "<extra></extra>"
            )
          ) %>% 
            add_bars(
              orientation = "h",
              hoverinfo = "color",
              marker = 
                list(
                  line = 
                    list(
                      color = I("black"),
                      width = 2
                    )  
                )
            ) %>% 
            add_text(
              x = 0.49,
              color = I("grey"),
              text = ~name,
              hoverinfo = "none",
              textposition = "left"
            ) %>% 
            add_annotations(
              xref = "paper",
              yref = "paper",
              x = 0.475,
              y = 1.01,
              font = list(size = 18, color = I("black")),
              text =
                visData %>%
                ungroup() %>%
                filter(name == "Goals") %>%
                select(value) %>%
                unlist() %>%
                paste(collapse = " - "),
              showarrow = FALSE
            ) %>%
            layout(
              barmode = "stack",
              shapes = 
                list(
                  type = "line",
                  x0 = 0.475,
                  x1 = 0.475, 
                  xref = "paper",
                  y0 = -0.5, 
                  y1 = 21.5,
                  line = list(color = "white")
                ),
              yaxis = 
                list(
                  title = "",
                  # showticklabels = FALSE,
                  fixedrange = TRUE,
                  visible = FALSE,
                  zeroline = FALSE,
                  range = c(-1, 23),
                  showgrid = FALSE
                ),
              xaxis = 
                list(
                  title = "",
                  showticklabels = FALSE,
                  showtick = FALSE,
                  fixedrange = TRUE,
                  showgrid = FALSE,
                  zeroline = FALSE
                ),
              showlegend = FALSE,
              images = 
                list(
                  list(
                    source = 
                      paste(teams[1], ".png", sep = ""),
                    xref = "paper",
                    yref = "paper",
                    x= 0.20,
                    y= 1.01,
                    sizex = 0.15,
                    sizey = 0.15,
                    opacity = 1
                  ),
                  list(
                    source = 
                      paste(teams[2], ".png", sep = ""),
                    xref = "paper",
                    yref = "paper",
                    x= 0.60,
                    y= 1.01,
                    sizex = 0.15,
                    sizey = 0.15,
                    opacity = 1
                  )
                ),
              plot_bgcolor  = "rgba(0, 0, 0, 0)",
              paper_bgcolor = "rgba(0, 0, 0, 0)"
            ) %>% 
            config(
              displayModeBar = FALSE
            )
          
        }
        
      })
      
      #################################################################
      ##                    UI for player or stat                    ##
      #################################################################
      
      output$statSelect <- renderUI({
        if(gameData() %>% is.null()){
          #DO NOTHING
        } else {
          selectizeInput(
            inputId = session$ns("statistic"),
            label = "Select a statistic",
            choices =
              gameData()$player %>% 
              full_join(
                gameData()$keeper %>% 
                  select(
                    -(Nationality:`Player of the Match`),
                    -(Result:Division)
                  ),
                by = "Name"
              ) %>%
              select(
                -(Name:`Minutes Played`),
                -(Result:Wor)
              ) %>% 
              colnames(),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
      })
      
      output$playerSelect <- renderUI({
        if(gameData() %>% is.null()){
          #DO NOTHING
        } else {
          selectizeInput(
            inputId = session$ns("player"),
            label = "Select a player",
            choices =
              split(gameData()$player$Name, gameData()$player$Club),
            options = list(
              placeholder = 'Please select an option below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
      })
      
      ##################################################################
      ##                Visualizes Player Performances                ##
      ##################################################################
      
      output$statRankVis <- renderPlotly({
        if(gameData() %>% is.null() | !isTruthy(input$statistic)){
          #DO NOTHING
        } else {
          statistic <- input$statistic
          
          
          ### Testing
          # visData <-
          #   data$player %>%
          #   full_join(
          #     data$keeper %>%
          #       select(
          #        -(Nationality:`Player of the Match`),
          #        -(Result:Division)
          #       ),
          #       by = "Name"
          #   ) %>%
          #   select(
          #     Name,
          #     Club,
          #     all_of(statistic)
          #   ) %>%
          #   arrange(
          #     get(statistic) %>% desc()
          #   ) %>%
          #   head(n = 5) %>%
          #   mutate(
          #     Name = factor(Name, levels = Name %>% rev())
          #   )
          
          visData <- 
            gameData()$player %>% 
            full_join(
              gameData()$keeper %>% 
                select(
                  -(Nationality:`Player of the Match`),
                  -(Result:Division)
                ),
              by = "Name"
            ) %>% 
            select(
              Name,
              Club,
              all_of(statistic)
            ) %>% 
            arrange(
              get(statistic) %>% desc()
            ) %>% 
            head(n = 5) %>% 
            mutate(
              Name = factor(Name, levels = Name %>% rev())
            )
          
          teamsIndex <- sapply(X = unique(visData$Club), FUN = function(x) {which(teamInfo$team == x)})
          
          plot_ly(
            data = visData,
            y = ~Name,
            x = ~get(statistic),
            color = ~Club,
            customdata = ~Club,
            colors = rev(teamInfo$color_primary[teamsIndex]),
            hovertemplate = paste(
              "<b>%{customdata}</b><br><br>",
              "Player: %{y}<br>",
              "%{xaxis.title.text}: %{x:}",
              "<extra></extra>"
            )
          ) %>%
            add_bars(
              orientation = "h",
              hoverinfo = "color",
              marker = 
                list(
                  line = 
                    list(
                      color = rep(I("black"), 5),
                      width = 2
                    )  
                )
            ) %>% 
            # add_text(
            #   color = I("black"),
            #   text = ~Name,
            #   textposition = "right",
            #   hoverinfo = "none"
            # ) %>%
            layout(
              title = 
                list(
                  text='Top 5 players', 
                  x = 0.5, 
                  xanchor = 'center', 
                  yanchor =  'top'
                ),
              yaxis = 
                list(
                  title = "",
                  showticklabels = FALSE,
                  fixedrange = TRUE
                ),
              xaxis = 
                list(
                  title = statistic,
                  fixedrange = TRUE,
                  ## Dynamically sets the number of breaks depending on the max value of the statistic
                  dtick = 
                    ((visData[,statistic] %>% max()) / 10) %>% 
                    if_else(
                      condition = . < 0.15, 
                      true = 0.25, 
                      false = ceiling(.)
                      ),
                  # Sets the range so that if the only values are 0, it still goes from 0 to 1.
                  range = 
                    c(
                      0, 
                      if_else(
                        (visData[,statistic] %>% max()) == 0, 
                        1, 
                        (visData[,statistic] %>% max()) + ((visData[,statistic] %>% max())/10)
                      )
                    )
                ),
              showlegend = FALSE,
              plot_bgcolor  = "rgba(0, 0, 0, 0)",
              paper_bgcolor = "rgba(0, 0, 0, 0)"
            ) %>% 
            config(
              displayModeBar = FALSE
            )
          
         }
      })
      
      ##################################################################
      ##                    Shows player game data                    ##
      ##################################################################
      
      output$playerGameStat <- renderReactable({
        if(gameData() %>% is.null() | !isTruthy(input$player)){
          #DO NOTHING
        } else {
          data <- 
            gameData()$player %>% 
            full_join(
              gameData()$keeper %>% 
                select(
                  -(Nationality:`Player of the Match`),
                  -(Result:Division)
                ),
              by = "Name"
            ) %>% 
            filter(
              Name == input$player
            )
          
          if(data$Position == "GK"){
            
          } else {
            data %>% 
              select(
                -(Club:Apps),
                -(Result:Wor),
                -(Won:`xSave%`)
              ) %>% 
              pivot_longer(
                cols = -(Name:Nationality)
              ) %>% 
              select(
                Statistic = name,
                Value = value
              ) %>% 
              reactable()
          }
        }
      })
    }
  )
}

