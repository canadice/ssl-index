
###########################################################################
###########################################################################
###                                                                     ###
###                         PLAYER BUILDER TOOL                         ###
###                                                                     ###
###########################################################################
###########################################################################

### UI module for player similarities using MDS
playerDatabaseUI <- function(id){
  
  ## Creating the namespacing function for all iDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("player"),
            label = "Select a player",
            choices = 
             playerData %>% 
                # select(Name) %>% 
                # arrange(Name)
                select(Team, Name) %>%
                group_by(Team) %>%
                arrange(Name) %>%
                mutate(
                  Name = if_else(Name == "Kuai Liang", "Liang Kuai", Name)
                ) %>% 
                group_split(.keep = FALSE) %>%
                setNames(playerData$Team %>% unique() %>% sort()) %>%
                lapply(
                  X = .,
                  FUN = function(x){
                    c(x) %>%
                      unname() %>%
                      lapply(
                        X = .,
                        FUN = function(x){
                          as.list(x)
                        }
                      ) %>%
                      unlist(recursive = FALSE)
                  }
                )
          )
        ),
        column(
          width = 8,
          plotlyOutput(
            outputId = ns("radarPlotly"),
            height = "250px"
          )
        )
      ),
      fluidRow(
        tabBox(
          width = NULL,
          
          ##----------------------------------------------------------------
          ##                        Game Log Stats                         -
          ##----------------------------------------------------------------
          
          tabPanel(
            "Game Log",
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  inputId = ns("gameSeason"),
                  label = "Filter Season",
                  choices =
                    playerGameData$Season %>%
                    unique() %>%
                    sort(),
                  multiple = TRUE
                )
              ),
              column(
                width = 4,
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
                reactableOutput(
                  outputId = ns("GameData")
                ) %>% withSpinner()
              )
            )
          ),
          
          ##----------------------------------------------------------------
          ##                      Career Summary Stats                     -
          ##----------------------------------------------------------------
          
          tabPanel(
            "Career Stats",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  #same as gameFilter, but for career tab
                  inputId = ns("careerFilter"),
                  label = "Filter type of Game",
                  choices =
                    c(
                      "ALL" = 'NULL',
                      "Cup" = 0,
                      "Division 1" = 1,
                      "Division 2" = 2
                    ),
                  multiple = FALSE
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                reactableOutput(
                  outputId = ns("CareerData")
                ) %>% withSpinner()
              )
            )
          ),
          
          ##---------------------------------------------------------------
          ##                      Development History                     -
          ##---------------------------------------------------------------
          
          tabPanel(
            "Development History",
            fluidRow(
              column(
                width = 10,
                offset = 1,
                p("The following plots show the development of the player over their career. 
                  The subplots show the different groups of attributes, and the color codes are
                  unique within each subplot. You can click on the attribute in the legend to hide 
                  the line, or double-click to isolate the line in the plot."),
                plotlyOutput(
                  outputId = ns("Development"),
                  height = "800px"
                )
              )
            )
          ),
          ##---------------------------------------------------------------
          ##                      Player Updater Tool                     -
          ##---------------------------------------------------------------
          
          tabPanel(
            "Player Updater",
            
            fluidRow(
              p("This has now moved to the Forum and can be accessed via ", htmltools::HTML('<a href="https://simsoccer.jcink.net/index.php?act=Pages&pid=4">this link!</a>'))
            )
          )
          
          ##---------------------------------------------------------------
          ##                          End of tabs                         -
          ##---------------------------------------------------------------
          
        )
      )
    )
 )
}

## Backend module for player similarities
playerDatabaseSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ## Bypassing that select does not allow a NULL option
      careerFilter <- 
        reactive({
          if(input$careerFilter == 'NULL'){
            NULL
          } else {
            input$careerFilter 
          }
        })
      
      ##----------------------------------------------------------------
      ##                Loading data from the API + db                 -
      ##----------------------------------------------------------------
      
      careerSeasonData <- 
        reactive({
          res <-  
            GET(
              url = "https://api.simulationsoccer.com/ssl/getPlayerStatistics",
              query = 
                list(
                  player = input$player, 
                  gameType = careerFilter(), 
                  seasonTotal = TRUE)
            )
          
          fromJSON(res$content %>% rawToChar())
        })
      
      careerData <- 
        reactive({
          res <-  
            GET(
              url = "https://api.simulationsoccer.com/ssl/getPlayerStatistics",
              query = 
                list(
                  player = input$player, 
                  gameType = careerFilter(), 
                  careerTotal = TRUE)
            )
          
          fromJSON(res$content %>% rawToChar())
        })
      
      ##---------------------------------------------------------------
      ##                        Reactive values                       -
      ##---------------------------------------------------------------
      
      radarAttributes <- reactive(
        playerData %>% 
          filter(Name == input$player) %>% 
          select(
            Name,
            DEFENDING:MENTAL
          )
      )
      
      ## Reactive data set that updates the input build of a player
      reactives <- reactiveValues(
        currentBuild = {
          playerData %>% 
            filter(Name == input$player) %>% 
            select(
              Acceleration:Throwing
            ) %>% 
            pivot_longer(
              cols = everything(),
              names_to = "Attribute",
              values_to = "Value"
            ) %>% 
            left_join(
              tpeCost,
              by = c("Value" = "value")
            ) %>% 
            mutate(
              cost = if_else(Attribute %in% c("Natural Fitness", "Stamina"), 0, cost)
            ) %>% 
            left_join(
              attributes,
              by = "Attribute"
            ) %>% 
            filter(
              !is.na(Value)
            ) %>% 
            select(
              -Keeper,
              -abbr
            )
        })      
      # Using reactable for Game Log
      ##---------------------------------------------------------------
      ##                        Game log output                       -
      ##---------------------------------------------------------------

      output$GameData <- renderReactable({
        res <-  
          GET(
            url = "https://api.simulationsoccer.com/ssl/getPlayerStatistics",
            query = 
              list(
                player = input$player
              )
          )
        
        data <- fromJSON(res$content %>% rawToChar()) %>% 
          select(
            -(Acc:Wor)
          )
        
        if(!is.null(input$gameSeason)){
          data <- 
            data %>% 
            filter(
              Season %in% input$gameSeason
            )
        }
        
        if(!is.null(input$gameFilter)){
          if(input$gameFilter %>% length() == 2){
            # Do nothing
          } else if("Cup" %in% input$gameFilter){
            data <- 
              data %>% 
              filter(
                Division == 0
              )
          } else {
            data <- 
              data %>% 
              filter(
                Division != 0
              )
          }
        }
        
        data %>% 
          select(
            -Name,
            -Nationality,
            -Position,
            -Apps
          ) %>%
          relocate(
            c(
              Season,
              Division,
              Matchday,
              Club,
              Opponent,
              Result
            ),
            .before = `Minutes Played` 
          ) %>% 
          mutate(
            Division = as.character(Division)
          ) %>% 
          mutate(
            Division = if_else(Division == "0", "Cup", Division)
          ) %>% 
          reactable(
            pagination = FALSE,
            theme = pff(font_color = "#000"),
            rowStyle = function(index) {
              if (data$Season[index] %% 2 == 0) list(background = "rgba(0, 0, 0, 0.05)")
              else list(background = "rgba(0, 0, 0, 0.0)")
            },
            columns = 
              list(
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value){
                      image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                      
                      div(style = "display: inline-block; width: 25px;", image)
                    }
                  ),
                Opponent = 
                  colDef(
                    name = "Opp.",
                    maxWidth = 50,
                    class = "cell",
                    align = "center",
                    cell = function(value){
                      
                      if(value %>% is.na()){
                        div(class = "club", " ")
                        
                      } else {
                        image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                        
                        div(style = "display: inline-block; width: 25px;", image)
                      }
                    }
                  ),
                Result = 
                  colDef(
                    maxWidth = 60,
                    cell = function(value) {
                      if(is.na(value)){
                        div(
                          style = list(background = "#FFFFFF"),
                          " "
                        )
                      } else {
                        values <- str_split(value, pattern = "-", simplify = TRUE) 
                        
                        if(any(str_detect(values, pattern = "a|p|e"))){
                          
                          if(which(str_detect(values, pattern = "a|p|e")) == 1){
                            color <- "#A4D1A2"
                          } else {
                            color <- "#CB8491"
                          }
                          
                        } else {
                          values <- values %>% as.numeric()
                          
                          if(values[1] > values[2]){
                            color <- "#A4D1A2"
                          } else if(values[1] < values[2]){
                            color <- "#CB8491"
                          } else {
                            color <- "#FFFFBF"
                          }
                        }
                        
                        div(
                          align = "center",
                          style = list(background = color),
                          value
                        )
                      }
                    }
                  )
              )
          ) 
        
      })
      
      
      # Using CareerData 
      output$CareerData <- renderReactable({
        if(!is.null(input$careerFilter)){
          data <- 
            careerSeasonData()
          
          dataSum <- 
            careerData()
          
          if(any(reactives$currentBuild$Group == "Goalkeeper")){
            data %>% 
              reactable(
                pagination = FALSE,
                theme = pff(font_color = "#000"),
                columns = 
                  list(
                    Season = colDef(
                      footer = "Totals",
                      maxWidth = 80,
                      style = list(position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1),
                      headerStyle = list(position = "sticky", left = 0, zIndex = 1),
                      footerStyle = list(fontWeight = "bold", position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1)
                    ),
                    Club = 
                      colDef(
                        maxWidth = 60,
                        align = "center",
                        class = "cell",
                        cell = function(value){
                          if(value %>% str_detect(",")){
                            clubs <- str_split(value, pattern = ",", simplify = TRUE) %>% c()
                            
                            list <- 
                              tagList(
                                lapply(
                                  clubs,
                                  function(X){
                                    div(
                                      style = "display: inline-block; width: 25px;", 
                                      img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                                    )
                                  }
                                )
                              )
                            
                          } else {
                            image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                            
                            list <- 
                              tagList(
                                div(style = "display: inline-block; width: 25px;", image)
                              )
                          }
                          
                          list
                          
                          # image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                          # 
                          # div(style = "display: inline-block; width: 25px;", image)
                        }
                      ),
                    `Minutes Played` = colDef(footer = dataSum$`Minutes Played`),
                    `Average Rating` = colDef(footer = dataSum$`Average Rating`),
                    `Player of the Match` = colDef(footer = dataSum$`Player of the Match`),
                    `Won` = colDef(footer = dataSum$`Won`),
                    `Lost` = colDef(footer = dataSum$`Lost`),
                    `Drawn` = colDef(footer = dataSum$`Drawn`),
                    `Clean Sheets` = colDef(footer = dataSum$`Clean Sheets`),
                    `Conceded` = colDef(footer = dataSum$`Conceded`),
                    `Saves Parried` = colDef(footer = dataSum$`Saves Parried`),
                    `Saves Held` = colDef(footer = dataSum$`Saves Held`),
                    `Saves Tipped` = colDef(footer = dataSum$`Saves Tipped`),
                    `Save%` = colDef(footer = dataSum$`Save%`),
                    `Penalties Faced` = colDef(footer = dataSum$`Penalties Faced`),
                    `Penalties Saved` = colDef(footer = dataSum$`Penalties Saved`),
                    `xSave%` = colDef(footer = dataSum$`xSave%`),
                    `Apps` = colDef(footer = dataSum$`Apps`)
                    
                  ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
              )
              
          } else {
            data %>% 
              reactable(
                pagination = FALSE,
                theme = pff(font_color = "#000"),
                columns = 
                  list(
                    Season = colDef(
                      footer = "Totals",
                      maxWidth = 80,
                      style = list(position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1),
                      headerStyle = list(position = "sticky", left = 0, zIndex = 1),
                      footerStyle = list(fontWeight = "bold", position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1)
                    ),
                    Club = 
                      colDef(
                        maxWidth = 60,
                        align = "center",
                        class = "cell",
                        cell = function(value){
                          if(value %>% str_detect(",")){
                            clubs <- str_split(value, pattern = ",", simplify = TRUE) %>% c()
                            
                            list <- 
                              tagList(
                                lapply(
                                  clubs,
                                  function(X){
                                    div(
                                      style = "display: inline-block; width: 25px;", 
                                      img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                                    )
                                  }
                                )
                              )
                            
                          } else {
                            image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                            
                            list <- 
                              tagList(
                                div(style = "display: inline-block; width: 25px;", image)
                              )
                          }
                          
                          list
                          
                          # image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                          # 
                          # div(style = "display: inline-block; width: 25px;", image)
                        }
                      ),
                    `Minutes Played` = colDef(footer = dataSum$`Minutes Played`),
                    `Distance Run (km)` = colDef(footer = dataSum$`Distance Run (km)`),
                    `Average Rating` = colDef(footer = dataSum$`Average Rating`),
                    `Player of the Match` = colDef(footer = dataSum$`Player of the Match`),
                    `Goals` = colDef(footer = dataSum$`Goals`),
                    `Assists` = colDef(footer = dataSum$`Assists`),
                    `xG` = colDef(footer = dataSum$`xG`),
                    `Shots on Target` = colDef(footer = dataSum$`Shots on Target`),
                    `Shots` = colDef(footer = dataSum$`Shots`),
                    `Penalties Taken` = colDef(footer = dataSum$`Penalties Taken`),
                    `Penalties Scored` = colDef(footer = dataSum$`Penalties Scored`),
                    `Successful Passes` = colDef(footer = dataSum$`Successful Passes`),
                    `Attempted Passes` = colDef(footer = dataSum$`Attempted Passes`),
                    `Pass%` = colDef(footer = dataSum$`Pass%`),
                    `Key Passes` = colDef(footer = dataSum$`Key Passes`),
                    `Successful Crosses` = colDef(footer = dataSum$`Successful Crosses`),
                    `Attempted Crosses` = colDef(footer = dataSum$`Attempted Crosses`),
                    `Cross%` = colDef(footer = dataSum$`Cross%`),
                    `Chances Created` = colDef(footer = dataSum$`Chances Created`),
                    `Successful Headers` = colDef(footer = dataSum$`Successful Headers`),
                    `Attempted Headers` = colDef(footer = dataSum$`Attempted Headers`),
                    `Header%` = colDef(footer = dataSum$`Header%`),
                    `Key Headers` = colDef(footer = dataSum$`Key Headers`),
                    `Dribbles` = colDef(footer = dataSum$`Dribbles`),
                    `Tackles Won` = colDef(footer = dataSum$`Tackles Won`),
                    `Attempted Tackles` = colDef(footer = dataSum$`Attempted Tackles`),
                    `Tackle%` = colDef(footer = dataSum$`Tackle%`),
                    `Key Tackles` = colDef(footer = dataSum$`Key Tackles`),
                    `Interceptions` = colDef(footer = dataSum$`Interceptions`),
                    `Clearances` = colDef(footer = dataSum$`Clearances`),
                    `Mistakes Leading to Goals` = colDef(footer = dataSum$`Mistakes Leading to Goals`),
                    `Yellow Cards` = colDef(footer = dataSum$`Yellow Cards`),
                    `Red Cards` = colDef(footer = dataSum$`Red Cards`),
                    `Fouls` = colDef(footer = dataSum$`Fouls`),
                    `Fouls Against` = colDef(footer = dataSum$`Fouls Against`),
                    `Offsides` = colDef(footer = dataSum$`Offsides`),
                    `Apps` = colDef(footer = dataSum$`Apps`)
                    
                  ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
              )
          }
        }
      })
      
      
      ##----------------------------------------------------------------
      ##                        Visualizations                         -
      ##----------------------------------------------------------------
      
      output$Development <- renderPlotly({
        res <-  
          GET(
            url = "https://api.simulationsoccer.com/ssl/getPlayerStatistics",
            query = 
              list(
                player = input$player
              )
          )
        
        data <- fromJSON(res$content %>% rawToChar())
        
        data <- 
          data %>% 
          select(
            Name,
            Club,
            Result:Division,
            Acc:Wor
          ) %>% 
          mutate(
            index = 1:n()
          ) %>% 
          pivot_longer(
            cols = Acc:Wor
          ) %>% 
          left_join(
            attributes, 
            by = c("name"= "abbr")
          ) %>% 
          filter(
            !(Attribute %in% c("Stamina", "Natural Fitness"))
          )
        
        if(any(reactives$currentBuild$Group == "Goalkeeper")){
          data <- 
            data %>% 
            filter(
              Keeper == TRUE
            )
        } else {
          data <- 
            data %>% 
            filter(
              Group != "Goalkeeper"
            )
        }
        
        
        fig1 <-
          plot_ly(
            data = data %>% 
              filter(Group == "Physical"),
            x = ~index,
            y = ~value,
            color = ~Attribute,
            text = ~Attribute,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text+y",
            colors = "Paired",
            name = 
              paste(
                "Physical",
                data %>%
                  filter(Group == "Physical") %>% 
                  select(Attribute) %>% 
                  unlist(),
                sep = " - "
                )
          ) %>% 
          layout(
            yaxis = 
              list(
                title = "Value",
                range = c(4, 21),
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            xaxis = 
              list(
                title = "Career",
                showticklabels = FALSE,
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            hovermode = "x unified"
          )
        
        fig2 <- 
          plot_ly(
            data = data %>% 
              filter(Group == "Technical"),
            x = ~index,
            y = ~value,
            color = ~Attribute,
            text = ~Attribute,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text+y",
            colors = "Paired",
            name = 
              paste(
                "Technical",
                data %>%
                  filter(Group == "Technical") %>% 
                  select(Attribute) %>% 
                  unlist(),
                sep = " - "
              )
          ) %>% 
          layout(
            yaxis = 
              list(
                title = "Value",
                range = c(4, 21),
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            xaxis = 
              list(
                title = "Career",
                showticklabels = FALSE,
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            hovermode = "x unified"
          )
        
        fig3 <- 
          plot_ly(
            data = data %>% 
              filter(Group == "Mental"),
            x = ~index,
            y = ~value,
            color = ~Attribute,
            text = ~Attribute,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text+y",
            colors = "Paired",
            name = 
              paste(
                "Mental",
                data %>%
                  filter(Group == "Mental") %>% 
                  select(Attribute) %>% 
                  unlist(),
                sep = " - "
              )
          ) %>% 
          layout(
            yaxis = 
              list(
                title = "Value",
                range = c(4, 21),
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            xaxis = 
              list(
                title = "Career",
                showticklabels = FALSE,
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            hovermode = "x unified"
          )
        
        
        if(any(reactives$currentBuild$Group == "Goalkeeper")){
          fig4 <- 
            plot_ly(
              data = data %>% 
                filter(Group == "Goalkeeper"),
              x = ~index,
              y = ~value,
              color = ~Attribute,
              text = ~Attribute,
              type = "scatter",
              mode = "lines",
              hoverinfo = "text+y",
              colors = "Paired",
              name = 
                paste(
                  "Goalkeeper",
                  data %>%
                    filter(Group == "Goalkeeper") %>% 
                    select(Attribute) %>% 
                    unlist(),
                  sep = " - "
                )
            ) %>% 
            layout(
              yaxis = 
                list(
                  title = "Value",
                  range = c(4, 21),
                  linecolor = "black",
                  linewidth = 0.2,
                  mirror = TRUE
                ),
              xaxis = 
                list(
                  title = "Career",
                  showticklabels = FALSE,
                  linecolor = "black",
                  linewidth = 0.2,
                  mirror = TRUE
                ),
              hovermode = "x unified"
            )
          
          fig <- 
            subplot(
              fig1, fig2, fig3, fig4, 
              nrows = 4, 
              shareY = TRUE,
              titleY = FALSE
            ) %>% 
            suppressWarnings()
          
          annotations <-
            list( 
              list( 
                x = -0.1,  
                y = 1.0, 
                font = list(size = 15),
                text = "Physical",  
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = -0.1,  
                y = 0.72,  
                text = "Technical",  
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = -0.1,  
                y = 0.47,  
                text = "Mental",  
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),
              list( 
                x = -0.1,  
                y = 0.22,  
                text = "Goalkeeper",
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),
              list( 
                x = 0.5,  
                y = -0.05,  
                text = "Career",  
                font = list(size = 12),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              )
            )
          
          
        } else {
          fig <- 
            subplot(
              fig1, fig2, fig3, 
              nrows = 3, 
              shareY = TRUE,
              titleY = FALSE
            ) %>% 
            suppressWarnings()
          
          annotations <-
            list( 
              list( 
                x = -0.1,  
                y = 1.0,  
                text = "Physical",  
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = -0.1,  
                y = 0.63,  
                text = "Technical", 
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = -0.1,  
                y = 0.30,  
                text = "Mental",  
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),
              list( 
                x = 0.5,  
                y = -0.05,  
                text = "Career",  
                font = list(size = 12),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              )
            )
        }
        
        
        fig %>%
          plotly::config(
            modeBarButtonsToRemove =
              c("zoom", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                "resetScale2d", "hoverClosestCartesian",
                "hoverCompareCartesian", "toggleSpikelines"
              )
          ) %>%
          layout(
            annotations = annotations,
            margin = 
              list(
                l = 120
              ),
            # autosize = TRUE,
            dragmode= FALSE,
            ## Legend is put to false so the plot is the same size
            paper_bgcolor='#ffffff00',
            plot_bgcolor='#ffffff00'
          )
        
        
        # p <- ggplot(data) + aes(x = index, y = value, color = Attribute) + geom_line() +
        #   facet_wrap(vars(Group)) + scale_y_continuous(limits = c(5, 20)) +
        #   theme_bw()
        # p %>% 
        #   ggplotly()
      })
      
      output$radarPlotly <- renderPlotly({
        if(radarAttributes() %>% is.null()){
          plotly_empty(
            type = "scatter", 
            mode = "markers",
            width = NULL,
            height = NULL
          ) %>%
            plotly::config(
              displayModeBar = FALSE
            ) %>%
            layout(
              autosize = TRUE,
              title = list(
                text = "Select a player to show visualization",
                yref = "paper",
                y = 0.5
              )
            )
        } else {
          
          data <- radarAttributes() %>% 
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
              data = data,
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
              name = ~Name,
              data = data
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
      )
    }
  )
}

