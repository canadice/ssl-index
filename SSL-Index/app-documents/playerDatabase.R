
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
                select(Name) %>% 
                arrange(Name)
            # select(Team, Name) %>% 
            # group_by(Team) %>% 
            # arrange(Name) %>% 
            # group_split(.keep = FALSE) %>% 
            # setNames(playerData$Team %>% unique() %>% sort()) %>% 
            # lapply(
            #   X = .,
            #   FUN = function(x){
            #     c(x) %>% 
            #       unname() %>% 
            #       lapply(
            #         X = .,
            #         FUN = function(x){
            #           as.list(x)
            #         }
            #       ) %>% 
            #       unlist(recursive = FALSE)
            #   }
            # )
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
          tabPanel(
            "Player Updater",
            
            fluidRow(
              ##-----------------------------------
              ##  Left hand side with information  
              ##-----------------------------------
              column(
                width = 4,
                box(
                  title = "Information",
                  status = "info",
                  solidHeader = TRUE,
                  width = NULL,
                  h4("TPE Information", align = "center"),
                  fluidRow(
                    column(
                      width = 10,
                      offset = 1,
                      uiOutput(
                        outputId = ns("earnedTPE")
                      ),
                      uiOutput(
                        outputId = ns("usedTPE")
                      )
                    )
                  ),
                  br(),
                  h5("Update Scale" %>% strong(), align = "center"),
                  DTOutput(
                    outputId = ns("costTPE"),
                    width = "80%"
                  ) %>% 
                    div(align = "center"),
                  actionButton(
                    inputId = ns("exportButton"),
                    label = "Export",
                    width = "80%"
                  ) %>%
                    div(align = "center")
                )
              ),
              
              ##------------------------------------
              ##  Right hand side with the builder  
              ##------------------------------------
              column(
                width = 8,
                p("Edit the Value column by double clicking on the cell of the attribute you want to edit. 
                Clicking on export provides a formatted text with all the updates you have made on your build."),
                DTOutput(ns("attributeTable"), width = "80%")
              )
              ##------------------------------------
            )
          ),
          tabPanel(
            "Game Log",
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  inputId = ns("gameSeason"),
                  label = "Filter Season",
                  choices =
                    keeperGameData$Season %>%
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
playerDatabaseSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      
      ##---------------------------------------------------------------
      ##                        Reactive values                       -
      ##---------------------------------------------------------------
      
      currentAvailable <- reactiveVal(350)
      
      currentCost <- reactiveVal(0)
      
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
            -Keeper
          )
      })
      
      ##---------------------------------------------------------------
      ##                          Observers                           -
      ##---------------------------------------------------------------
      
      ## Observer that calculates the correct used TPE and available TPE for the build
      observeEvent(
        input$currentTPE,
        {
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
          currentCost(sum(reactives$currentBuild$cost))
        },
        ignoreInit = FALSE
      )
      
      ## Observes a change in player and updates the current build and costs accordingly
      observeEvent(
        input$player,
        {
          reactives$currentBuild <- 
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
              -Keeper
            )
          
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
          currentCost(sum(reactives$currentBuild$cost))
        }
      )
      
      observeEvent(
        input$attributeTable_cell_edit,
        {
          editedCell = input$attributeTable_cell_edit
          i = as.numeric(editedCell$row)
          j = as.numeric(editedCell$col)
          k = as.numeric(editedCell$value)
          
          # Checks if new value falls within attribute constraints
          if(k < 5){ 
            k <- 5
          } else if(k > 20) {
            k <- 20
          }
          
          # Checks if you alter the Natural Fitness or Stamina which are fixed attributes
          if(i %in% c(5,7)){
            k <- 20 
            
            reactives$currentBuild[i,j] <- k
          } else {
            #write values to reactive
            reactives$currentBuild[i,j] <- k
            
            reactives$currentBuild[i,"cost"] <- tpeCost$cost[tpeCost$value == k]  
          }
          
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
          currentCost(sum(reactives$currentBuild$cost))
        }
      )
      
      
      ## Observer for the export button that creates a formatted text with all info of the build
      observeEvent(
        input$exportButton,
        {
          current <- 
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
              -Keeper
            )
          
          updated <- 
            reactives$currentBuild
          
          merged <- 
            current %>% 
            left_join(
              updated,
              by = "Attribute",
              suffix = c(".old", ".new")
            ) %>% 
            filter(
              Value.old != Value.new
            ) %>% 
            mutate(
              ChangeCost = cost.new - cost.old
            ) %>% 
            select(
              Attribute,
              Value.old,
              Value.new,
              ChangeCost
            )  
          
          if(merged %>% nrow() > 0){
            showModal(
              modalDialog(
                span(
                  "Copy the code below containing your updates into your update post."
                ),
                br(),
                br(),
                column(
                  width = 8,
                  offset = 2,
                  helpText(
                    paste(
                      paste(
                        paste(
                          "[b]Earned TPE:[/b] ", 
                          playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                          "+",
                          input$currentTPE-playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                          "=",
                          input$currentTPE),
                        paste("[b]Used TPE:[/b] ", currentCost()), 
                        paste("[b]Banked TPE:[/b] ", currentAvailable()),
                        sep = "<br>"
                      ),
                      " ",
                      paste(
                        apply(
                          X = merged,
                          MARGIN = 1, 
                          FUN = function(merged){
                            paste(merged[1], paste(merged[2:3], collapse = " -> "), paste("(",merged[4], ")", sep = ""), collapse = " ")    
                          }
                        ),
                        collapse = "<br>"
                      ),
                      sep = "<br>"
                    ) %>% 
                      HTML()
                  ) %>% 
                    div(
                      style = "background: #f0f0f0; border: #656565"
                    )
                ),
                title="Update output",
                footer = 
                  tagList(
                    modalButton("Ok")
                  ),
                easyClose = TRUE
              )
            )
          } else {
            showModal(
              modalDialog(
                span(
                  "You have not changed your build yet."
                ),
                title="Update output",
                footer = 
                  tagList(
                    modalButton("Ok")
                  ),
                easyClose = TRUE
              )
            )
          }
          
        }
      )
      
      ##---------------------------------------------------------------
      ##                      Datatable outputs                       -
      ##---------------------------------------------------------------
      
      ## Table with the cost of all attribute values
      output$costTPE <- renderDT({
        datatable(
          data.frame(
            `Value` = c("5-7", "8-10", "11-13", "14-16", "17-18", "19-20"),
            Cost = c(2, 4, 6, 12, 18, 25)
          ),
          class = 'compact cell-border stripe',
          rownames = FALSE,
          style = "bootstrap",
          escape = FALSE,
          options = 
            list(
              ordering = FALSE,
              dom = 't',
              ## Sets color of table background
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                "}")
            )
        )  
      })
      
      ## TPE information UI
      output$usedTPE <- renderUI({
        tagList(
          fluidRow(
            column(
              width = 6,
              h5("Used TPE")
            ),
            column(
              width = 6,
              h5(currentCost())
            )
          ),
          fluidRow(
            column(
              width = 6,
              h5("Available TPE")
            ),
            column(
              width = 6,
              h5(currentAvailable())
            )
          ),
          fluidRow(
            column(
              width = 6,
              h5("Forum TPE")
            ),
            column(
              width = 6,
              h5(playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname())
            )
          )
        )
      })
      
      output$earnedTPE <- renderUI({
        tagList(
          fluidRow(
            column(
              width = 6,
              h5("Earned TPE")
            ),
            column(
              width = 6,
              numericInput(
                inputId = session$ns("currentTPE"),
                label = NULL,
                value = playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                min = 350,
                max = 2500,
                width = "80%"
              )
            )
          )
        )
      })
      
      ##----------------------------------------------------------------
      ##              Datatable output for attributes                  -
      ##----------------------------------------------------------------
      
      output$attributeTable <- renderDT({
        datatable(
          reactives$currentBuild %>% 
            relocate(
              Group,
              .before = Attribute
            ) %>% 
            rename(
              Cost = cost
            ), 
          rownames = FALSE,
          selection = "none",
          editable = 
            list(
              target = "cell",
              disable = 
                list(
                  columns = c(0,1,3)
                )
            ),
          extensions = "RowGroup",
          options = 
            list(
              rowGroup = list(datSrc = 0),
              ordering = FALSE, 
              pageLength = -1,
              dom = 'Rt',
              ## Sets color of table background
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                "}")
            )
          )
      })
      
      # Using reactable
      output$GameData <- renderReactable({
        if(any(reactives$currentBuild$Group == "Goalkeeper")){
          data <- 
            keeperGameData %>% 
            filter(
              Name == input$player
            )
        } else {
          data <- 
            playerGameData %>% 
            filter(
              Name == input$player
            ) %>% 
            mutate(
              `Average Rating` = round(`Average Rating`, 2),
              xG = round(xG, 2)
            ) 
        }
        
        if(!is.null(input$gameSeason)){
          data <- 
            data %>% 
            filter(
              Season %in% input$gameSeason
            )
        }
        
        if(!is.null(input$gameFilter)){
          if("Cup" %in% input$gameFilter){
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
              Matchday,
              Club,
              Opponent,
              Result
            ),
            .before = `Minutes Played` 
          ) %>% 
          reactable(
            pagination = FALSE,
            columns = 
              list(
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
                  ),
                Opponent = 
                  colDef(
                    name = "Opp.",
                    maxWidth = 50,
                    class = "cell",
                    align = "center",
                    cell = function(value, index){
                      
                      if(value %>% is.na()){
                        div(class = "club", " ")
                        
                      } else {
                        logo <- 
                          img(
                            class = "logo",
                            src = teamInfo$logo[teamInfo$team == value],
                            alt = value,
                            height = 30
                          )
                        
                        div(class = "club", logo)
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
                        
                        if(any(str_detect(values, pattern = "p"))){
                          
                          if(which(str_detect(values, pattern = "p")) == 1){
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
      
      # Using DT
      # output$GameData <- renderDT({
      #   if(any(reactives$currentBuild$Group == "Goalkeeper")){
      #     data <- 
      #       keeperGameData %>% 
      #       filter(
      #         Name == input$player
      #       )
      #   } else {
      #     data <- 
      #       playerGameData %>% 
      #       filter(
      #         Name == input$player
      #       ) 
      #   }
      #   
      #   if(!is.null(input$gameSeason)){
      #     data <- 
      #       data %>% 
      #       filter(
      #         Season %in% input$gameSeason
      #       )
      #   }
      #   
      #   if(!is.null(input$gameFilter)){
      #     if("Cup" %in% input$gameFilter){
      #       data <- 
      #         data %>% 
      #         filter(
      #           str_detect(Matchday, "Cup")
      #         )
      #     } else {
      #       data <- 
      #         data %>% 
      #         filter(
      #           str_detect(Matchday, "Cup", negate = TRUE)
      #         )
      #     }
      #   }
      #   
      #   data %>% 
      #     select(
      #       -Name,
      #       -Nationality,
      #       -Position,
      #       -Apps
      #     ) %>% 
      #     relocate(
      #       Season,
      #       .before = "Minutes Played"
      #     ) %>% 
      #     relocate(
      #       Matchday,
      #       .after = "Season"
      #     ) %>% 
      #     relocate(
      #       c(
      #         Club,
      #         Opponent,
      #         Result
      #       ),
      #       .after = "Matchday"
      #     ) %>% 
      #     mutate(
      #       `Average Rating` = `Average Rating` %>% round(2),
      #       xG = xG %>% round(2)
      #     ) %>% 
      #     rename(
      #       S = Season,
      #       MD = Matchday
      #     )
      # },
      # rownames = FALSE,
      # escape = FALSE,
      # style = "bootstrap",
      # class = 'compact cell-border stripe',
      # selection = "none",
      # extensions = "FixedColumns",
      # options = 
      #   list(
      #     orderClasses = FALSE, 
      #     # order = list(list(0, 'asc')),
      #     ## Sets a scroller for the rows
      #     scrollY = '500px',
      #     ## Sets size of rows shown
      #     scrollCollapse = TRUE,
      #     ## Removes pages in the table
      #     paging = FALSE,
      #     ## Adds scrollable horizontal
      #     scrollX = TRUE,
      #     dom = 't',
      #     bInfo = FALSE,
      #     fixedColumns = 
      #       list(
      #         leftColumns = 4
      #       )
      #   ))
      
      ##----------------------------------------------------------------
      ##                        Visualizations                         -
      ##----------------------------------------------------------------
      
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

