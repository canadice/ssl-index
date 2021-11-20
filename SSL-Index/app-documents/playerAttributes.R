
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SHL                 ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
playerComparisonUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        
        ##----------------------------------------------------------------
        ##                          First column                         -
        ##----------------------------------------------------------------
        
        column(
          width = 4,
          selectInput(
            inputId = ns("teamOne"),
            label = "Select team to search",
            choices = 
              c(
                "Free Agents" = "FA"#,
                #teams$team
              )
          ),
          uiOutput(
            outputId = ns("selectPlayerOne")
          )
        ),
        
        ##----------------------------------------------------------------
        ##                          Third column                         -
        ##----------------------------------------------------------------
        
        column(
          width = 4,
          offset = 4,
          selectInput(
            inputId = ns("teamTwo"),
            label = "Select team to search",
            choices = 
              c(
                "Free Agents" = "FA"#,
                #teams$team
              )
          ),
          uiOutput(
            outputId = ns("selectPlayerTwo")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            title = "Visualization",
            plotlyOutput(
              outputId = ns("radarPlotly")
            )
          )
        ),
        column(
          width = 6,
          box(
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            title = "Positional Experience",
            plotlyOutput(
              outputId = ns("fieldPlotly")
            )
          )
        )
      )
    )
  )
}

## Backend module for player similarities
playerComparisonSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){

      ###  Selecting a player
      output$selectPlayerOne <- renderUI({
        selectInput(
          inputId = session$ns("playerOne"),
          label = "Select player",
          choices = 
            playerData %>% 
            filter(
              Team == input$teamOne
            ) %>% 
            transmute(
              Name = paste(`First Name`, `Last Name`)
            ) %>% 
            arrange(
              Name
            ) %>% 
            unname() %>% 
            unlist()
        )
      })
      
      output$selectPlayerTwo <- renderUI({
        selectInput(
          inputId = session$ns("playerTwo"),
          label = "Select player",
          choices = 
            c(
              "",
              playerData %>% 
                filter(
                  Team == input$teamTwo
                ) %>% 
                transmute(
                  Name = paste(`First Name`, `Last Name`)
                ) %>% 
                arrange(
                  Name
                ) %>%
                unname() %>% 
                unlist()
            )
        )
      })
           
      ###  Creating data sets to visualize                                         
      playerOne <- reactive({
        if(input$playerOne %>% length() == 0){
          NULL
        } else {
          playerData %>% 
            mutate(
              Name = paste(`First Name`, `Last Name`)
            ) %>% 
            filter(
              Name == input$playerOne
            )  
        }
      })
      
      playerTwo <- reactive({
        if(input$playerTwo %>% length() == 0){
          NULL
        } else if(input$playerTwo == ""){
          NULL
        } else {
          playerData %>% 
            mutate(
              Name = paste(`First Name`, `Last Name`)
            ) %>% 
            filter(
              Name == input$playerTwo
            )  
        }
      })
      
      ### Visualization
      output$radarPlotly <- renderPlotly({
        if(playerOne() %>% is.null()){
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
                text = "Select a player in the table below to show visualization",
                yref = "paper",
                y = 0.5
              )
            )
        } else {
          fig <- 
            playerOne() %>% 
            select(
              Name,
              Acceleration:`Work Rate`
            ) %>% 
            pivot_longer(
              where(is.numeric),
              names_to = "attributeIndex",
              values_to = "Rating"
            ) %>% 
            mutate(
              text = paste(attributeIndex, Rating, sep = ": ")
            ) %>%
            plot_ly(
              type = 'scatterpolar',
              mode = "markers",
              r = ~Rating,
              theta = ~attributeIndex,
              text = ~text,
              fill = 'toself',
              hoverinfo = "text",
              color = I("#cf5b00"),
              name = ~Name,
              width = NULL,
              height = NULL
            ) 
          ## Adding on a second trace for another player
          if(playerTwo() %>% is.null()){
            
          } else {
            
            data <- 
              playerTwo() %>% 
              select(
                Name,
                Acceleration:`Work Rate`
              ) %>% 
              pivot_longer(
                where(is.numeric),
                names_to = "attributeIndex",
                values_to = "Rating"
              ) %>% 
              mutate(
                text = paste(attributeIndex, Rating, sep = ": ")
              ) 
            
            fig <- 
              fig %>%
              add_trace(
                r = data$Rating,
                theta = data$attributeIndex,
                text = data$text,
                color = I("#0074CF"),
                name = data$Name
              )
          }
          
          fig %>%
            plotly::config(
              modeBarButtonsToRemove =
                c("pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                  "resetScale2d", "hoverClosestCartesian",
                  "hoverCompareCartesian", "toggleSpikelines"
                )
            ) %>%
            layout(
              autosize = TRUE,
              polar =
                list(
                  radialaxis =
                    list(
                      visible = TRUE,
                      range = c(0,20)
                    )
                ),
              ## Legend is put to false so the plot is the same size
              showlegend = FALSE
            )
        }
      })
      
      output$pitchExp <- renderPlotly({
        if(playerOne() %>% is.null()){
          ggplot(NULL) + 
            annotation_custom(
              pitch %>% rastergrob,
              xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf
            )
          
          
          
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
                text = "Select a player in the table below to show visualization",
                yref = "paper",
                y = 0.5
              )
            )
        } else {
          fig <- 
            playerOne() %>% 
            select(
              Name,
              Acceleration:`Work Rate`
            ) %>% 
            pivot_longer(
              where(is.numeric),
              names_to = "attributeIndex",
              values_to = "Rating"
            ) %>% 
            mutate(
              text = paste(attributeIndex, Rating, sep = ": ")
            ) %>%
            plot_ly(
              type = 'scatterpolar',
              mode = "markers",
              r = ~Rating,
              theta = ~attributeIndex,
              text = ~text,
              fill = 'toself',
              hoverinfo = "text",
              color = I("#cf5b00"),
              name = ~Name,
              width = NULL,
              height = NULL
            ) 
          ## Adding on a second trace for another player
          if(playerTwo() %>% is.null()){
            
          } else {
            
            data <- 
              playerTwo() %>% 
              select(
                Name,
                Acceleration:`Work Rate`
              ) %>% 
              pivot_longer(
                where(is.numeric),
                names_to = "attributeIndex",
                values_to = "Rating"
              ) %>% 
              mutate(
                text = paste(attributeIndex, Rating, sep = ": ")
              ) 
            
            fig <- 
              fig %>%
              add_trace(
                r = data$Rating,
                theta = data$attributeIndex,
                text = data$text,
                color = I("#0074CF"),
                name = data$Name
              )
          }
          
          fig %>%
            plotly::config(
              modeBarButtonsToRemove =
                c("pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                  "resetScale2d", "hoverClosestCartesian",
                  "hoverCompareCartesian", "toggleSpikelines"
                )
            ) %>%
            layout(
              autosize = TRUE,
              polar =
                list(
                  radialaxis =
                    list(
                      visible = TRUE,
                      range = c(0,20)
                    )
                ),
              ## Legend is put to false so the plot is the same size
              showlegend = FALSE
            )
        }
      })
    }
  )
}

