
############################################################################
############################################################################
###                                                                      ###
###                      COMPARE PLAYERS IN THE SSL                      ###
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
        
        tabBox(
          width = NULL,
          tabPanel(
            "Compare Players",
            ##----------------------------------------------------------------
            ##                          First column                         -
            ##----------------------------------------------------------------
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = ns("teamOne"),
                  label = "Select team to search",
                  choices = 
                    c(
                      "Free Agents" = "FA",
                      "Retired",
                      "Prospects",
                      teamInfo$team
                    ),
                  selected = "Free Agents"
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
                      "Free Agents" = "FA",
                      "Retired",
                      "Prospects",
                      teamInfo$team
                    ),
                  selected = "Free Agents"
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
                  id = ns("fieldExperience"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  title = "Positional Experience",
                  imageOutput(
                    outputId = ns("fieldImage"),
                    width = "100%",
                    height = "100%"
                  ) %>% 
                    div(
                      align = "center"
                    )
                )
              )
            )
          ),
          tabPanel(
            "Draft Class Tracker",
            fluidRow(
              column(
                width = 2,
                box(
                  title = "Information",
                  status = "info",
                  solidHeader = TRUE,
                  width = NULL,
                  selectInput(
                    inputId = ns("class"),
                    label = "Select Draft Class",
                    choices = c(
                      "ALL",
                      unique(playerData$Class) %>% sort(decreasing = TRUE))
                  )
                )
              ),
              column(
                width = 10,
                box(
                  title = "Tracker",
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  DT::DTOutput(
                    outputId = ns("tableTPE")
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
playerComparisonSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ## Creates positional coordinates off-center compared to the ones in the database
      {positionalCoord <- 
        data.frame(
          x = 
            c(
              372, 
              
              130,
              372,
              620,
              
              130,
              372,
              620,
              
              130,
              372,
              620,
              
              130,
              372,
              620,
              
              372
            ),
          y = 
            c(
              775,
              
              625,
              625,
              625,
              
              455,
              455,
              455,
              
              310,
              310,
              310,
              
              150,
              150,
              150,
              
              50
            )
        )}
      ## Loads selected data for TPE Tracker
      currentTPEData <- reactive({
        if(input$class != "ALL"){
          playerData <- 
            playerData %>% 
            filter(
              Class == input$class
            )  
        }
        
        playerData %>% 
          select(
            Name,
            Username,
            Class,
            Team,
            `Preferred Position`,
            TPE,
            # `Applied TPE` = TPE - `TPE Available`,
            Active
          ) %>% 
          left_join(
            teamInfo %>% 
              select(
                team, 
                color_primary,
                color_secondary
              ),
            by = c("Team" = "team")
          )
        
      })
      
      ## js function for automatic reranking
      js <- c(
        "table.on('draw.dt', function(){",
        "  var PageInfo = table.page.info();",
        "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
        "    cell.innerHTML = i + 1 + PageInfo.start;",
        "  });",
        "})")
      
      ## TPE Tracker for different classes
      output$tableTPE <- renderDT({
        datatable(
          currentTPEData() %>% 
            arrange(
              -TPE
            ), 
          callback = JS(js),
          style = "bootstrap",
          class = 'compact cell-border stripe',
          rownames = TRUE,
          escape = FALSE,
          options = 
            list(
              ordering = TRUE, 
              ## Sets a scroller for the rows
              scrollX = '800px',
              scrollY = '550px',
              ## Sets size of rows shown
              scrollCollapse = TRUE,
              paging = FALSE,
              dom = 'ft',
              columnDefs = 
                list(
                  list(
                    targets = 8:9,
                    visible = FALSE
                  )
                )
            )
        ) %>% 
          formatStyle(
            columns = 0:7,
            valueColumns = "color_primary",
            backgroundColor = 
              styleEqual(
                sort(unique(teamInfo$color_primary)), 
                sort(unique(teamInfo$color_primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:7,
            valueColumns = "color_secondary",
            color = 
              styleEqual(
                sort(unique(teamInfo$color_secondary)), 
                sort(unique(teamInfo$color_secondary))
              )
          )
      })
      
      ###  Selecting a player
      output$selectPlayerOne <- renderUI({
        selectInput(
          inputId = session$ns("playerOne"),
          label = tags$span(style="color: #cf5b00;","Select player"),
          choices = 
            playerData %>% 
            filter(
              Team == input$teamOne
            ) %>% 
            select(
              Name
            ) %>% 
            arrange(
              Name
            )
        )
      })
      
      output$selectPlayerTwo <- renderUI({
        selectInput(
          inputId = session$ns("playerTwo"),
          label = tags$span(style="color: #0074CF;","Select player"),
          choices = 
            c(
              "",
              playerData %>% 
                filter(
                  Team == input$teamTwo
                ) %>% 
                select(
                  Name
                ) %>% 
                arrange(
                  Name
                ) 
            )
        )
      })
           
      ###  Creating data sets to visualize                                         
      playerOne <- reactive({
        if(input$playerOne %>% length() == 0){
          NULL
        } else {
          playerData %>% 
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
            filter(
              Name == input$playerTwo
            )  
        }
      })
      
      ### Visualization
      output$radarPlotly <- renderPlotly({
        if(playerOne() %>% is.null() & playerTwo() %>% is.null()){
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
        } else if(playerTwo() %>% is.null()){
          
          data <- playerOne()
          
          if(data$Position == "Goalkeeper"){
            data <- 
              data %>% 
              select(
                Name,
                Acceleration:`Work Rate`,
                `Aerial Reach`:`Tendency to Rush`
              ) %>% 
              select_if(
                ~mean(is.na(.)) < 0.9
              )
          } else {
            data <- 
              data %>% 
              select(
                Name,
                Acceleration:`Work Rate`
              ) 
          }
          
          fig <- 
            data %>% 
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
        } else {
          data <- playerOne()
          data2 <- playerTwo()
          
          if(sum(data2$Position == "Goalkeeper", data$Position == "Goalkeeper") == 1){
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
                  text = "You have selected a goalkeeper and an <br> outfielder which cannot be compared",
                  yref = "paper",
                  y = 0.5
                )
              )
          } else {
            if(data$Position == "Goalkeeper"){
              data <- 
                data %>% 
                select(
                  Name,
                  Acceleration:`Work Rate`,
                  `Aerial Reach`:`Tendency to Rush`
                ) %>% 
                select_if(
                  ~mean(is.na(.)) < 0.9
                )
            } else {
              data <- 
                data %>% 
                select(
                  Name,
                  Acceleration:`Work Rate`
                ) 
            }
            
            if(data2$Position == "Goalkeeper"){
              data2 <- 
                data2 %>% 
                select(
                  Name,
                  Acceleration:`Work Rate`,
                  `Aerial Reach`:`Tendency to Rush`
                ) %>% 
                select_if(
                  ~mean(is.na(.)) < 0.9
                )
            } else {
              data2 <- 
                data2 %>% 
                select(
                  Name,
                  Acceleration:`Work Rate`
                ) 
            }
            
            data2 <- 
              data2 %>%  
              pivot_longer(
                where(is.numeric),
                names_to = "attributeIndex",
                values_to = "Rating"
              ) %>% 
              mutate(
                text = paste(attributeIndex, Rating, sep = ": ")
              ) 

            fig <- 
              data %>% 
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
              ) %>%
              add_trace(
                r = data2$Rating,
                theta = data2$attributeIndex,
                text = data2$text,
                color = I("#0074CF"),
                name = data2$Name
              )
            
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
        }
      })
      
      output$fieldImage <- renderImage({
        base <- pitch %>% image_ggplot()
        
        p <- 
          base + 
          geom_point(
            mapping = aes(x = x, y = y),
            data = positionalCoord
          ) 
        
        if(playerOne() %>% is.null()){
          p
        } else {
          expData1 <- 
            playerOne() %>% 
            select(
              Name,
              Striker:Goalkeeper
            ) %>% 
            pivot_longer(
              where(is.numeric),
              names_to = "posExp",
              values_to = "Value"
            ) %>% 
            cbind(
              positionalCoord,
              .
            ) 
            
          
          p <- 
            base + 
            geom_point(
              mapping = aes(x = x-20, y = y, size = Value),
              data = expData1,
              color = "#cf5b00"
            )
          
          ## Adding on a second trace for another player
          if(playerTwo() %>% is.null()){
            
          } else {
            
            expData2 <- 
              playerTwo() %>% 
              select(
                Name,
                Striker:Goalkeeper
              ) %>% 
              pivot_longer(
                where(is.numeric),
                names_to = "posExp",
                values_to = "Value"
              ) %>% 
              cbind(
                positionalCoord,
                .
              )
            
            p <- 
              p + 
              geom_point(
                mapping = aes(x = x+20, y = y, size = Value),
                data = expData2,
                color = "#0074CF"
              )
            
          }
        }
        card <- image_graph(res = 96)
        print(
          p + 
          theme(
            legend.position = "none"
          )
        )
        dev.off()
        
        tempImage <- 
          card %>% 
          image_crop(geometry = "480x600+160") %>% 
          image_write(tempfile(fileext = "png"), format = "png")
        
        return(
          list(
            src = tempImage, 
            contentType = "image/png"
            )
          )
      },
      deleteFile = TRUE
      )
    }
  )
}

