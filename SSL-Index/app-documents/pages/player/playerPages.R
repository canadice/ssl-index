playerPagesUI <- function(id) {
  ns <- NS(id)
  tagList(
    tagList(
      fluidRow(
        column(width = 12,
               column(width = 3, 
                      uiOutput(ns("selectPlayer"))
                      ),
               column(
                 width = 6, 
                 uiOutput(ns("playerFilter"))  
               )
             )
      ),
      fluidRow(
        column(width = 6,
               box(
                 title = "Profile Information", status = "danger", solidHeader = TRUE, width = NULL,
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput(ns("playerName"))
                   ),
                   column(width = 3, imageOutput(ns("clubLogo"), height = NULL))
                 ),
                 column(width = 6,
                        uiOutput(ns("playerInfo"))
                  ),
                 column(width = 6,
                        uiOutput(ns("playerInfoExtra"))
                 )
               )
             ),
        column(width = 6,
               box(
                 title = "Recent Match Statistics", status = "danger", solidHeader = TRUE, width = NULL,
                 reactableOutput(ns("matchStatsTable"))
               )
             )
      ),
      fluidRow(
        column(width = 8,
               box(
                 title = "Player Attributes", status = "danger", solidHeader = TRUE, width = NULL,
                 uiOutput(ns("attributeTable"))
               )
        ),
        column(width = 4,
               box(
                 title = "TPE Progress", status = "danger", solidHeader = TRUE, width = NULL,
                 plotlyOutput(ns("tpeProgressPlot"))
               )
        )
      ),
      box(
        title = "Updating History", status = "danger", solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE, width = NULL,
        fluidRow(
          column(width = 12,
                 reactableOutput(ns("historyUpdates")) %>% 
                   withSpinnerMedium()
          )
        )
      ),
      box(title = "TPE History", status = "danger", solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE,width = NULL,
          fluidRow(
            column(
              width = 12,
              reactableOutput(ns("historyTPE"))
            )
          )
      ),
      box(title = "Bank History", status = "danger", solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE,width = NULL,
          fluidRow(
            column(
              width = 12,
              reactableOutput(ns("historyBank"))
            )
          )
      )
    )
  )
}

playerPagesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #### REACTIVE DATA ####
      playerData <- reactive({
        req(input$selectedPlayer)
        pid <- input$selectedPlayer %>% as.numeric()
        
        readAPI(url = "https://api.simulationsoccer.com/player/getPlayer", query = list(pid = pid))
        
        # getPlayerDataAsync(pid = pid)
      })
      
      allNames <- reactive({
        readAPI(url = "https://api.simulationsoccer.com/player/getAllPlayers") %>% 
          select(
            name, pid, username, team, status_p
          ) %>% 
          future_promise()
      })
      
      historyTPE <- 
        reactive({
          req(input$selectedPlayer)
          pid <- input$selectedPlayer %>% as.numeric()
          
          getTpeHistory(pid)
        })
      
      historyUpdates <- 
        reactive({
          req(input$selectedPlayer)
          pid <- input$selectedPlayer %>% as.numeric()
          
          getUpdateHistory(pid)
        })
      
      historyBank <- 
        reactive({
          req(input$selectedPlayer)
          pid <- input$selectedPlayer %>% as.numeric()
          
          getBankTransactions(pid)
        })
      
      #### OUTPUTS ####
      output$selectPlayer <- renderUI({
        req(input$retired, input$freeAgent)
        allNames() %>% 
          then(
            onFulfilled = function(names) {
              names <- 
                names %>%
                filter(if(input$retired != 1) status_p > 0 else TRUE) %>%
                filter(if(input$freeAgent != 1) !(team %in% c("FA", "Retired")) else TRUE) %>% 
                arrange(name)
              
              namedVector <- names$pid
              
              names(namedVector) <- names$name
              
              selectInput(session$ns("selectedPlayer"), "Select Player", choices = namedVector)
            }
          )
      })
      
      output$playerFilter <- renderUI({
        tagList(
          column(
            width = 4,
            radioButtons(session$ns("retired"), label = "Include retired: ", choices = c("Yes" = 1, "No" = 0), inline = TRUE, selected = 0)  
          ),
          column(
            width = 4,
            radioButtons(session$ns("freeAgent"), label = "Include free agents: ", choices = c("Yes" = 1, "No" = 0), inline = TRUE)  
          )
        )
      })
      
      output$clubLogo <- renderImage({
        req(input$selectedPlayer)
        pid <- input$selectedPlayer %>% as.numeric()
        
        value <- playerData()
        
        list(
          src = normalizePath(file.path("./www", sprintf("%s.png", value$team))),
          width = 100,
          height = 100,
          alt = value$team
        )
      },
      deleteFile = FALSE)
      
      output$playerName <- renderUI({
        req(input$selectedPlayer)
        
        data <- playerData() 
        
        tagList(
          h2(data$name),
          h3(paste0("@", data$username))
        )
        
      })
      
      output$playerInfo <- renderUI({
        req(input$selectedPlayer)
        
        data <- playerData()
        
        bank <- readAPI("https://api.simulationsoccer.com/bank/getBankBalance", query = list(name = data$name))
        
        tagList(
          h4(paste("TPE: ", data$tpe)),
          h4(paste("Banked TPE: ", data$tpebank)),
          h4(paste("Bank Balance: ", bank$balance)),
          h4(paste("Player Status: "), data$playerStatus, class = data$playerStatus),
          h4(paste("User Status: "), data$userStatus, class = data$userStatus),
          h5(paste("Render: "), data$render)
        )
        
      })
      
      output$playerInfoExtra <- renderUI({
        req(input$selectedPlayer)
        
        data <- playerData() 
        
        value <- 
          data %>% select(contains("pos_")) %>% 
          pivot_longer(everything()) %>% 
          mutate(
            name = str_remove(name, pattern = "pos_") %>% str_to_upper()
          )
        
        tagList(
          h4("Traits"),
          data$traits %>% str_split(pattern = traitSep) %>% unlist() %>% paste(collapse = "<br>") %>% HTML(),
          br(),
          h4("Primary Position(s)"),
          value %>% filter(value == 20) %>% select(name) %>% unlist() %>% paste(collapse = ", ") %>% HTML(),
          h4("Secondary Position(s)"),
          value %>% filter(value < 20, value >= 10) %>% select(name)  %>% unlist() %>% paste(collapse = ", ") %>% HTML()
        )
        
      })
      
      output$tpeProgressPlot <- renderPlotly({
        req(input$selectedPlayer)
        
        pid <- input$selectedPlayer
        
        getTpeHistory(pid = pid) %>% 
          future_promise() %>% 
          then(
            onFulfilled = function(data){
              if(nrow(data) < 2){
                plot_ly() %>%
                  add_annotations(
                    text = "The player has had no TPE progression<br> in the Portal",
                    x = 0.5, y = 0.5,
                    xref = "paper", yref = "paper",
                    showarrow = FALSE,
                    font = list(size = 20),
                    align = "center",
                    borderpad = 10,
                    bgcolor = "rgba(255, 255, 255, 0.5)"
                  ) %>%
                  layout(
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
                    margin = list(l = 0, r = 0, b = 0, t = 0),
                    plot_bgcolor = "#333333",   # background color
                    paper_bgcolor = "#333333"
                  ) %>% 
                  plotly::config(
                    displayModeBar = TRUE,  # Enable display of mode bar (optional, true by default)
                    modeBarButtonsToRemove = list(
                      "toImage", "zoom2d", "pan2d", "select2d",
                      "lasso2d", "zoomIn2d", "zoomOut2d",
                      "autoScale2d", "resetScale2d"
                    ),
                    displaylogo = FALSE  # Remove Plotly logo
                  )
              } else {
                visData <- 
                  data %>% 
                  mutate(week = week(Time), year = year(Time)) %>% 
                  group_by(year, week) %>% 
                  summarize(total = sum(`TPE Change`, na.rm = TRUE)) %>% 
                  ungroup() %>% 
                  mutate(cumulative = cumsum(total), week = 1:n()) %>% 
                  suppressMessages()
                
                plot_ly(visData, hoverinfo = "text") %>% 
                  add_trace(x = ~week, y = ~cumulative, type = 'scatter', mode = 'markers+lines',
                            line = list(color = sslGold),
                            marker = list(size = 5, color = sslGold),
                            text = ~paste("Week:", week, "<br>TPE:", cumulative)
                          ) %>% 
                  layout(
                    title = list(
                      text = "TPE Progression",
                      font = list(color = "white")  # Set title text color to white
                    ),
                    xaxis = list(
                      title = "Time",
                      tickfont = list(color = "white"),  # Set x-axis tick labels color to white
                      titlefont = list(color = "white"),  # Set x-axis title color to white
                      dtick = 1,
                      showgrid = FALSE
                    ),
                    yaxis = list(
                      title = "TPE",
                      range = c(300, 2100),
                      tickfont = list(color = "white"),  # Set y-axis tick labels color to white
                      titlefont = list(color = "white"),  # Set y-axis title color to white
                      dtick = 200,  # Show tickmarks at intervals of 200
                      gridcolor = "rgba(255, 255, 255, 0.5)",  # Set gridline color to white with opacity
                      gridwidth = 1  # Set gridline width
                    ),
                    plot_bgcolor = "#333333",   # background color
                    paper_bgcolor = "#333333",   # plot area background color
                    showlegend = FALSE  # Hide legend (optional)
                  ) %>% 
                  plotly::config(
                    displayModeBar = TRUE,  # Enable display of mode bar (optional, true by default)
                    modeBarButtonsToRemove = list(
                      "zoom2d", "pan2d", "select2d",
                      "lasso2d", "zoomIn2d", "zoomOut2d",
                      "autoScale2d", "resetScale2d"
                    ),
                    displaylogo = FALSE  # Remove Plotly logo
                  )
              }
            }
          )
      })
      
      output$attributeTable <- renderUI({
        req(input$selectedPlayer)
        
        attributeReactable(playerData(), session, output)
      })
      
      output$matchStatsTable <- renderReactable({
        req(input$selectedPlayer)
        
        data <- playerData()
        
        if(data$pos_gk == 20){
          matches <- getKeeperMatchStats(data$name)
        } else {
          matches <- getOutfieldMatchStats(data$name)
        }
        
        matches %>% 
          then(
            onFulfilled = function(stats){
              stats %>% 
                leaderReactable()
            }
          )
      })
      
      output$historyTPE <- renderReactable({
        historyTPE() %>%
          mutate(
            Time = as_datetime(Time)
          ) %>% 
          reactable(
            columns = 
              list(
                Time = colDef(format = colFormat(datetime = TRUE))
              )
          )
        
      })
      
      output$historyUpdates <- renderReactable({
        historyUpdates() %>% 
          mutate(
            Time = as_datetime(Time)
          ) %>% 
          reactable(
            columns = 
              list(
                Time = colDef(format = colFormat(datetime = TRUE))
              )
          )
      })
      
      output$historyBank <- renderReactable({
        historyBank() %>% 
          then(
            onFulfilled = function(value){
              value %>% 
                mutate(
                  Time = as_datetime(Time)
                ) %>% 
                reactable(
                  columns = 
                    list(
                      Time = colDef(format = colFormat(datetime = TRUE)),
                      Transaction = colDef(format = colFormat(digits = 0, separators = TRUE, currency = "USD"))
                    )
                )
            },
            onRejected = NULL
          )
      })
    }
  )
}