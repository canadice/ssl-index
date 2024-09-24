welcomeUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("information")),
    
    box(title = "Latest Results", width = NULL,
        uiOutput(ns("schedule")) %>% 
          withSpinnerSmall()
        ) %>% 
      column(width = 12),
    
    box(title = "News", width = NULL,
        column(
          width = 6,
          h4("Weekly top earners"),
          reactableOutput(ns("weeklyLeaders")) %>% 
            withSpinnerMedium()
        ),
        column(
          width = 6,
          h4("Recent creates"),
          reactableOutput(ns("created")) %>% 
            withSpinnerMedium()
        ),
        column(width = 12,
               h4("Activity Checks"),
               plotlyOutput(ns("activityChecks")) %>% 
                 withSpinnerMedium() %>% 
                 div(class = "plotlyBorder"))
      ) %>% 
      column(width = 8),
    
    box(title = "Current Standings", width = NULL,
        h5("Major League"),
        reactableOutput(ns("standings_1")) %>% 
          withSpinnerSmall(),
        h5("Minor League"),
        reactableOutput(ns("standings_2")) %>% 
          withSpinnerSmall()
        ) %>% 
      column(width = 4)
    
  )
}

welcomeServer <- function(id, usergroup) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### INFORMATION ####
      output$information <- renderUI({
        if(any(5 %in% usergroup)){
          box(title = "Information", width = NULL, collapsible = TRUE,
              h2("Your account needs to be activated in order to access the rest of the portal functions. Please check the e-mail used when registering on the SSL forums.") %>% 
                div(class = "Retired")
          ) %>% 
            column(width = 12)
        }
      })
      
      
      #### Latest league standings ####
      lapply(1:2,
             FUN = function(division){
               output[[paste0("standings_", division)]] <- renderReactable({
                 getStandings(division, season = currentSeason$season) %>% 
                   select(
                     Team, 
                     Wins:Losses,
                     Points
                   ) %>% 
                   reactable(
                     defaultColDef = colDef(minWidth = 50),
                     columns = 
                       list(
                         Team = colDef(
                           # width = 25,
                           cell = function(value){
                             image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value, title = value)  
                             
                             list <- 
                               tagList(
                                 div(style = "display: inline-block; width: 25px;", image)
                               )
                           }
                         )
                       )
                   )
               })
             })
      
      
      #### Latest results ####
      output$schedule <- renderUI({
        schedule <- readAPI(url = "https://api.simulationsoccer.com/index/schedule", query = list(season = currentSeason$season))
        
        if(length(schedule) == 0){
          "No schedule is available yet"
        } else {
          tagList(
            div(
              class = "results",
              id = "results-scroll",
              lapply(1:nrow(schedule),
                     function(i){
                       box(
                         title = div(
                           div(style = "display: inline-block; width: 40px;", img(src = sprintf("%s.png", schedule[i, "Home"]), style = "height: 40px;", alt = schedule[i, "Home"], title = schedule[i, "Home"])), 
                           strong(" - "), 
                           div(style = "display: inline-block; width: 40px;", img(src = sprintf("%s.png", schedule[i, "Away"]), style = "height: 40px;", alt = schedule[i, "Away"], title = schedule[i, "Away"])),
                           align = "center"
                         ),
                         width = NULL,
                         status = "primary",
                         h4(paste(schedule[i, "HomeScore"], schedule[i, "AwayScore"], sep = "-") %>% 
                              str_replace_all(pattern = "NA", replacement = " ")
                         ),
                         footer = 
                           paste(
                             paste(
                               if_else(schedule[i, "MatchType"] == 0, 
                                       "Cup",
                                       if_else(schedule[i, "MatchType"] == 1, 
                                               "Major League",
                                               if_else(schedule[i, "MatchType"] == 2, "Minor League", "Pre-Season"))),
                               schedule[i, "MatchDay"], sep = ", "
                             ),
                             paste(
                               schedule[i, "IRLDate"]
                             ),
                             sep = "<br>"
                           ) %>% 
                           HTML() %>% 
                           div(align = "center")
                       )
                       
                     })
            ),
            tags$script(HTML("
            $(document).ready(function() {
              var div = document.getElementById('results-scroll');
              var width = 0;
              for (var i = 0; i < div.children.length; i++) {
                var score = $(div.children[i]).find('h4').text().trim();
                if (!score.match(/^\\d+-\\d+$/)) {
                  width = div.children[i].clientWidth * (i-6);
                  break;
                } else {
                  width = div.children[i].clientWidth * i
                }
              }
              div.scrollLeft = width;
            });
          "))
          )
        }
        
        
        
      })
      #### Weekly TPE Leaders ####
      output$weeklyLeaders <- renderReactable({
        data <- topEarners()
        
        data %>% 
          then(
            onFulfilled = function(data){
              data %>% 
                reactable(
                  defaultColDef = colDef(minWidth = 75)
                )
            }
          )
      })
      
      #### Recently created ####
      output$created <- renderReactable({
        data <- getRecentCreates()
        
        data %>% 
          then(
            onFulfilled = function(data){
              data %>% 
                reactable(
                  defaultColDef = colDef(minWidth = 25),
                  columns = list(
                    Pos = colDef(maxWidth = 50)
                  )
                )
            }
          )
      })
      
      output$activityChecks <- renderPlotly({
        readAPI("https://api.simulationsoccer.com/player/acHistory") %>% 
          mutate(weekYear = paste(paste0("W", week), year, sep ="\n")) %>% 
          plot_ly(x = ~weekYear, y= ~count, type = "scatter", mode = "lines+markers",
                  hoverinfo = "text",
                  line = list(color = sslGold),
                  marker = list(size = 5, color = sslGold),
                  text = ~paste("#AC: ", count)
                  ) %>% 
          layout(
            xaxis = list(
              title = "Time",
              tickfont = list(color = "white"),  # Set x-axis tick labels color to white
              titlefont = list(color = "white"),  # Set x-axis title color to white
              dtick = 1,
              showgrid = FALSE
            ),
            yaxis = list(
              title = "#ACs",
              range = c(0, 150),
              tickfont = list(color = "white"),  # Set y-axis tick labels color to white
              titlefont = list(color = "white"),  # Set y-axis title color to white
              dtick = 20,  # Show tickmarks at intervals of 200
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
      })
    }
  )
}