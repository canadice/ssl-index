
box::use(
  bslib[card, card_header, card_body, card_footer, layout_columns],
  dplyr[select, if_else, mutate, arrange, slice_head, desc],
  promises[future_promise, then],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, config],
  reactable[reactableOutput, renderReactable, reactable, colDef],
  rlang[is_empty],
  shiny,
  stringr[str_replace_all],
  tippy[tippy],
)

box::use(
  app/logic/ui/tags[flexRow, flexCol],
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/db/api[readAPI],
  app/logic/constant,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    
    shiny$uiOutput(ns("information")),
    
    card(
      card_header(
        flexRow(
          shiny$tagList(
            shiny$tags$style("align-items: center; justify-content: space-between;"),
            shiny$uiOutput(ns("scheduleTitlePadder")),
            shiny$div("Latest Results", style = "width: 100%;"),
            shiny$div(
              shiny$uiOutput(ns("leagueSelector")),
              style = "font-size: 14px; font-weight: 400;"
            )
          )
      ),
      card_body(
        shiny$uiOutput(ns("schedule")) |> 
            withSpinnerCustom(height = 20)
        )
      ),
      min_height = "200px"
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(
          shiny$h5("Major League Standings")
        ),
        card_body(
          reactableOutput(ns("standings_1")) |> 
            withSpinnerCustom(height = 20)
        )
      ),
      card(
        card_header(
          shiny$h5("Minor League Standings")
        ),
        card_body(
          reactableOutput(ns("standings_2")) |> 
            withSpinnerCustom(height = 20)
        )
      )
    ),
    layout_columns(
      col_widths = c(6,6,12),
      card(
        card_header(
          shiny$h4("Weekly top earners")
        ),
        card_body(
          reactableOutput(ns("weeklyLeaders")) |> 
            withSpinnerCustom(height = 40)
        )
      ),
      card(
        card_header(
          shiny$h4("Recent creates")
        ),
        card_body(
          reactableOutput(ns("created")) |> 
            withSpinnerCustom(height = 40)
        )
      ),
      card(
        card_header(
          shiny$h4("Activity Checks")
        ),
        card_body(
          plotlyOutput(ns("activityChecks")) |> 
            withSpinnerCustom(height = 40) |> 
            shiny$div(class = "plotlyBorder"))
        )
      )
    )
}

#' @export
server <- function(id, usergroup) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      
      #### INFORMATION ####
      output$information <- shiny$renderUI({
        if(any(5 %in% usergroup)){
          card(
            card_header(
              "Information"
            ),
            card_body(
              h2("Your account needs to be activated in order to access the rest of the portal functions. Please check the e-mail used when registering on the SSL forums.") |> 
                div(class = "Retired")
            )
          )
        }
      })
      
      
      #### Latest league standings ####
      lapply(1:2,
             FUN = function(division){
               output[[paste0("standings_", division)]] <- renderReactable({
                 standings <- readAPI(url = "https://api.simulationsoccer.com/index/standings", query = list(league = division, season = constant$currentSeason$season))
                 
                 if(!(standings |> is_empty())){
                   standings |> 
                     select(
                       Team, 
                       Wins:Losses,
                       Points
                     ) |> 
                     reactable(
                       defaultColDef = colDef(minWidth = 30),
                       columns = 
                         list(
                           Team = colDef(
                             minWidth = 100,
                             cell = function(value){
                               image <- shiny$img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value, title = value)  
                               
                               list <-
                                 shiny$tagList(
                                   flexRow(style = "align-items: center; gap: 8px;", shiny$tagList(
                                     image,
                                     shiny$span(class = "truncated-text", value)
                                   ))
                                 )
                             }
                           ),
                           Wins = colDef(header = tippy("W", "Wins", placement = "top", theme = "ssl", arrow = TRUE)),
                           Draws = colDef(header = tippy("D", "Draws", placement = "top", theme = "ssl", arrow = TRUE)),
                           Losses = colDef(header = tippy("L", "Losses", placement = "top", theme = "ssl", arrow = TRUE)),
                           Points = colDef(header = tippy("P", "Points", placement = "top", theme = "ssl", arrow = TRUE))
                         )
                     )
                 }
               })
             })
      
      #### Latest results ####
      schedule <- shiny$reactive({
        shiny$req(input$selectedLeague)
        readAPI(url = "https://api.simulationsoccer.com/index/schedule", 
                    query = list(league = input$selectedLeague, season = constant$currentSeason$season)
        )
      })
      
      # Empty element with width matching selector to make spacing of title elements easier
      output$scheduleTitlePadder <- shiny$renderUI({
        shiny$div(style = "width: 150px;", class = "hide-in-mobile")
      })
      
      output$leagueSelector <- shiny$renderUI({
        shiny$div(
          shiny$selectInput(
            inputId = session$ns("selectedLeague"),
            label = NULL,
            choices = 
              c(
                "All Leagues" = "ALL",
                "Major" = "1",
                "Minor" = "2",
                "Cup" = "0"
              ),
            width = "150px"
          )
        )
      })
      
      output$schedule <- shiny$renderUI({
        league <- input$selectedLeague
        
        if(schedule() |> is_empty()){
          "No schedule is available yet"
        } else {
          schedule <- schedule()
          
          shiny$tagList(
            shiny$div(
              class = "results",
              id = "results-scroll",
              lapply(1:nrow(schedule),
                     function(i){
                       card(
                         card_header(
                           shiny$div(
                             shiny$div(style = "display: inline-block; width: 40px;", shiny$img(src = sprintf("%s.png", schedule[i, "Home"]), style = "height: 40px;", alt = schedule[i, "Home"], title = schedule[i, "Home"])), 
                             shiny$strong(" - "), 
                             shiny$div(style = "display: inline-block; width: 40px;", shiny$img(src = sprintf("%s.png", schedule[i, "Away"]), style = "height: 40px;", alt = schedule[i, "Away"], title = schedule[i, "Away"])),
                             align = "center"
                           )
                         ),
                         card_body(
                           shiny$h4(paste(schedule[i, "HomeScore"], schedule[i, "AwayScore"], sep = "-") |> 
                                str_replace_all(pattern = "NA", replacement = " ")
                           )
                         ),
                         card_footer(
                           paste(
                             paste(
                               if_else(schedule[i, "MatchType"] == 0, 
                                       "Cup",
                                       if_else(schedule[i, "MatchType"] == 1, 
                                               "Major League",
                                               if_else(schedule[i, "MatchType"] == 2, "Minor League", 
                                                       if_else(schedule[i, "MatchType"] == 5, "WSFC","Friendlies")))),
                               schedule[i, "MatchDay"], sep = ", "
                             ),
                             paste(
                               schedule[i, "IRLDate"]
                             ),
                             sep = "<br>"
                           ) |> 
                             shiny$HTML() |> 
                             shiny$div(align = "center")
                         )
                       )
                     })
            ),
            shiny$tags$script(shiny$HTML("
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
        data <- 
          readAPI(url = "https://api.simulationsoccer.com/player/topEarners") |> 
          future_promise()
        
        data |> 
          then(
            onFulfilled = function(data){
              data |> 
                reactable(
                  defaultColDef = colDef(minWidth = 75)
                )
            }
          )
      })
      
      #### Recently created ####
      output$created <- renderReactable({
        data <- 
          readAPI("https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) |> 
          arrange(created |> desc()) |> 
          slice_head(n = 5) |> 
          select(Pos = position, Name = name, Username = username) |> 
          future_promise()
        
        
        data |> 
          then(
            onFulfilled = function(data){
              data |> 
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
        readAPI("https://api.simulationsoccer.com/player/acHistory") |> 
          mutate(weekYear = paste(paste0("W", nweeks))) |> 
          plot_ly(x = ~weekYear, y= ~count, type = "scatter", mode = "lines+markers",
                  hoverinfo = "text",
                  line = list(color = constant$sslGold),
                  marker = list(size = 5, color = constant$sslGold),
                  text = ~paste("#AC: ", count)
          ) |> 
          layout(
            xaxis = list(
              title = "Time",
              tickfont = list(color = "white"),  # Set x-axis tick labels color to white
              titlefont = list(color = "white"),  # Set x-axis title color to white
              dtick = 2,
              showgrid = FALSE
            ),
            yaxis = list(
              title = "#ACs",
              range = c(0, 220),
              tickfont = list(color = "white"),  # Set y-axis tick labels color to white
              titlefont = list(color = "white"),  # Set y-axis title color to white
              dtick = 20,  # Show tickmarks at intervals of 200
              gridcolor = "rgba(255, 255, 255, 0.5)",  # Set gridline color to white with opacity
              gridwidth = 1  # Set gridline width
            ),
            plot_bgcolor = "#333333",   # background color
            paper_bgcolor = "#333333",   # plot area background color
            showlegend = FALSE  # Hide legend (optional)
          ) |> 
          config(
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