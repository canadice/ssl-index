leagueStandingsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      ## First row
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("selectedSeason"),
            label = "Select a season",
            choices = 
              c(
                1:currentSeason$season %>% 
                sort(decreasing = TRUE),
                "ALL"
              )
          )
        ),
        column(
          width = 6
        ),
        column(
          width = 2,
          uiOutput(ns("leagueSelector"))
        )
      ),
      ## Second row
      fluidRow(
        h1("Standings"),
        uiOutput(ns("standings"))
      )
    ) # close fluidpage
  ) # close tagList
}

leagueStandingsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### DATA GENERATION ####
      standings <- reactive({
        req(input$selectedLeague)
        
        readAPI(url = "https://api.simulationsoccer.com/index/standings", 
                query = list(league = input$selectedLeague, season = input$selectedSeason)
        ) %>% 
          future_promise()
      })
      
      
      #### UI OUTPUT ####
      output$leagueSelector <- renderUI({
        season <- input$selectedSeason %>% as.numeric()
        
        if(season < 5){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "League" = "1",
                "Cup" = "0"
              )
          )
        } else if (season == 12){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "Major" = "1",
                "Minor" = "2",
                "Cup" = "0"
              )
          )
        } else if (season < 12){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "Division 1" = "1",
                "Division 2" = "2",
                "Cup" = "0"
              )
          )
        } else {
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "Major" = "1",
                "Minor" = "2",
                "Cup" = "0"
              )
          )
        }
      })
      
      output$standings <- renderUI({
        season <- input$selectedSeason
        league <- input$selectedLeague
        
        if(season == "ALL"){
          relegation <- FALSE
        } else if(season %>% as.numeric() < 5 | season %>% as.numeric() > 11){
          relegation <- FALSE
        } else {
          relegation <- TRUE
        } 
        
        
        standings() %>% 
          then(
            onFulfilled = function(data){
              if(data %>% is_empty()){
                NULL 
              } else {
                data %>% 
                  reactable(
                    pagination = FALSE,
                    defaultColDef = colDef(
                      minWidth = 60,
                      align = "center",
                      style = function(value, index){
                        list(
                          background = 
                            ifelse(index > 6 & relegation & league == 1, 
                                   red, 
                                   ifelse(index < 3 & relegation & league == 2, 
                                          green, 
                                          NA)
                            ),
                          # color = 
                          #   ifelse(index > 6, "white", "black"),
                          borderTop = 
                            ifelse((index == 7 & relegation & league == 1)|(index == 3 & relegation & league == 2), 
                                   "solid", 
                                   "none")
                        )
                      }
                    ),
                    columns = list(
                      Team = colDef(name = "", width = 200, align = "left", cell = function(value){
                        image <- img(src = sprintf("%s.png", value), style = "height: 30px;", alt = value, title = value)  
                        
                        list <- 
                          tagList(
                            div(
                              class = "tableClubName",
                              div(style = "display: inline-block; width: 30px;", image),
                              span(value)  
                            )
                          )
                      }),
                      MatchesPlayed = colDef(header = tippy("GP", "Games played", placement = "top", theme = "material")),
                      GoalsFor = colDef(header = tippy("GF", "Goals scored", placement = "top", theme = "material")),
                      GoalsAgainst = colDef(header = tippy("GA", "Goals conceded", placement = "top", theme = "material"))
                    )
                  )
              }
              
            }
          )
      })
      
    }
  )
}