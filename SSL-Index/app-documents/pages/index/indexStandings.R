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
        )
      })
      
      
      #### UI OUTPUT ####
      output$leagueSelector <- renderUI({
        if(input$selectedSeason != "ALL"){
          season <- input$selectedSeason %>% as.numeric()
          
          if(season < 5){
            selectInput(
              inputId = session$ns("selectedLeague"),
              label = "League",
              choices = 
                c(
                  "League" = "1",
                  "Cup",
                  "ALL"
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
                  "Cup",
                  "WSFC",
                  "ALL"
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
                  "Cup",
                  "ALL"
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
                  "Cup",
                  "ALL"
                )
            )
          }
        } else {
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "Major / Division 1" = "1",
                "Minor / Division 2" = "2",
                "Cup",
                "ALL"
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
              data %>% 
                reactable(
                  pagination = FALSE,
                  defaultColDef = colDef(
                    minWidth = 60,
                    align = "center",
                    style = function(value, index){
                      list(
                        background = 
                          ifelse(index > 6 & relegation, "#e58e73", NA),
                        # color = 
                        #   ifelse(index > 6, "white", "black"),
                        borderTop = 
                          ifelse(index == 7 & relegation, "solid", "none")
                      )
                    }
                  ),
                  columns = list(
                    Team = colDef(name = "", width = 200, align = "left", cell = function(value){
                      image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value, title = value)  
                      
                      list <- 
                        tagList(
                          div(
                            class = "tableClubName",
                            div(style = "display: inline-block; width: 25px;", image),
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
          )
      })
      
    }
  )
}