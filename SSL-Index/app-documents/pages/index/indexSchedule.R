leagueScheduleUI <- function(id) {
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

leagueScheduleServer <- function(id) {
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
        if(input$selectedSeason != "ALL"){
          season <- input$selectedSeason %>% as.numeric()
          
          if(season < 5){
            selectInput(
              inputId = session$ns("selectedLeague"),
              label = "League",
              choices = 
                c(
                  "ALL",
                  "League" = "1",
                  "Cup"
                )
            )
          } else if (season == 12){
            selectInput(
              inputId = session$ns("selectedLeague"),
              label = "League",
              choices = 
                c(
                  "ALL",
                  "Major" = "1",
                  "Minor" = "2",
                  "Cup",
                  "WSFC"
                )
            )
          } else if (season < 12){
            selectInput(
              inputId = session$ns("selectedLeague"),
              label = "League",
              choices = 
                c(
                  "ALL",
                  "Division 1" = "1",
                  "Division 2" = "2",
                  "Cup"
                )
            )
          } else {
            selectInput(
              inputId = session$ns("selectedLeague"),
              label = "League",
              choices = 
                c(
                  "ALL",
                  "Major" = "1",
                  "Minor" = "2",
                  "Cup"
                )
            )
          }
        } else {
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "Major / Division 1" = "1",
                "Minor / Division 2" = "2",
                "Cup"
              )
          )
        }
      })
      
      output$standings <- renderUI({
        season <- input$selectedSeason
        league <- input$selectedLeague
        
        if(season == "ALL"){
          relegation <- FALSE
        } else if(season < 5 | season > 11){
          relegation <- FALSE
        } else {
          relegation <- TRUE
        } 
        
        standings() %>% 
          reactable(
            pagination = FALSE,
            fullWidth = FALSE,
            defaultColDef = colDef(
              maxWidth = 60,
              align = "center",
              style = function(value, index){
                list(
                  background = 
                    ifelse(index > 6 & relegation, "#e58e73", "white"),
                  # color = 
                  #   ifelse(index > 6, "white", "black"),
                  borderTop = 
                    ifelse(index == 7 & relegation, "solid", "none")
                )
              }
            )
          )
      })
      
    }
  )
}