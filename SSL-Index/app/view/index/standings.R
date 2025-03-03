box::use(
  dplyr,
  bslib,
  promises[future_promise, then],
  reactable[reactable, reactableOutput, renderReactable, colDef],
  shiny,
  rlang[is_empty],
  tippy[tippy],
)

box::use(
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/constant,
  app/logic/db/api[readAPI],
  app/logic/ui/tags[flexRow, flexCol],
  app/logic/ui/selector[leagueSelectInput],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_columns(
          colwidths = c(2, 8, 2),
          shiny$selectInput(
            inputId = ns("selectedSeason"),
            label = "Select a season",
            choices = 
              c(
                1:constant$currentSeason$season |> 
                  sort(decreasing = TRUE),
                "ALL"
              )
          ),
          "",
          shiny$uiOutput(ns("leagueSelector")) |> 
            withSpinnerCustom(height = 20)
        )
      ),
      bslib$card_body(
        shiny$h1("Standings"),
        shiny$uiOutput(ns("standings")) |> 
          withSpinnerCustom(height = 80)
      )
    ) 
  ) 
}

#' @export
server <- function(id) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      
      #### DATA GENERATION ####
      standings <- shiny$reactive({
        shiny$req(input$selectedLeague)
        
        readAPI(url = "https://api.simulationsoccer.com/index/standings", 
                query = list(league = input$selectedLeague, season = input$selectedSeason)
        ) |> 
          future_promise()
      })
      
      
      #### UI OUTPUT ####
      output$leagueSelector <- shiny$renderUI({
        leagueSelectInput(season = input$selectedSeason, session = session)
      })
      
      output$standings <- shiny$renderUI({
        season <- input$selectedSeason
        league <- input$selectedLeague
        
        if(season == "ALL"){
          relegation <- FALSE
        } else if(season |> as.numeric() < 5 | season |> as.numeric() > 11){
          relegation <- FALSE
        } else {
          relegation <- TRUE
        } 
        
        standings() |> 
          then(
            onFulfilled = function(data){
              if(data |> is_empty()){
                NULL 
              } else {
                data |> 
                  dplyr$select(!GoalDifference) |> 
                  reactable(
                    pagination = FALSE,
                    defaultColDef = colDef(
                      minWidth = 60,
                      align = "center",
                      style = function(value, index){
                        list(
                          background = 
                            dplyr$if_else(index > 6 & relegation & league == 1, 
                                  constant$red, 
                                  dplyr$if_else(index < 3 & relegation & league == 2, 
                                          constant$green, 
                                          NA)
                                  ),
                          # color = 
                          #   ifelse(index > 6, "white", "black"),
                          borderTop = 
                            dplyr$if_else((index == 7 & relegation & league == 1)|(index == 3 & relegation & league == 2), 
                                  "solid", 
                                  "none")
                        )
                      }
                    ),
                    columns = list(
                      Team = colDef(name = "", width = 200, align = "left", cell = function(value){
                        image <- shiny$img(src = sprintf("static/logo/%s.png", value), style = "height: 30px;", alt = value, title = value)  
                        
                        list <- 
                          shiny$tagList(
                            flexRow(style = "align-items: center; gap: 8px;", 
                                    shiny$tagList(
                                      image,
                                      shiny$span(class = "truncated-text", value)
                                    )
                                  )
                          )
                      }),
                      MatchesPlayed = colDef(header = tippy("GP", "Games played", theme = "ssl")),
                      Wins = colDef(header = tippy("W", "Wins", theme = "ssl")),
                      Draws = colDef(header = tippy("D", "Draws", theme = "ssl")),
                      Losses = colDef(header = tippy("L", "Losses", theme = "ssl")),
                      GoalsFor = colDef(header = tippy("GF", "Goals scored", theme = "ssl")),
                      GoalsAgainst = colDef(header = tippy("GA", "Goals conceded", theme = "ssl")),
                      Points = colDef(header = tippy("P", "Points", theme = "ssl"))
                    )
                  )
              }
              
            }
          )
      })
      
    }
  )
}