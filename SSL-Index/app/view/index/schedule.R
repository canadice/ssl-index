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
                  sort(decreasing = TRUE)
              )
          ),
          "",
          shiny$uiOutput(ns("leagueSelector")) |> 
            withSpinnerCustom(height = 20)
        )
      ),
      bslib$card_body(
        shiny$h1("Schedule"),
        shiny$uiOutput(ns("schedule")) |> 
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
      schedule <- shiny$reactive({
        shiny$req(input$selectedLeague)
        
        readAPI(url = "https://api.simulationsoccer.com/index/schedule", 
                query = list(league = input$selectedLeague, season = input$selectedSeason)
        ) |> 
          future_promise()
      })
      
      
      #### UI OUTPUT ####
      output$leagueSelector <- shiny$renderUI({
        leagueSelectInput(season = input$selectedSeason, session = session)
      })
      
      output$schedule <- shiny$renderUI({
        season <- input$selectedSeason
        league <- input$selectedLeague
        
        schedule() |> 
          then(
            onFulfilled = function(data){
              if(data |> is_empty()){
                NULL 
              } else {
                data |> 
                  dplyr$mutate(
                    HomeScore = if (!"HomeScore" %in% names(data)) NA_character_ else HomeScore,
                    AwayScore = if (!"AwayScore" %in% names(data)) NA_character_ else AwayScore,
                    Penalties = if (!"Penalties" %in% names(data)) NA_character_ else Penalties,
                    ExtraTime = if (!"ExtraTime" %in% names(data)) NA_character_ else ExtraTime
                  ) |>
                  dplyr$rename(
                    Date = IRLDate
                  ) |> 
                  dplyr$mutate(
                    dplyr$across(
                      c(HomeScore, AwayScore),
                      function(x) ifelse(is.na(x), " ", x)
                    ),
                    Score = dplyr$case_when(
                      Penalties == 1 & HomeScore > AwayScore ~ paste0("p", paste(HomeScore, AwayScore, sep = " - ")),
                      Penalties == 1 & HomeScore < AwayScore ~ paste0(paste(HomeScore, AwayScore, sep = " - "), "p"),
                      ExtraTime == 1 & HomeScore > AwayScore ~ paste0("e", paste(HomeScore, AwayScore, sep = " - ")),
                      ExtraTime == 1 & HomeScore < AwayScore ~ paste0(paste(HomeScore, AwayScore, sep = " - "), "p"),
                      TRUE ~ paste(HomeScore, AwayScore, sep = " - ")
                    ),
                    MatchType = dplyr$case_when(
                      MatchType == -1 ~ "Friendlies",
                      MatchType == 0 ~ "Cup",
                      MatchType == 1 ~ "Major League",
                      TRUE ~ "Minor League"
                    )
                  ) |> 
                  dplyr$select(!c(HomeScore, AwayScore, ExtraTime, Penalties)) |> 
                  reactable(
                    pagination = FALSE,
                    searchable = TRUE,
                    columns = 
                      list(
                        Date = colDef(width = 100),
                        MatchType = colDef(width = 100),
                        MatchDay = colDef(width = 100),
                        Home = 
                          colDef(
                            cell = function(value){
                              image <- shiny$img(src = sprintf("static/logo/%s.png", value), style = "height: 30px;", title = value)
                              
                              shiny$tagList(
                                shiny$div(style = "display: inline-block; width: 30px;", image),
                                shiny$div(style = "font-size: 1.2rem", value)
                              )
                            }
                          ),
                        Away = 
                          colDef(
                            cell = function(value){
                              image <- shiny$img(src = sprintf("static/logo/%s.png", value), style = "height: 30px;", title = value)
                              
                              shiny$tagList(
                                shiny$div(style = "display: inline-block; width: 30px;", image),
                                shiny$div(style = "font-size: 1.2rem", value)
                              )
                            }
                          )
                      )
                  )
              }
            }
          )
        
      })
      
    }
  )
}
