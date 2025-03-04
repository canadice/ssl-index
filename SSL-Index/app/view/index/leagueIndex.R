box::use(
  bslib,
  dplyr,
  promises[future_promise, then],
  reactable[reactableOutput, renderReactable],
  shiny,
)

box::use(
  app/logic/constant,
  app/logic/db/api[readAPI],
  app/logic/ui/reactableHelper[recordReactable, indexReactable],
  app/logic/ui/selector[leagueSelectInput],
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/ui/tags[flexCol, flexRow],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_columns(
          colwidths = c(4, 6, 2),
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
          shiny$uiOutput(ns("leagueSelector"))
        )
      ),
      bslib$card_body(
        shiny$tabsetPanel(
          header = shiny$h1("Outfield"),
          shiny$tabPanel("Statistics",
                         reactableOutput(ns("outfieldBasic")) |> 
                           withSpinnerCustom(height = 80)),
          shiny$tabPanel("Adv. Statistics",
                         reactableOutput(ns("outfieldAdvanced")) |> 
                           withSpinnerCustom(height = 80)),
          shiny$tabPanel("Leaders",
                         shiny$uiOutput(ns("outfieldLeaders")) |> 
                           withSpinnerCustom(height = 80))
        ),
        shiny$tabsetPanel(
          header = shiny$h1("Keeper"),
          shiny$tabPanel("Statistics",
                         reactableOutput(ns("keeperBasic")) |> 
                           withSpinnerCustom(height = 80)),
          shiny$tabPanel("Adv. Statistics",
                         reactableOutput(ns("keeperAdvanced")) |> 
                           withSpinnerCustom(height = 80)),
          shiny$tabPanel("Leaders",
                         shiny$uiOutput(ns("keeperLeaders")) |> 
                           withSpinnerCustom(height = 80))
        )
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
      outfieldData <- shiny$reactive({
        shiny$req(input$selectedLeague)
        
        readAPI(url = "https://api.simulationsoccer.com/index/outfield", 
                query = list(league = input$selectedLeague, season = input$selectedSeason)
        ) |> 
          future_promise()
      })
      
      keeperData <- shiny$reactive({
        shiny$req(input$selectedLeague)
        
        readAPI(url = "https://api.simulationsoccer.com/index/keeper", 
                query = list(league = input$selectedLeague, season = input$selectedSeason)
        ) |> 
          future_promise()
      })
      
      #### UI OUTPUT ####
      output$leagueSelector <- shiny$renderUI({
        leagueSelectInput(season = input$selectedSeason, session = session)
      })
      
      outstatistics <- c("goals", "assists", "player of the match", "distance run (km)", "successful passes", "chances created", "tackles won", "interceptions", "yellow cards", "red cards")
      
      output$outfieldLeaders <- shiny$renderUI({
        # Split statistics into threes
        statisticThrees <- split(outstatistics, (seq_along(outstatistics) - 1) %/% 3)
        
        # Create fluidFlexes for each table
        lapply(statisticThrees, function(table) {
          flexRow(
            lapply(table, function(stat) {
              flexCol(
                reactableOutput(session$ns(paste0(stat, "_leader"))) |>
                  shiny$div(class = "leaderTable")
              )
            })
          )
        })
      })
      
      lapply(outstatistics, function(stat){
        output[[paste0(stat, "_leader")]] <- renderReactable({
          outfieldData() |> 
            then(
              onFulfilled = function(data){
                data |> 
                  dplyr$select(
                    name, club, dplyr$all_of(stat)
                  ) |> 
                  dplyr$arrange(
                    dplyr$across(
                      dplyr$starts_with(stat),
                      dplyr$desc
                    )
                  ) |> 
                  dplyr$slice_head(n = 10) |> 
                  recordReactable()
              }
            )
        })
      })
      
      keepstatistics <- c("won", "clean sheets", "conceded", "save%")
      
      output$keeperLeaders <- shiny$renderUI({
        # Split statistics into threes
        statisticThrees <- split(keepstatistics, (seq_along(keepstatistics) - 1) %/% 3)
        
        # Create fluidRows for each pair
        lapply(statisticThrees, function(table) {
          flexRow(
            lapply(table, function(stat) {
              flexCol(
                reactableOutput(session$ns(paste0(stat, "_leader"))) |>
                  shiny$div(class = "leaderTable")
              )
            })
          )
        })
      })
      
      lapply(keepstatistics, function(stat){
        output[[paste0(stat, "_leader")]] <- renderReactable({
          keeperData() |> 
            then(
              onFulfilled = function(data){
                data |> 
                  dplyr$select(
                    name, club, dplyr$all_of(stat)
                  ) |> 
                  dplyr$arrange(
                    dplyr$across(
                      dplyr$starts_with(stat),
                      dplyr$desc
                    )
                  ) |> 
                  dplyr$slice_head(n = 10) |>
                  recordReactable()
              }
            )
        })
      })
      
      #### REACTABLE OUTPUT ####
      output$outfieldBasic <- renderReactable({
        outfieldData() |> 
          then(
            onFulfilled = function(data){
              currentData <- 
                data |> 
                dplyr$select(
                  name:assists, `shots on target`:offsides, blocks, `shots blocked`, `average rating`
                ) 
              
              currentData |> 
                indexReactable()
            }
          )
          
      })  
      
      output$outfieldAdvanced <- renderReactable({
        outfieldData() |> 
          then(
            onFulfilled = function(data){
              currentData <- 
                data |> 
                dplyr$select(
                  name:club, 
                  xg,
                  xa:`fk shots`,
                  `open play key passes`:`goals outside box`,
                  `press%`:`pen adj xG`
                ) 
              
              currentData |> 
                indexReactable()
            }
          )
        
      }) 
      
      output$keeperBasic <- renderReactable({
        keeperData() |> 
          then(
            onFulfilled = function(data){
              currentData <- 
                data |> 
                dplyr$select(
                  name:`save%`
                ) 
              
              currentData |> 
                indexReactable()
            }
          )
        
      })  
      
      output$keeperAdvanced <- renderReactable({
        keeperData() |> 
          then(
            onFulfilled = function(data){
              currentData <- 
                data |> 
                dplyr$select(
                  name:club, 
                  `penalties faced`:`xg prevented`
                ) 
              
              currentData |> 
                indexReactable()
            }
          )
        
      }) 
      
    }
  )
}