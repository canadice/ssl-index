box::use(
  dplyr,
  bslib,
  promises[future_promise, then],
  reactable[reactable, reactableOutput, renderReactable],
  shiny,
)

box::use(
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/constant,
  app/logic/db/api[readAPI],
  app/logic/ui/tags[flexRow, flexCol],
  app/logic/ui/selector[leagueSelectInput],
  app/logic/ui/reactableHelper[recordReactable, indexReactable],
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
                13:constant$currentSeason$season |> 
                  sort(decreasing = TRUE)
              )
          ),
          "",
          ""
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
        shiny$req(input$selectedSeason)
        season <- input$selectedSeason
        
        readAPI("https://api.simulationsoccer.com/index/academyOutfield", 
                query = list(season = season)) |> 
          future_promise()
      })
      
      keeperData <- shiny$reactive({
        shiny$req(input$selectedSeason)
        season <- input$selectedSeason
        
        readAPI("https://api.simulationsoccer.com/index/academyKeeper", 
                query = list(season = season)) |> 
          future_promise()
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
