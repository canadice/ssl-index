box::use(
  bslib,
  dplyr,
  promises[future_promise, then],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  shiny,
  stringr[str_to_upper],
  tippy[tippy],
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
          colwidths = c(2, 10)
        ),
        shiny$selectInput(
          inputId = ns("selectedClass"),
          label = "Select a class",
          choices = 
            c(
              1:(constant$currentSeason$season + 1) |> 
                sort(decreasing = TRUE)
            )
        ),
        ""
      ),
      bslib$card_body(
        shiny$h1("Draft Class Tracker"),
        reactableOutput(ns("tracker")) |> 
          withSpinnerCustom(height = 400)
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    
    ### Data
    draftclass <- shiny$reactive({
      shiny$req(input$selectedClass)
      readAPI(url = "https://api.simulationsoccer.com/player/getDraftClass", 
              query = list(class = input$selectedClass)
        ) |> 
        future_promise()
    }) |> 
      shiny$bindEvent(input$selectedClass)
    
    ### Output
    output$tracker <- renderReactable({
      draftclass() |> 
        then(
          onFulfilled = function(data){
            data |> 
              dplyr$rename_with(str_to_upper) |> 
              dplyr$relocate(TEAM, POSITION) |> 
              reactable(
                pagination = FALSE,
                columns = list(
                  POSITION = colDef(name = "POS", width = 50),
                  USERSTATUS = colDef(width = 150),
                  PLAYERSTATUS = colDef(width = 150),
                  TPE = colDef(width = 50),
                  NAME = colDef(cell = function(value) tippy(value, tooltip = value, theme = "ssl")),
                  USERNAME = colDef(cell = function(value) tippy(value, tooltip = value, theme = "ssl")),
                  BANKBALANCE = colDef(width = 120, format = colFormat(digits = 0, separators = TRUE, currency = "USD")),
                  TEAM = colDef(
                    name = "", 
                    width = 200, 
                    align = "left", 
                    cell = function(value){
                      image <- shiny$img(src = sprintf("static/logo/%s.png", value), style = "height: 30px;", alt = value, title = value)  
                      list <- 
                        shiny$tagList(
                          shiny$div(
                            class = "tableClubName",
                            shiny$div(style = "display: inline-block; width: 30px;", image),
                            shiny$span(value)  
                          )
                        )
                    })
                )
              )
          },
          onRejected = function(err) {
            # Show error notification and return empty content
            shiny$showNotification("Failed to load draft class data.", type = "error")
            NULL
          }
        )
    })
    
  })
}
