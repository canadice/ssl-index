draftClassUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      ## First row
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("selectedClass"),
            label = "Select a class",
            choices = 
              c(
                1:(currentSeason$season + 1) |> 
                sort(decreasing = TRUE)
              )
          )
        ),
        column(
          width = 6
        ),
        column(
          width = 2#,
          # uiOutput(ns("leagueSelector"))
        )
      ),
      ## Second row
      fluidRow(
        h2("Draft Class Tracker"),
        uiOutput(ns("tracker")) |> 
          withSpinnerMedium()
      )
    ) # close fluidpage
  ) # close tagList
}

draftClassServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### DATA GENERATION ####
      draftclass <- reactive({
        readAPI(url = "https://api.simulationsoccer.com/player/getDraftClass", 
                query = list(class = input$selectedClass)
        ) |> 
          future_promise()
      })
      
      
      output$tracker <- renderUI({
        draftclass() |> 
          then(
            onFulfilled = function(data){
              data |> 
                rename_with(str_to_upper) |> 
                relocate(TEAM, POSITION) |> 
                reactable(
                  pagination = FALSE,
                  columns = list(
                    POSITION = colDef(name = "POS", width = 50),
                    USERSTATUS = colDef(width = 150),
                    PLAYERSTATUS = colDef(width = 150),
                    TPE = colDef(width = 50),
                    NAME = colDef(cell = function(value) tippy(value, tooltip = value, theme = "ssl", arrow = TRUE)),
                    USERNAME = colDef(cell = function(value) tippy(value, tooltip = value, theme = "ssl", arrow = TRUE)),
                    BANKBALANCE = colDef(width = 120, format = colFormat(digits = 0, separators = TRUE, currency = "USD")),
                    TEAM = colDef(name = "", width = 200, align = "left", cell = function(value){
                      image <- img(src = sprintf("%s.png", value), style = "height: 30px;", alt = value, title = value)  
                      
                      list <- 
                        tagList(
                          div(
                            class = "tableClubName",
                            div(style = "display: inline-block; width: 30px;", image),
                            span(value)  
                          )
                        )
                    })
                  )
              )
            }
          )
      })
      
    }
  )
}
