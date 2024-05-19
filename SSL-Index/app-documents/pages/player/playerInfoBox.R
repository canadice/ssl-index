playerInfoBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      fluidRow(
        column(
          width = 10,
          uiOutput(ns("name"))
        ),
        column(
          width = 2,
          imageOutput(ns("team"), height = NULL)
        )  
      ) %>% 
        withSpinnerSmall()
    )
  )
}

playerInfoBoxServer <- function(id, pid) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$name <- renderUI({
        getPlayerName(pid) %>% 
          then(
            onFulfilled = function(value){
              h3(value$name)
            },
            onRejected = function(error){
              print("something is wrong")
            }
          )
      })
      
      output$team <- renderImage({
        getPlayerTeam(pid) %>%
          then(
            onFulfilled = function(value){
              list(
                src = normalizePath(file.path("./www", sprintf("%s.png", value$team))),
                width = 100,
                height = 100,
                alt = value$team
              )
            }
          )
      },
      deleteFile = FALSE)
    }
  )
}
