playerAttributeBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      textOutput(ns("name")) 
    ),
    fluidRow(
      imageOutput(ns("team")) 
    )
  )
}

playerAttributeBoxServer <- function(id, pid) {
  moduleServer(
    id,
    function(input, output, session) {
      output$name <- renderText({
        getPlayerName(pid) %>% 
          then(
            onFulfilled = function(value){
              value$name
            }
          )
      })
      
      output$team <- renderImage({
        getPlayerTeam(pid) %>% 
          then(
            onFulfilled = function(value){
              # value$team %>% paste(".png", sep = "")
              img(src = sprintf("%s.png", value$team))
            }
          )
      }, 
      deleteFile = TRUE)
    }
  )
}