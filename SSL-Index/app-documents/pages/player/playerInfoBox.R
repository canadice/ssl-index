playerInfoBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      fluidRow(
        column(
          width = 10,
          fluidRow(
            uiOutput(ns("name"))
          ),
          fluidRow(
            column(
              width = 5,
              uiOutput(ns("traits"))
            ),
            column(
              width = 5,
              uiOutput(ns("positions"))
            )
          )
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
        getPlayerName(pid = pid) %>% 
          then(
            onFulfilled = function(value){
              h2(value$name %>% paste(paste("(", value$class, ")", sep = ""), sep = ", "))
            },
            onRejected = function(error){
              print("something is wrong")
            }
          )
      })
      
      output$traits <- renderUI({
        getPlayerTraits(pid = pid) %>% 
          then(
            onFulfilled = function(value){
              tagList(
                h4("Player Traits"),
                if(value %>% length() == 0){
                  "No traits"
                } else {
                  paste(value, collapse = "<br>") %>% HTML()
                }
              )
            },
            onRejected = function(error){
              print("something is wrong")
            }
          )
      })
      
      output$positions <- renderUI({
        getPlayerPositions(pid = pid) %>% 
          then(
            onFulfilled = function(value){
              tagList(
                h4("Primary Position(s)"),
                value %>% filter(value == 20) %>% select(name) %>% unlist() %>% paste(collapse = ", ") %>% HTML(),
                h4("Secondary Position(s)"),
                value %>% filter(value < 20, value >= 10) %>% select(name)  %>% unlist() %>% paste(collapse = ", ") %>% HTML(),
              )
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
