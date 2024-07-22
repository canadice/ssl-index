bodTeamUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(title = "Organizational Overview", width = NULL, solidHeader = TRUE,
                 reactableOutput(ns("teamOverview")) %>% 
                   withSpinnerMedium()
             )
             
      )
    )
  )
}

bodTeamServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      playerData <- reactive({
        getPlayersFromAllTeams()
      }) 
      
      output$teamOverview <- renderReactable({
         playerData() %>% 
          then(
            onFulfilled = function(value){
              value %>% 
                select(!c(pid, `left foot`, `right foot`)) %>% 
                arrange(
                  team,
                  affiliate,
                  tpe %>% desc()
                ) %>% 
                reactable(
                  # groupBy = c("team"),
                  pagination = FALSE,
                  rowStyle = function(index){
                    if(.[index, "tpebank"] < 0){
                      list(background = "#FFCCCB", color = "black")
                    }
                  },
                  columns = list(
                    team = colDef(width = 200)
                  )
                )
            }
          ) 
      })
      
    }
  )
}