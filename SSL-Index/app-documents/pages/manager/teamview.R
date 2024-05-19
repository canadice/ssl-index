managerTeamUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(title = "Your Team Overview", width = NULL, solidHeader = TRUE,
                 reactableOutput(ns("teamOverview")) %>% 
                   withSpinnerMedium(),
                 br(),
                 actionButton(ns("goRegress"), "Regress Player")
             )
             
      )
    ),
    fluidRow(
      column(
        width = 12,
        playerInfoBoxUI(ns("playerInfo")),
      )
    ) %>% 
      div(id = ns("selectedPlayer")) %>% 
      hidden()
  )
}

managerTeamServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      playerData <- reactive({
        getPlayersFromTeam(userinfo$uid)
      })
      
      output$teamOverview <- renderReactable({
         playerData() %>% 
          then(
            onFulfilled = function(value){
              value %>% 
                select(!pid) %>% 
                reactable(
                  selection = "single",
                  onClick = "select",
                  groupBy = "affiliate"
                )
            }
          ) 
          
      })
      
      observe({
        selected <- getReactableState("teamOverview", "selected")
        req(selected)
        
        playerData() %>% 
          then(
            onFulfilled = function(value){
              selection <- value[selected,]
              
              if(selection$tpebank < 0){
                shinyjs::show("selectedPlayer")
                
                playerInfoBoxServer(id = "playerInfo", pid = selection$pid)
              } else {
                shinyjs::hide("selectedPlayer")
                showToast(type = "error", "The chosen player does not need to regress.")
              }
                    
            }
          )
        
      }) %>% 
        bindEvent(
          input$goRegress
        )
    }
  )
}