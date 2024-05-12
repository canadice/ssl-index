managerTeamUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(title = "Players to approve", width = NULL, solidHeader = TRUE,
                 reactableOutput(ns("teamOverview")) %>% 
                   withSpinnerMedium(),
                 actionButton(ns("goApprove"), "Approve selected player")
             )
             
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(title = "Regress player", width = NULL, solidHeader = TRUE,
          playerPageUI(ns("teamOverviewPlayer"))
        )
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
                select(!uid) %>% 
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
        
        shinyjs::show("selectedPlayer")
        
        playerData() %>% 
          then(
            onFulfilled = function(value){
              playerPageServer("teamOverviewPlayer", uid = value[selected,"uid"])      
            }
          )
        
      }) %>% 
        bindEvent(
          input$goApprove
        )
    }
  )
}