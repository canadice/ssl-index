playerApproveUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(title = "Players to approve", width = NULL, solidHeader = TRUE,
                 reactableOutput(ns("needApproval")),
                 actionButton(ns("goApprove"), "Approve selected player")
             )
               
      )
    )
  )
}

playerApproveServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$needApproval <- renderReactable({
        getPlayersForApproval() %>% 
          reactable(
            selection = "single",
            onClick = "select"
          )
      })
      
      observe({
        selected <- getReactableState("needApproval", "selected")
          req(selected)
          approvePlayer(getPlayersForApproval()[selected,"uid"])
          updateReactable("needApproval", getPlayersForApproval())
      }) %>% 
        bindEvent(
          input$goApprove
        )
    }
  )
}