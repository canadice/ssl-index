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
      playerForApproval <- reactiveVal({
        getPlayersForApproval()
      })
      
      output$needApproval <- renderReactable({
        playerForApproval() |> 
          select(
            !c(uid, pid)
          ) |> 
          reactable(
            selection = "single",
            onClick = "select"
          )
      })
      
      observe({
        selected <- getReactableState("needApproval", "selected")
          req(selected)
          
          approvePlayer(playerForApproval()[selected,"uid"], session = session)
          
          showToast("success", "Player has successfully been approved.")
          
          playerForApproval(getPlayersForApproval())
          
          updateReactable("needApproval", playerForApproval())
      }) |> 
        bindEvent(
          input$goApprove
        )
    }
  )
}
