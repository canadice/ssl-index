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
        playerForApproval() %>% 
          reactable(
            selection = "single",
            onClick = "select"
          )
      })
      
      observe({
        selected <- getReactableState("needApproval", "selected")
          req(selected)
          
          approvePlayer(playerForApproval()[selected,"uid"])
          
          addBankTransaction(uid = 1, pid = playerForApproval()[selected,"pid"], source = "Academy Contract", transaction = 3000000, status = 1)
          
          
          today <- (now() %>% as_date() %>% as.numeric())
          start <- (currentSeason$startDate %>% as_date() - days(7)) %>% as.numeric()
          
          tpe <- 
            tibble(
              source = "Catch-up TPE",
              tpe = floor((today - start)/7)*6
            )
          
          tpeLog(uid = 1, pid = playerForApproval()[selected,"pid"], tpe = tpe)
          updateTPE(pid = playerForApproval()[selected,"pid"], tpe = tpe)
          
          showToast("success", "Player has successfully been approved.")
          
          playerForApproval(getPlayersForApproval())
          
          updateReactable("needApproval", playerForApproval())
      }) %>% 
        bindEvent(
          input$goApprove
        )
    }
  )
}