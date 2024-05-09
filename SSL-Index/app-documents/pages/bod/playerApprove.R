playerApproveUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             reactableOutput(ns("needApproval")) %>%
               box(title = "Players to approve", width = NULL, solidHeader = TRUE)
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
          mutate(
            Approval = actionButton(session$ns(paste("approve", uid, sep = "")), label = "Approve!") %>% as.character()
          ) %>% 
          reactable(
            columns = 
              list(
                Approval = colDef(html = TRUE)
              )
          )
      })
      
      lapply(getPlayersForApproval()$uid, function(i) {
        
        observe({
          
          approvePlayer(i)
          
          updateReactable(session$ns("needApproval"), 
                          data = getPlayersForApproval())
          
        }) %>% 
          bindEvent(
            input[[paste("approve", i, sep = "")]]
          )
      })
      
    }
  )
}