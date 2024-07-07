bankProcessUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(title = "Transactions to approve", width = NULL, solidHeader = TRUE,
                 reactableOutput(ns("needApproval")) %>% 
                   withSpinnerMedium(),
                 fluidRow(
                   column(
                     width = 12,
                     actionButton(ns("goApprove"), "Approve selected transactions"),  
                     actionButton(ns("goReject"), "Reject selected transactions")  
                   )
                 )
             )
      )
    )
  )
}

bankProcessServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      updated <- reactiveVal({0})
      
      transactions <- reactive({
        getBankTransactionsForApproval()
      }) %>% 
        bindEvent(
          updated()
        )
      
      output$needApproval <- renderReactable({
        transactions() %>% 
          then(
            onFulfilled = function(data){
              data %>% 
                mutate(
                  Time = as_datetime(Time)
                ) %>% 
                reactable(
                  selection = "multiple",
                  onClick = "select",
                  columns = 
                    list(
                      Time = colDef(format = colFormat(datetime = TRUE))
                    )
                )
            }
          )
      })
      
      observe({
        transactions() %>% 
          then(
            onFulfilled = function(data){
              selected <- getReactableState("needApproval", "selected")
              req(selected)
              
              approveTransaction(data[selected,], uid = userinfo$uid)
              
              showToast("success", "The selected transactions have successfully been approved.")
              
              updated(updated() + 1)
              
              # updateReactable("needApproval", playerForApproval())
            }
          )
      }) %>% 
        bindEvent(
          input$goApprove
        )
      
      observe({
        transactions() %>% 
          then(
            onFulfilled = function(data){
              selected <- getReactableState("needApproval", "selected")
              req(selected)
              
              rejectTransaction(data[selected,], uid = userinfo$uid)
              
              showToast("success", "The selected transactions have successfully been rejected.")
              
              updated(updated() + 1)
              
              # updateReactable("needApproval", playerForApproval())
            }
          )
      }) %>% 
        bindEvent(
          input$goReject
        )
    }
  )
}