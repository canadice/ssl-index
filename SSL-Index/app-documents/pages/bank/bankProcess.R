bankProcessUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(title = "Transactions to approve", width = NULL, solidHeader = TRUE,
                 reactableOutput(ns("needApproval")) %>% 
                   withSpinnerMedium(),
                 br(),
                 fluidRow(
                   column(
                     width = 12,
                     actionButton(ns("goApprove"), "Approve selected transactions"),  
                     actionButton(ns("goReject"), "Reject selected transactions")  
                   )
                 )
             )
      )
    ),
    fluidRow(
      column(12,
             box(title = "Historical transactions", width = NULL, solidheader = TRUE,
             reactableOutput(ns("history")) %>% 
               withSpinnerMedium()
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
        readAPI("https://api.simulationsoccer.com/bank/getBankTransactions",
                query = list(status = 0)
        ) %>% 
          future_promise()
      }) %>% 
        bindEvent(
          updated()
        )
      
      allTransactions <- reactive({
        readAPI("https://api.simulationsoccer.com/bank/getBankTransactions") %>% 
          future_promise()
      }) %>% 
        bindEvent(
          updated()
        )
      
      
      #### OUTPUTS ####
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
                  pagination = FALSE,
                  columns = 
                    list(
                      Time = colDef(format = colFormat(datetime = TRUE)),
                      Transaction = colDef(style = function(value){list(color = if_else(value < 0, red, "white"))},
                                      format = colFormat(prefix = "$", separators = TRUE, digits = 0))
                    )
                )
            }
          )
      })
      
      output$history <- renderReactable({
        allTransactions() %>% 
          then(
            onFulfilled = function(bank){
              bank %>% 
                mutate(
                  Time = as_datetime(Time)
                ) %>% 
                reactable(
                  searchable = TRUE,
                  columns = 
                    list(
                      Time = colDef(format = colFormat(datetime = TRUE)),
                      Transaction = colDef(style = function(value){list(color = if_else(value < 0, red, "white"))}, 
                                           format = colFormat(prefix = "$", separators = TRUE, digits = 0))
                    )
                )
            }
          )
      })
      
      #### APPROVE OR REJECT OBSERVERS ####
      observe({
        transactions() %>% 
          then(
            onFulfilled = function(data){
              selected <- getReactableState("needApproval", "selected")
              req(selected)
              
              bankVerify(transactions = data[selected,], session = session, approve = TRUE)
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
              
              bankVerify(transactions = data[selected,], session = session, approve = FALSE)
              
              # updateReactable("needApproval", playerForApproval())
            }
          )
      }) %>% 
        bindEvent(
          input$goReject
        )
      
      observe({
        transactions() %>% 
          then(
            onFulfilled = function(data){
              selected <- getReactableState("needApproval", "selected")
              req(selected)
              
              removeModal()
              
              approveTransaction(data[selected,], uid = userinfo$uid)
              
              showToast("success", "The selected transactions have successfully been approved.")
              
              updated(updated() + 1)
            }
          )
      }) %>% 
        bindEvent(input$confirmApprove)
      
      observe({
        transactions() %>% 
          then(
            onFulfilled = function(data){
              selected <- getReactableState("needApproval", "selected")
              req(selected)
              
              removeModal()
              
              rejectTransaction(data[selected,], uid = userinfo$uid)
              
              showToast("success", "The selected transactions have successfully been rejected.")
              
              updated(updated() + 1)
            }
          )
      }) %>% 
        bindEvent(input$confirmReject)
    }
  )
}