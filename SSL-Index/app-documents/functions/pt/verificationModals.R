ptGradingVerify <- function(task, session){
  showModal(
    modalDialog(
      span(
        "Are you sure you want to submit this graded task?" %>% strong()
      ),
      br(),
      column(
        width = 12,
        reactable(
          task
        ) %>% 
          div(
            class = "verifyBox"
          )
      ),
      br(),
      tagList(
        modalButton("No, go back"),
        actionButton(
          inputId = session$ns("confirmSubmission"),
          label = "Yes, confirm grading!"
        )
      ),
      title="Task Grading",
      footer = NULL,
      easyClose = FALSE
    )
  )
}

bankVerify <- function(transactions, session, approve = TRUE){
  showModal(
    modalDialog(
      span(
        paste("Are you sure you want to", if_else(approve, "APPROVE", "REJECT"), "the following transaction(s)?") %>% strong()
      ),
      br(),
      column(
        width = 12,
        reactable(
          transactions
        ) %>% 
          div(
            class = "verifyBox"
          )
      ),
      br(),
      tagList(
        modalButton("No, go back"),
        if(approve){
          actionButton(
            inputId = session$ns("confirmApprove"),
            label = "Yes, approve!"
          )
        } else {
          actionButton(
            inputId = session$ns("confirmReject"),
            label = "Yes, reject!"
          )
        }
      ),
      title="Bank Transactions",
      footer = NULL,
      easyClose = FALSE
    )
  )
}