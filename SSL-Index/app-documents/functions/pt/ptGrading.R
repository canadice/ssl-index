ptGradingVerify <- function(task, session){
  showModal(
    modalDialog(
      span(
        "Are you sure you want to submit this graded task?" %>% strong()
      ),
      br(),
      column(
        width = 8,
        offset = 2,
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