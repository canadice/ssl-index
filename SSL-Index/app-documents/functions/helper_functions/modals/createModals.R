modalVerifyBuild <- function(data, session){
  showModal(
    modalDialog(
      span(
        "Check the information in the build." |> strong(),
        style = "color: red;"
      ),
      br(),
      column(
        width = 10,
        offset = 1,
        helpText(
          paste(
            apply(
              data |> 
                t() |> 
                tibble() |> 
                mutate(
                  ` ` = colnames(data)
                ) |> 
                relocate(
                  ` `
                ),
              MARGIN = 1,
              FUN = function(x) {
                paste(x[1] |> str_to_title(), x[2], sep = ": ")
              }
            ),
            collapse = "<br>"
          ) |> 
            HTML()
        ) |> 
          div(
            style = "background: #f0f0f0; border: #656565"
          )
      ),
      br(),
      tagList(
        modalButton("No, go back"),
        actionButton(
          inputId = session$ns("confirmBuild"),
          label = "Yes, confirm update!"
        )
      ),
      title="Verify build!",
      footer = NULL,
      easyClose = FALSE
    )
  )
}

modalUnfinishedBuild <- function(){
  showModal(
    modalDialog(
      span("Please allocate as much of the TPE you are given as possible. If you need help with your build, reach out to an Academy coach on Discord."),
      title="Unfinished build!",
      footer = tagList( modalButton("Ok") ),
      easyClose = TRUE
    )
  )
}
