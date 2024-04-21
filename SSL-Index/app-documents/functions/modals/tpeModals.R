modalOverdraft <- function(){
  showModal(
    modalDialog(
      span("You have spent too much TPE on your attributes! Reduce some of your attributes and try again."),
      title="Not enough TPE!",
      footer = tagList( modalButton("Ok") ),
      easyClose = TRUE
    )
  )
}

modalReduction <- function(update){
  showModal(
    modalDialog(
      span(
        "You cannot reduce attributes in a regular update.",
        paste("Return ", paste0(update$attribute[(update$old - update$new) > 0], collapse = ", "), " to their original values.")
      ),
      title="Reducing attributes!",
      footer = 
        tagList(
          modalButton("Ok")
        ),
      easyClose = TRUE
    )
  )
}

modalVerify <- function(update, session){
  showModal(
    modalDialog(
      span(
        "Are you sure you want to update these attributes?" %>% strong(),
        style = "color: red;"
      ),
      br(),
      column(
        width = 8,
        offset = 2,
        helpText(
          paste(
            paste(
              update$attribute,
              paste(
                update$old,
                update$new,
                sep = " -> "
              )
            ),
            collapse = "<br>"
          ) %>% 
            HTML()
        ) %>% 
          div(
            style = "background: #f0f0f0; border: #656565"
          )
      ),
      br(),
      tagList(
        modalButton("No, go back"),
        actionButton(
          inputId = session$ns("confirmUpdate"),
          label = "Yes, confirm update!"
        )
      ),
      title="Update output",
      footer = NULL,
      easyClose = FALSE
    )
  )
}

modalNothing <- function(){
  showModal(
    modalDialog(
      span(
        "You have not changed your build yet, there is nothing to update."
      ),
      title="No changes made!",
      footer = 
        tagList(
          modalButton("Ok")
        ),
      easyClose = TRUE
    )
  )
}

modalAC <- function(){
  showModal(
    modalDialog(
      span(
        "You have successfully claimed your Activity Check for the week"
      ),
      title="Activity Check Claimed!",
      footer = 
        tagList(
          modalButton("Ok")
        ),
      easyClose = TRUE
    )
  )
}

modalTC <- function(tpe){
  showModal(
    modalDialog(
      span(
        paste("You have successfully claimed your Training Camp for the season.", tpe$tpe, "TPE has been added to your player.")
      ),
      title="Training Camp Claimed!",
      footer = 
        tagList(
          modalButton("Ok")
        ),
      easyClose = TRUE
    )
  )
}
