modalBankVerify <- function(update, cost, session){
  showModal(
    modalDialog(
      tagList(
        span(
          "Are you sure you want to purchase these items?" %>% strong()
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
              class = "verifyBox"
            )
        ) %>% 
          fluidRow(),
        paste("Total cost:", cost %>% dollar()),
        br(),
        fluidRow(
          column(width = 4, offset = 1,
                 modalButton("No, go back")
               ),
          column(width = 4,
                 actionButton(
                   inputId = session$ns("confirmPurchase"),
                   label = "Yes, confirm purchase!"
                 )
               )
        )
      ),
      title="Verify your purchase!",
      footer = NULL,
      easyClose = FALSE
    )
  )
}


addBankTransaction <- function(uid, pid, source, transaction){
  portalQuery(
    paste(
      "INSERT INTO banktransactions (time, pid, source, transaction, status, uid) VALUES
      (", paste0("'", lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric(), "'"),", ", 
      pid, ", ", 
      paste0("'", source, "'"),", ",
      -transaction, ", ",
      "1,",
      uid, ");"
    )
  )
}
