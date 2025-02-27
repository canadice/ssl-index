modalBankVerify <- function(update, cost, session){
  showModal(
    modalDialog(
      tagList(
        span(
          "Are you sure you want to purchase these items?" |> strong()
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
            ) |> 
              HTML()
          ) |> 
            div(
              class = "verifyBox"
            )
        ) |> 
          fluidRow(),
        paste("Total cost:", cost |> dollar()),
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


addBankTransaction <- function(uid, pid, source, transaction, status = 1){
  time <- lubridate::now() |> with_tz("US/Pacific") |> as.numeric()
  
  portalQuery(
    paste(
      "INSERT INTO banktransactions (time, pid, `source`, `transaction`, `status`, uid) 
      VALUES",
      paste(
        "(",
        paste(
          time,
          pid,
          paste0("'", source, "'"),
          transaction,
          status,
          uid,
          sep = ","
        ),
        ")",
        collapse = ","
      ),
      ";"
    )
  )
}

approveTransaction <- function(data, uid){
  for(i in 1:nrow(data)){
    portalQuery(
      paste(
        "UPDATE banktransactions 
        SET status = 1, approvedBy = ", uid, " 
        WHERE status = 0 AND time = ", data[i, "Time"], " AND pid = ", data[i, "pid"], " AND source = '", data[i,"Source"], "';",
        sep = ""
      )
    )
  }
}

rejectTransaction <- function(data, uid){
  for(i in 1:nrow(data)){
    portalQuery(
      paste(
        "UPDATE banktransactions 
        SET status = -1, approvedBy = ", uid, " 
        WHERE status = 0 AND time = ", data[i, "Time"], " AND pid = ", data[i, "pid"], " AND source = '", data[i,"Source"], "';",
        sep = ""
      )
    )
  }
}
