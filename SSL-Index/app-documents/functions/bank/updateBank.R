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


addBankTransaction <- function(uid, pid, source, transaction, status = 1){
  time <- lubridate::now() %>% with_tz("US/Pacific") %>% as.numeric()
  
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  # Try executing all inserts; if an error occurs, rollback the transaction.
  tryCatch({
    n <- length(pid)  # Assume all vectors have equal length
    
    for(i in seq_len(n)){
      res <- portalQuery(
        query = 
          "INSERT INTO banktransactions (time, pid, `source`, `transaction`, `status`, uid) 
         VALUES (?time, ?pid, ?source, ?transaction, ?status, ?uid);",
        time        = time,
        pid         = pid[i],
        source      = source[i],
        transaction = transaction[i],
        status      = status,
        uid         = uid,
        type        = "set"
      )
      
      # Optionally check if the insert failed (depends on how portalQuery returns errors)
      # If portalQuery returns NULL or a specific error code, you could trigger an error:
      if (is.null(res)) {
        stop("Insert failed for row ", i)
      }
    }
    
    # All rows inserted successfully; commit the transaction
    portalQuery(
      query = "COMMIT;",
      type = "set"
    )
    
  }, error = function(e) {
    # An error occurred; rollback the transaction
    portalQuery(
      query = "ROLLBACK;",
      type = "set"
    )
    message("Transaction failed, rolling back: ", e$message)
  })
  
  
}

approveTransaction <- function(data, uid){
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  tryCatch({
    for(i in 1:nrow(data)){
      portalQuery(
        query = "UPDATE banktransactions 
               SET status = 1, approvedBy = ?approvedBy
               WHERE status = 0 
                 AND time = ?time 
                 AND pid = ?pid 
                 AND source = ?source;",
        approvedBy = uid,
        time       = data[i, "Time"],
        pid        = data[i, "pid"],
        source     = data[i, "Source"],
        type       = "set"
      )
    }
    
    # Commit the transaction if all updates succeed
    portalQuery(
      query = "COMMIT;",
      type = "set"
    )
    
  }, error = function(e) {
    # Rollback the transaction if any error occurs
    portalQuery(
      query = "ROLLBACK;",
      type = "set"
    )
    message("Error updating banktransactions, transaction rolled back: ", e$message)
  })
  
}

rejectTransaction <- function(data, uid){
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  tryCatch({
    for(i in 1:nrow(data)){
      portalQuery(
        query = "UPDATE banktransactions 
               SET status = -1, approvedBy = ?approvedBy
               WHERE status = 0 
                 AND time = ?time 
                 AND pid = ?pid 
                 AND source = ?source;",
        approvedBy = uid,
        time       = data[i, "Time"],
        pid        = data[i, "pid"],
        source     = data[i, "Source"],
        type       = "set"
      )
    }
    
    # Commit the transaction if all updates succeed
    portalQuery(
      query = "COMMIT;",
      type = "set"
    )
    
  }, error = function(e) {
    # Rollback the transaction if any error occurs
    portalQuery(
      query = "ROLLBACK;",
      type = "set"
    )
    message("Error updating banktransactions, transaction rolled back: ", e$message)
  })
  
}
