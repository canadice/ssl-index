updateTransaction <- function(transaction, players = tibble(), picks = tibble()){
  tid <- 
    budgetQuery(
      paste(
      "SELECT AUTO_INCREMENT
FROM information_schema.TABLES
WHERE TABLE_SCHEMA = 'budgetdb'
AND TABLE_NAME = 'transactions';")
      ) |> unlist()
  
  budgetQuery(
    paste(
      "INSERT INTO transactions (link, type, processed) VALUES ",
      paste("(", paste0(transaction, collapse = ","), ")")
    )
  )
  
  if(nrow(picks) > 0){
    budgetQuery(
      paste(
        "INSERT INTO transactionspicks VALUES",
        paste(
          "(", paste(tid, picks$pick, picks$org, sep = ", "), ")", collapse = ","
        ),
        ";"
      )
    )
    
    for(i in 1:nrow(picks)){
      budgetQuery(
        paste(
          "UPDATE draftpicks SET current = ", picks$org[i], "WHERE pickid = ", picks$pick[i],
          ";"
        )
      )
    }
  }
  
  if(nrow(players) > 0){
    budgetQuery(
      paste(
        "INSERT INTO transactionsplayers VALUES",
        paste(
          "(", paste(tid, players$player, players$org, sep = ", "), ")", collapse = ","
        ),
        ";"
      )
    )
    
    for(i in 1:nrow(players)){
      budgetQuery(
        paste(
          "UPDATE budgetplayers SET org = ", players$org[i], "WHERE pid = ", players$player[i],
          ";"
        )
      )
    }
  }
  
  
  
  
  
}
