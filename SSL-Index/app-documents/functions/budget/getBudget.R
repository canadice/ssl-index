getBudget <- function(){
  budgetQuery(
    paste(
      "SELECT * FROM budgetplayers;"
    )
  ) %>% 
    future_promise()
}

getBudgetPlayer <- function(pid){
  budgetQuery(
    paste(
      "SELECT * FROM budgetplayers WHERE pid = ", pid, ";"
    )
  ) %>% 
    future_promise()
}