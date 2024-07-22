updateContract <- function(values){
  
  transfer <- values %>% select(link, type, transfervalue, processed)
  
  budgetQuery(
    paste(
      "INSERT INTO transactions (link, type, transfervalue, processed) VALUES ",
      paste("(", paste0(transfer, collapse = ","), ")")
    )
  )
    
  
  values <- 
    values %>% 
    pivot_longer(!pid, values_transform = as.character) %>% 
    filter(
      !is.na(value),
      name != "transfervalue"
    )
  

  
  budgetQuery(
    paste(
      "UPDATE budgetplayers SET",
      paste(
        paste("`", str_to_lower(values$name), "`", sep = ""),
        "=",
        values$value,
        collapse = ", "
      ),
      "WHERE pid = ", values$pid %>% unique(), ";"
    )
  )
}