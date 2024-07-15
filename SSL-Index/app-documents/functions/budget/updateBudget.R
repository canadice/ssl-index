updateContract <- function(values){
  values <- 
    values %>% 
    pivot_longer(!pid, values_transform = as.character) %>% 
    filter(
      !is.na(value)
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