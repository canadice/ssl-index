
############################################################################
############################################################################
###                                                                      ###
###                   AUTOMATIC GRADING OF PREDICTIONS                   ###
###                                                                      ###
############################################################################
############################################################################

require(rvest)
require(stringr)
require(stringi)
require(plyr)
require(dplyr)
require(tidyr)

url <- 
  "http://sslforums.com/index.php?showtopic=268" %>% 
  c(
    .,
    paste(., "&st=15", sep = "")
  )

correct <- 
  c(
    "1", 
    "2", 
    "1", 
    "1"
  )


scrapePredictions <- function(url){
  predictions <- 
    read_html(url) %>% 
    html_elements(".postcolor") %>% 
    html_text2()
  
  firstPost <- any(str_detect(predictions, pattern = "Claim Thread"))
  
  predictions <- 
    predictions %>% 
    .[!str_detect(., pattern = "Claim Thread")] %>% 
    str_split(pattern = "\n") %>% 
    sapply(
      X = .,
      FUN = function(x){
        stri_remove_empty_na(x) %>% 
          t() %>% 
          as.data.frame()
      }
    )
  
  if(predictions %>% dim() %>% length() < 2){
    predictions <- 
      predictions %>% 
      do.call(
        what = rbind.fill,
        args = .
      )
  } else {
    predictions <- 
      predictions %>% 
      t() %>% 
      as.data.frame()
  }
  
  users <- 
    read_html(url) %>% 
    html_elements(".normalname") %>% 
    html_text2()
  
  if(firstPost){
    users <- users[-1]
  }
  
  predictions$users <- users
  
  return(predictions)
}

predictions <- 
  lapply(
    X = url,
    FUN = scrapePredictions
  ) %>% 
  do.call(
    what = rbind.fill,
    args = .
  ) %>% 
  as_tibble() %>% 
  mutate(
    V1 = str_remove_all(string = V1, pattern = "[0-9][\\.: ]+") %>% toupper(),
    V2 = str_remove_all(string = V2, pattern = "[0-9][\\.: ]+") %>% toupper(),
    V3 = str_remove_all(string = V3, pattern = "[0-9][\\.: ]+") %>% toupper(),
    V4 = str_remove_all(string = V4, pattern = "[0-9][\\.: ]+") %>% toupper(),
    correct = NA
  ) %>% 
  arrange(users)

for(i in 1:nrow(predictions)){
  predictions$correct[i] = sum(predictions[i,1:4] == correct) %>% as.numeric()
}

paste(predictions$users, predictions$correct, sep = ": ") %>% paste0(collapse = "\n") %>% cat() 

mean(predictions$correct) %>% round(digits = 0)




