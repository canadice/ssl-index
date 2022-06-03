
##################################################################
##              Season Standings Prediction Grader              ##
##################################################################

require(googlesheets4)
require(dplyr)
require(stringr)
require(rvest)
require(stringi)

sheet <- "https://docs.google.com/spreadsheets/d/1TdhL28JCGza0KdHyykjQCAYp_xfj43fK5KftTO3fS-c/edit#gid=1971845815"

predictions <- read_sheet(sheet)

colnames(predictions) <- 
  colnames(predictions) %>% 
  str_remove(pattern = "Predict the league standings: ")

correct <- 
  c(
    3,
    10,
    5,
    4,
    1,
    6,
    7,
    9,
    2,
    8
  )


url <- 
  "https://simsoccer.jcink.net/index.php?showtopic=1015" %>% 
  c(
    .,
    paste(., "&st=15", sep = ""),
    paste(., "&st=30", sep = ""),
    paste(., "&st=45", sep = "")
  )

scrapeForum <- function(url){
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
  
  if(any((predictions %>% lapply(FUN = length) %>% unlist()) > 1)){
    predictions <- 
      predictions %>% 
      do.call(
        what = plyr::rbind.fill,
        args = .
      )
  } else {
    predictions <- 
      predictions %>% 
      unlist() %>% 
      data.frame(V1 = .)
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

forumWords <- 
  lapply(
    X = url,
    FUN = scrapeForum
  ) %>% 
  do.call(
    what = plyr::rbind.fill,
    args = .
  ) %>% 
  as_tibble()

gradedPredictions <- 
  predictions %>% 
  mutate(
    `Write your username:` =
      tolower(`Write your username:`)
  ) %>% 
  full_join(
    forumWords %>% 
      mutate(
        users =
          tolower(users)
      ),
    by = c("Write your username:" = "users")
  ) %>%
  mutate(
    nrCorrect = 
      rowSums(.[,3:12] == matrix(correct, byrow = TRUE, ncol = length(correct), nrow = nrow(.)), na.rm = TRUE)+2,
    verificationCheck =
      `Add a verification word.` == V1
  ) %>% 
  arrange(
    verificationCheck
  )
  
paste(gradedPredictions$`Write your username:`, gradedPredictions$nrCorrect, gradedPredictions$verificationCheck, sep = ": ") %>% paste0(collapse = "\n") %>% cat() 

mean(gradedPredictions$nrCorrect) %>% round(digits = 0)
