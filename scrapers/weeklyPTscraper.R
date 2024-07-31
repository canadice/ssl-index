
### Loading packages, use install.packages("name") if you need
require(tidyverse)
require(rvest)
require(fuzzyjoin)

### The link to the Weekly PT thread and adding multiple pages (forum has max 10 posts per page)
url <- 
  "https://forum.simulationsoccer.com/showthread.php?tid=5485" %>% 
  paste(., "&page=", 1:20, sep = "")


### Function that reads the url, takes the username, post text and link into a tibble
readPosts <- function(page){
  words <- 
    read_html(page) %>% 
    html_elements(".post_body") %>% 
    html_text2() %>% 
    stringr::str_remove_all(pattern = "\\r") %>% 
    stringr::str_squish()
  
  link <- 
    read_html(page) %>% 
    html_elements(".post_body a") %>% 
    html_attr("href")  
    
  
  user <- 
    read_html(page) %>% 
    html_elements(".author_information") %>% 
    html_elements("a") %>% 
    html_text2() %>% 
    stringi::stri_remove_empty_na()
  
  tibble(
    username = user,
    word = words,
    link = link
  )
}

### Maps all the thread pages and summarizes all unique posts into a single tibble
forum <- 
  map(
    .x = url,
    .f = readPosts
  ) %>% 
    do.call(what = rbind, args = .) %>% 
  unique() 

### Writes tibble to csv file with correct encoding
write.csv(forum, file = "Weekly PT Grading Base.csv", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")





