playerLinkScraper <- 
  function(forum){
    ### Takes the player link scraped from the team pages
    ##  If it is a complete link with the base url there it scrapes it directly
    ##  For use with teamLinkScraper and playerLinkScraper then only the endings are used, requiring the baseLink addition
    if(stringr::str_detect(forum, "sslforums")){
      
    } else{
      baseLink <- "https://sslforums.com/"
      
      forum <- paste(baseLink, forum, sep = "")
      
    }
    
    ### Reads the information
    topic <- xml2::read_html(forum)
    
    ### Reads all topics
    playerLinks <- 
      topic %>% 
      rvest::html_elements(".topic-row") %>% 
      rvest::html_elements(".row4 a") %>% 
      rvest::html_attr("href") %>% 
      .[
        stringr::str_detect(string = ., pattern = "sslforums")
      ]
    
    return(playerLinks)
  }
