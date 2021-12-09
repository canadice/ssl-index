playerScraper <- 
  function(player){
    ### Takes the player link scraped from the team pages
    ##  If it is a complete link with the base url there it scrapes it directly
    ##  For use with teamLinkScraper and playerLinkScraper then only the endings are used, requiring the baseLink addition
    if(stringr::str_detect(player, "sslforums")){
      
    } else{
      baseLink <- "https://sslforums.com/"
      
      player <- paste(baseLink, player, sep = "")
      
    }
    
    ### Reads the information
    topic <- xml2::read_html(player)
    
    postData <- 
      topic %>% 
      rvest::html_elements(".post2") %>% 
      dplyr::nth(2) %>% 
      rvest::html_elements(".postcolor") %>% 
      rvest::html_text2() %>% 
      stringr::str_split(pattern = "\\n") %>% 
      unlist() %>% 
      .[stringr::str_detect(string = ., pattern = ":")] %>% 
      .[!stringr::str_detect(string = ., pattern = "edited by")] %>% 
      stringr::str_split(pattern = ":", simplify = TRUE) %>% 
      matrix(ncol = 2) %>% 
      data.frame() %>% 
      mutate(
        X2 = stringr::str_squish(X2)
      ) %>% 
      tidyr::pivot_wider(
        names_from = X1,
        values_from = X2
      )
    
    postData$Created <- 
      topic %>% 
      rvest::html_elements(".postdetails") %>% 
      rvest::html_text() %>% 
      dplyr::nth(1) %>% 
      stringr::str_split(pattern = ": ", simplify = TRUE) %>% 
      dplyr::nth(2) %>% 
      lubridate::as_date(format = "%b %d %Y")
    
    postData$Class <- 
      topic %>% 
      rvest::html_elements(".topic-title") %>% 
      rvest::html_text() %>% 
      stringr::str_extract_all(pattern = "(?<=\\().*?(?=\\))", simplify = TRUE) %>% 
      c()
    
    postData$`Preferred Position` <- postData$Position
    
    postData$Position <- 
      topic %>% 
      rvest::html_elements(".topic-title") %>% 
      rvest::html_text() %>% 
      stringr::str_split(pattern = " - ", simplify = TRUE) %>% 
      dplyr::nth(2) %>% 
      c()
    
    postData$TPE <- 
      topic %>% 
      rvest::html_elements(".topic-desc") %>% 
      rvest::html_text() %>% 
      stringr::str_split(pattern = ":", simplify = TRUE) %>% 
      dplyr::nth(2) %>% 
      stringr::str_squish() %>% 
      as.numeric()
    
     playerTeam <-
      teamInfo %>% 
      select(
        team
      ) %>% 
      dplyr::slice(
        topic %>% 
          rvest::html_elements("#navstrip") %>% 
          rvest::html_text() %>% 
          stringr::str_squish() %>% 
          stringr::str_detect(
            ## Takes team information from a separate data set 
            pattern = teamInfo$team
          ) %>% 
          which()  
      )
     
     if((playerTeam %>% nrow()) == 0){
       playerTeam <- 
         playerTeam %>% 
         dplyr::add_row() 
     }
    
    postData$Team <- playerTeam %>% unname() %>% unlist()
    
    postData <- 
      postData %>% 
      relocate(
        c(
          Class,
          TPE,
          Created,
          Team
        ),
        .after = Username
      ) %>% 
      relocate(
        `Preferred Position`,
        .after = Position
      )
    
    return(postData)
  }
