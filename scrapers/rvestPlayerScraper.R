playerScraper <- 
  function(player){
    ### Takes the player link scraped from the team pages
    ##  If it is a complete link with the base url there it scrapes it directly
    ##  For use with teamLinkScraper and playerLinkScraper then only the endings are used, requiring the baseLink addition
    if(stringr::str_detect(player, "simsoccer")){
      
    } else{
      baseLink <- "https://simsoccer.jcink.net/"
      
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
    
    if(!("Preferred Position" %in% colnames(postData))){
      postData$`Preferred Position` <- postData$Position
      
      postData <-
        postData %>% 
        relocate(
          `Preferred Position`,
          .after = Position
        )
    }
    
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
      forum <- topic %>% 
        rvest::html_elements("#navstrip") %>% 
        rvest::html_text() %>% 
        stringr::str_squish()
      
      playerTeam <- 
        playerTeam %>% 
        dplyr::add_row() %>%
        mutate(
          team = 
            case_when(
              stringr::str_detect(forum, pattern = "Retired") ~ "Retired",
              stringr::str_detect(forum, pattern = "Prospect") ~ "Prospect",
              TRUE ~ "FA"
            )
        )
    }
    
    postData$Team <- playerTeam %>% unname() %>% unlist()
    
    userData <-
      topic %>%
      html_elements(".normalname a") %>%
      nth(1) %>%
      html_attr("href") %>%
      read_html() %>%
      html_elements("div.row2") %>%
      html_text() %>% 
      .[str_detect(., pattern = "Last Post")] %>% 
      str_split(pattern = ": ", simplify = TRUE) %>% 
      .[,2] %>% 
      str_squish()
    
    postData$lastPost <- userData 
    
    postData <- 
      postData %>% 
      mutate(
        lastPost = 
          dplyr::case_when(
            stringr::str_detect(lastPost, pattern = "minute") ~ lubridate::today(),
            stringr::str_detect(lastPost, pattern = "hour") ~ lubridate::today(),
            stringr::str_detect(lastPost, pattern = "Today") ~ lubridate::today(),
            stringr::str_detect(lastPost, pattern = "Yesterday") ~ lubridate::today()-1,
            TRUE ~ lubridate::as_date(lastPost, format = "%b %d %Y")
          ),
        Active = 
          dplyr::case_when(
            lubridate::today() - (lastPost %>% unlist()) > 21 ~ "IA",
            TRUE ~ "Active"
          )
      )
      
    postData$Username <-
      topic %>%
      html_elements(".normalname") %>%
      nth(1) %>% 
      html_text()
    
    postData$`All Traits` <- 
      paste(
        postData %>% 
          select(
            contains("Trait")
          ),
        collapse = " \\ "
      )
        
    
    # postData$lastPost
    
    postData <- 
      postData %>%
      select(
        !starts_with("Trait")
      ) %>% 
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
        Position,
        .before = `Preferred Position`
      ) %>% 
      relocate(
        `All Traits`,
        .before = lastPost
      )
    
    return(postData)
  }
