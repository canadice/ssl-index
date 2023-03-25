
###########################################################################
###########################################################################
###                                                                     ###
###               FORUM SCRAPER SCRIPT FOR GITHUB ACTIONS               ###
###                                                                     ###
###########################################################################
###########################################################################

#remotes::install_github("Canadice/sslrtools")
# remotes::install_github("EriqLaplus/discordr")
require(discordr)
require(sslrtools)

require(rvest)

require(plyr)

require(dplyr)

require(tidyr)

require(stringr)

require(lubridate)

require(DBI)
require(dbplyr)
require(RSQLite)

teamInfo <- 
  data.frame(
    franchiseID = 0,
    teamID = 0,
    team = "FA",
    established = 0,
    abbreviation = "FA",
    color_primary = "#000000",
    color_secondary = "#ffffff"
  )

tpeCost <- 
  data.frame(
    value =  c(5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
    cost = c(0,   2,   4,   8,  12,  16,  22,  28,  34,  46,  58,  70,  88, 106, 131, 156)
  )


#### Functions that are used multiple times
newThreads <- function(forum){
  topics <- 
    read_html(forum) %>% 
    html_elements(".topic-row")
  
  started <- 
    topics %>% 
    html_elements("a[title]") %>% 
    html_attr("title") %>% 
    str_split(pattern = ": ", simplify = TRUE) %>% 
    .[,2] %>%
    stringi::stri_remove_empty_na() %>% 
    lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>% 
    lubridate::with_tz(tzone = "Europe/Stockholm")
    
  
  new <- 
    topics[
      (now() - started) < (hours(24))
    ]
  
  return(new)
}

## Opens the connection to the SQLite Database
con <- RSQLite::dbConnect(RSQLite::SQLite(), "database/hookDatabase.db")


postedThreads <- dbGetQuery(con, "SELECT * FROM postedThreads")


conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv('ANNOUNCEMENTS'), 
    username = 'Forum Watcher', 
    set_default = TRUE)

#################################################################
##                        Announcements                        ##
#################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=6"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 
 
if(length(new)>0){
  title <- title %>% .[seq(2, length(.), by =2)]
}

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&")
  
  send_webhook_message(
    paste(
      "<@&957275417365057566> New Announcement!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new announcements.")
  
} else {
  #Do Nothing
}

##################################################################
##                         Job Openings                         ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=25"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2()  
 
if(length(new)>0){
  title <- title %>% .[seq(2, length(.), by =2)]
}

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&")
  
  send_webhook_message(
    paste(
      "New Job Opening!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new job openings.")
  
} else {
  #Do Nothing
}

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv("TPE_OPPS"), 
    username = 'PT Watcher', 
    set_default = TRUE)

##################################################################
##                         New Prediction                       ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=10"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2()
 
if(length(new)>0){
  title <- title %>% .[seq(2, length(.), by =2)]
}

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&")
  
  send_webhook_message(
    paste(
      "<@&1028578599965569026> New Prediction PT has been posted!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new predictions")
  
} else {
  #Do Nothing
}

##################################################################
##                             New ACs                          ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=7"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 

 if(length(new)>0){
   title <- title %>% .[seq(2, length(.), by =2)]
 }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&")
  
  send_webhook_message(
    paste(
      "<@&1028578599965569026> New Activity Check Thread!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new AC thread.")
  
} else {
  #Do Nothing
}

##################################################################
##                         New Affiliate                        ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=34"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2()

if(length(new)>0){
 title <- title %>% .[seq(2, length(.), by =2)]
}

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&")
  
  send_webhook_message(
    paste(
      "<@&1028578599965569026> New Affiliate Thread!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new affiliate thread.")
  
} else {
  #Do Nothing
}


##################################################################
##                         New Claims                           ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=82"

topics <-
  read_html(forum) %>%
  html_elements(".topic-row")

started <-
  topics %>%
  html_elements("span.desc") %>%
  html_text2() %>%
  stringi::stri_remove_empty_na() %>%
  str_split(pattern = "\n", simplify = TRUE) %>%
  .[,1] %>%
  stringr::str_remove_all(pattern = "th|rd|nd") %>%
  stringr::str_replace_all(pattern = "([0-9]+)st", replacement = "\\1") %>% 
  lubridate::as_datetime(format = "%d %B %Y - %I:%M %p", tz = "America/Los_Angeles") %>%
  lubridate::with_tz(tzone = "Europe/Stockholm")

currentClaimThread <-
  topics[
    (now() - started) < (hours(8))
  ]

if(length(currentClaimThread) > 0){
  posts <-
    read_html(
      currentClaimThread %>%
        html_elements("span.desc") %>%
        html_elements("a") %>%
        html_attr("href") %>%
        nth(1)
      ) %>%
    html_elements("span.post-normal")

  posted <-
    posts %>%
    html_elements(".row4 span.postdetails") %>%
    html_text2() %>%
    str_split(pattern = ": ", simplify = TRUE) %>%
    .[,2] %>%
    lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>%
    lubridate::with_tz(tzone = "Europe/Stockholm")

  new <-
    posts[
      (now() - posted) < (hours(48))
    ]

  post <-
    new %>%
    html_elements("div.postcolor") %>%
    html_text2()
  
  task <-
    post %>%
    str_split(
      pattern = "The following users may claim the specified TPE for|:\n",
      simplify = TRUE) %>%
    .[,2] %>%
    str_squish()
  
  index <- !(task %in% postedThreads$title[postedThreads$forum == forum])
  
  new <- new[index & (task != "")]
  post <- post[index & (task != "")]
  task <- task[index & (task != "")]

  if(length(task) > 0){
    claims <-
      post %>%
      str_split(
        pattern = "ec1",
        simplify = TRUE
      ) %>%
      .[,2] %>%
      str_split(
        pattern = "c2",
        simplify = TRUE
      ) %>%
      .[,1]
    
    link <-
      new %>%
      html_elements("a[title]") %>%
      html_attr("onclick") %>% 
      str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
      unlist() %>% 
      paste(
        currentClaimThread %>%
          html_elements("span.desc") %>%
          html_elements("a") %>%
          html_attr("href") %>%
          nth(1) %>% 
          str_remove_all("&view=getlastpost"),
        "&st=0&#entry",
        .,
        sep = ""
      ) %>% 
      str_remove(pattern = "s=[0-9a-z]+&")
    
    
    for(i in 1:length(link)){
      send_webhook_message(
        paste(
          "<@&1028578599965569026> A new TPE claim has been posted!", "\n\n",
          paste(
            task[i], link[i], 
            paste(
              "```", claims[i], "```",
              sep = ""
            ),
            sep = " - "
          )
        )
      )
      
      Sys.sleep(5)
      
    }
    
    postedThreads <- 
      rbind(
        postedThreads,
        data.frame(
          title = task, link = link, forum = forum
        )
      )
    
    print("Sent new pt claim.")
  }
  
} else {
  #Do Nothing
}



##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv("PLAYER_CHECKS"), 
    username = 'Captain Hook', 
    set_default = TRUE)


##################################################################
##                       Retirement posts                       ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=83"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 
 
if(length(new)>0){
  title <- title %>% .[seq(2, length(.), by =2)]
}

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&")
  
  send_webhook_message(
    paste(
      "-----------------------------------------------", "\n",
      "Someone has announced their retirement:", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new retirement.")
  
} else {
  #Do Nothing
}



##################################################################
##                      New player created                      ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=42"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 
 
if(length(new)>0){
  title <- title %>% .[seq(2, length(.), by =2)]
}

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&")
  
  checks <- 
    lapply(
      link,
      FUN = function(x){
        scrape <- try(playerScraper(x), silent=TRUE)
        
        if( 
          inherits(
            scrape,  
            "try-error")
        ){
          "The formatting of the player page is wrong."
        } 
        else if (scrape$`Preferred Position` == "Goalkeeper"){
          attributes <- 
            scrape %>% 
            select(
              Acceleration:Throwing
            ) %>% 
            pivot_longer(cols = everything()) %>% 
            mutate(
              value = value %>% as.numeric()
            ) %>% 
            left_join(
              tpeCost,
              by = "value"
            )
          
          paste("The attributes sum up to:", sum(attributes$cost) - 2*156, "TPE")
             
        } else {
          attributes <- 
            scrape %>% 
            select(
              Acceleration:`Work Rate`
            ) %>% 
            pivot_longer(cols = everything()) %>% 
            mutate(
              value = value %>% as.numeric()
            ) %>% 
            left_join(
              tpeCost,
              by = "value"
            )
          
          positional <- 
            scrape %>% 
            select(
              Striker:`Defense [R]`
            ) %>% 
            pivot_longer(cols = everything()) %>% 
            mutate(
              value = value %>% as.numeric()
            )
          
          paste(
            paste("The attributes sum up to:", sum(attributes$cost) - 2*156, "TPE"),
            paste("The positional XP sum up to:", sum(positional$value)),
            sep = "\n"
          )
        }
      }
    )
  
  send_webhook_message(
    paste(
      "-----------------------------------------------", "\n",
      "Someone has created:", "\n\n", 
      paste(
        title, " - ", link, " ", "BUILD CHECKS", "\n", sep = "", checks, collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new player created.")
  
} else {
  #Do Nothing
}


##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv("WAIVER_WATCHER"), 
    username = 'Waiver Watcher', 
    set_default = TRUE)


#################################################################
##                      New Waiver Claims                      ##
#################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=72"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 

 if(length(new)>0){
   title <- title %>% .[seq(2, length(.), by =2)]
 }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&") %>% 
    .[seq(2, length(.), by = 2)]
  
  send_webhook_message(
    paste(
      "-----------------------------------------------", "\n",
      "A new waiver claim has started:", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new waiver post.")
  
} else {
  #Do Nothing
}

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv("PLAYER_APPROVER"), 
    username = 'Player Approver', 
    set_default = TRUE)

#################################################################
##                   New Approved Players                      ##
#################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=61"

topics <- 
  read_html(forum) %>% 
  html_elements(".topic-row")

lastPost <-
  topics %>%
  html_elements("span.desc") %>%
  html_text2()

if(lastPost %>% length() > 0) {
  lastPost <- 
    lastPost %>% 
    str_split(pattern = "\n", simplify = TRUE) %>%
    .[,1] %>%
    stringr::str_remove_all(pattern = "th|rd|nd") %>%
    stringr::str_replace_all(pattern = "([0-9]+)st", replacement = "\\1") %>% 
    lubridate::as_datetime(format = "%d %B %Y - %I:%M %p", tz = "America/Los_Angeles") %>%
    lubridate::with_tz(tzone = "Europe/Stockholm") %>% 
    .[seq(2, length(.), by = 2)]
  
  new <- 
    topics[
      (now() - lastPost) < (hours(24))
    ]
  
  title <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_text2()
   
  if(length(new)>0){
    title <- title %>% .[seq(2, length(.), by =2)]
  }
  
  index <- !(title %in% postedThreads$title[postedThreads$forum == forum])
  
  new <- new[index]   
  title <- title[index]
}

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href")%>% 
    str_remove(pattern = "s=[0-9a-z]+&") %>% 
    .[seq(2, length(.), by =2)]
  
  send_webhook_message(
    paste(
      "-----------------------------------------------", "\n",
      "A new player has been approved:", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new new approved player.")
} else {
  #Do Nothing
}


#################################################################
##                          SIM FILES                          ##
#################################################################

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv('SIM_FILES'), 
    username = 'FM File Scraper', 
    set_default = TRUE)

forum <- "https://simsoccer.jcink.net/index.php?showforum=77"

topics <-
  read_html(forum) %>%
  html_elements(".topic-row")

started <-
  topics %>%
  html_elements("span.desc") %>%
  html_text2() %>%
  stringi::stri_remove_empty_na() %>%
  str_split(pattern = "\n", simplify = TRUE) %>%
  .[,1] %>%
  stringr::str_remove_all(pattern = "th|rd|nd") %>%
  stringr::str_replace_all(pattern = "([0-9]+)st", replacement = "\\1") %>% 
  lubridate::as_datetime(format = "%d %B %Y - %I:%M %p", tz = "America/Los_Angeles") %>%
  lubridate::with_tz(tzone = "Europe/Stockholm")

currentFileThread <-
  topics[
    (now() - started) < (hours(24))
  ]


if(length(currentFileThread) > 0){
  posts <-
    read_html(
      currentFileThread %>%
        html_elements("span.desc") %>%
        html_elements("a") %>%
        html_attr("href") %>%
        nth(1)
    ) %>%
    html_elements("span.post-normal")
  
  posted <-
    posts %>%
    html_elements(".row4 span.postdetails") %>%
    html_text2() %>%
    str_split(pattern = ": ", simplify = TRUE) %>%
    .[,2] %>%
    lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>%
    lubridate::with_tz(tzone = "Europe/Stockholm")
  
  new <-
    posts[
      (now() - posted) < (hours(24))
    ]
  
  post <-
    new %>%
    html_elements("div.postcolor") %>%
    html_text2()
  
  index <- !(post %in% postedThreads$title[postedThreads$forum == forum])
  
  new <- new[index & (post != "")]
  post <- post[index & (post != "")]
  
  if(length(post) > 0){
    link <-
      new %>%
      html_elements("a[title]") %>%
      html_attr("onclick") %>% 
      str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>% 
      unlist() %>% 
      paste(
        currentFileThread %>%
          html_elements("span.desc") %>%
          html_elements("a") %>%
          html_attr("href") %>%
          nth(1) %>% 
          str_remove_all("&view=getlastpost"),
        "&st=0&#entry",
        .,
        sep = ""
      ) %>% 
      str_remove(pattern = "s=[0-9a-z]+&")
    
    
    for(i in 1:length(link)){
      send_webhook_message(
        paste(
          "<@&921299604291612702> <@&1029418339514191983> A new sim-file has been posted", "\n\n",
          paste(
            post[i], link[i],
            sep = " - "
          )
        )
      )
      
      Sys.sleep(5)
      
    }
    
    postedThreads <- 
      rbind(
        postedThreads,
        data.frame(
          title = post, link = link, forum = forum
        )
      )
    
    print("Sent new sim file.")
  }
  
} else {
  #Do Nothing
}

##################################################################
##              Storing posted threads in database              ##
##################################################################

RSQLite::dbWriteTable(con, "postedThreads", postedThreads, overwrite = TRUE)

RSQLite::dbDisconnect(con)





