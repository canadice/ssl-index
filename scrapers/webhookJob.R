
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

print("Starting work for webhooks!")

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
    html_elements(".inline_row")
  
  started <- 
    topics %>% 
    html_elements(".subject_new a") %>% 
    html_attr("title") %>% 
    as.numeric() %>% 
    lubridate::as_datetime(origin = lubridate::origin) %>% 
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

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv('ANNOUNCEMENTS'), 
    username = 'Forum Watcher', 
    set_default = TRUE)

#################################################################
##                        Announcements                        ##
#################################################################

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=21"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 
 
# if(length(new)>0){
#   title <- title %>% .[seq(2, length(.), by =2)]
# }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href") %>% 
    paste(
      "https://forum.simulationsoccer.com/",
      .,
      sep = ""
    )
  
  send_webhook_message(
    paste(
      "## New Announcement!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n||<@&957275417365057566>||"
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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=40"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2()  
 
# if(length(new)>0){
#   title <- title %>% .[seq(2, length(.), by =2)]
# }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href") %>% 
    paste(
      "https://forum.simulationsoccer.com/",
      .,
      sep = ""
    )
  
  send_webhook_message(
    paste(
      "## New Job Opening!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n||<@&957275417365057566>||"
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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=25"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2()
 
# if(length(new)>0){
#   title <- title %>% .[seq(2, length(.), by =2)]
# }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href") %>% 
    paste(
      "https://forum.simulationsoccer.com/",
      .,
      sep = ""
    )
  
  send_webhook_message(
    paste(
      "## New Prediction Task!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n||<@&1028578599965569026>||"
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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=22"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 

 # if(length(new)>0){
 #   title <- title %>% .[seq(2, length(.), by =2)]
 # }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href") %>% 
    paste(
      "https://forum.simulationsoccer.com/",
      .,
      sep = ""
    )
  
  send_webhook_message(
    paste(
      "## New Activity Check!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n||<@&1028578599965569026>||"
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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=49"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2()

# if(length(new)>0){
#  title <- title %>% .[seq(2, length(.), by =2)]
# }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href") %>% 
    paste(
      "https://forum.simulationsoccer.com/",
      .,
      sep = ""
    )
  
  send_webhook_message(
    paste(
      "## New Affiliate Thread!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n||<@&1028578599965569026>||"
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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=97"

topics <-
  read_html(forum) %>%
  html_elements(".inline_row")

# started <-
#   topics %>%
#   html_elements("span.desc") %>%
#   html_text2() %>%
#   stringi::stri_remove_empty_na() %>%
#   str_split(pattern = "\n", simplify = TRUE) %>%
#   .[,1] %>%
#   stringr::str_remove_all(pattern = "th|rd|nd") %>%
#   stringr::str_replace_all(pattern = "([0-9]+)st", replacement = "\\1") %>% 
#   lubridate::as_datetime(format = "%d %B %Y - %I:%M %p", tz = "America/Los_Angeles") %>%
#   lubridate::with_tz(tzone = "Europe/Stockholm")

currentClaimThread <-
  topics[1]

if(length(currentClaimThread) > 0){
  posts <-
    read_html(
      currentClaimThread %>%
        html_elements("a[title]") %>%
        html_attr("href") %>%
        paste(
          "https://forum.simulationsoccer.com/",
          .,
          sep = ""
        )
      ) %>%
    html_elements(".post")

  if(posts %>% length() > 1){
    posted <-
      posts %>%
      html_elements(".post_date") %>%
      html_text2() %>%
      ## Removes "edited by" text
      stringr::str_split("\\(This post was last", simplify = TRUE) %>%
      .[,1] %>% 
      lubridate::ymd_hm(tz = "America/Los_Angeles") %>% 
      lubridate::with_tz(tzone = "Europe/Stockholm")
    
    new <-
      posts[
        (now() - posted) < (hours(48))
      ]
    
    if(new %>% length() > 0){
      post <-
        new %>%
        html_elements(".post_body") %>%
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
            pattern = "Code:",
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
          html_elements("a") %>%
          html_attr("href") %>% 
          .[stringr::str_detect(., pattern= "#pid")] %>% 
          paste(
            "https://forum.simulationsoccer.com/",
            .,
            sep = ""
          )
        
        for(i in 1:length(link)){
          send_webhook_message(
            paste(
              "## New TPE Claim!", "\n\n", 
              paste(
                paste("[",task[i],"](", link[i], ")", sep = ""), collapse = "\n\n"
              ),
              # paste(
              #   "```", claims[i], "```",
              #   sep = ""
              # ),
              "\n\n||<@&1028578599965569026>||"
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
    }
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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=98"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 
 
# if(length(new)>0){
#   title <- title %>% .[seq(2, length(.), by =2)]
# }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href") %>% 
    paste(
      "https://forum.simulationsoccer.com/",
      .,
      sep = ""
    )
  
  send_webhook_message(
    paste(
      "## Someone has retired!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=57"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 
 
# if(length(new)>0){
#   title <- title %>% .[seq(2, length(.), by =2)]
# }

index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)

new <- new[index]   
title <- title[index]

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href") %>% 
    paste(
      "https://forum.simulationsoccer.com/",
      .,
      sep = ""
    )
  
  checks <- 
    lapply(
      link,
      FUN = function(x){
        topic <- xml2::read_html(x)
        
        postData <- 
          topic %>%
          rvest::html_elements(".post_body") %>%
          .[1] %>%
          # ## Changes to dplyr 1.1.0 removes this functionality.
          # dplyr::nth(2) %>%
          rvest::html_text2() %>%
          stringr::str_split(pattern = "\\n") %>%
          unlist() %>%
          .[stringr::str_detect(string = ., pattern = ":")] %>%
          .[!stringr::str_detect(string = ., pattern = "edited by")] %>%
          stringr::str_split(pattern = ":", simplify = TRUE) %>%
          matrix(ncol = 2) %>%
          data.frame() %>%
          dplyr::mutate(
            X2 = stringr::str_squish(X2)
          ) %>%
          tidyr::pivot_wider(
            names_from = X1,
            values_from = X2
          )
        
        if(postData$`Preferred Position` == "GK"){
          attributes <- 
            postData %>% 
            dplyr::select(
              Acceleration:Technique
            ) %>% 
            tidyr::pivot_longer(cols = everything()) %>% 
            dplyr::mutate(
              value = value %>% as.numeric()
            ) %>% 
            dplyr::left_join(
              tpeCost,
              by = "value"
            )
          
          paste("The attributes sum up to:", sum(attributes$cost) - 2*156, "TPE")
        } else {
          attributes <- 
            postData %>% 
            dplyr::select(
              Acceleration:Technique
            ) %>% 
            tidyr::pivot_longer(cols = everything()) %>% 
            dplyr::mutate(
              value = value %>% as.numeric()
            ) %>% 
            dplyr::left_join(
              tpeCost,
              by = "value"
            )
          
          positional <- 
            postData %>% 
            dplyr::select(
              Striker:`Defense [R]`
            ) %>% 
            tidyr::pivot_longer(cols = everything()) %>% 
            dplyr::mutate(
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
      "## Someone has created!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", " ", "\nBUILD CHECKS", "\n", checks, sep = ""), collapse = "\n\n"
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
# 
# conn_obj <- 
#   create_discord_connection(
#     webhook = Sys.getenv("WAIVER_WATCHER"), 
#     username = 'Waiver Watcher', 
#     set_default = TRUE)


#################################################################
##                      New Waiver Claims                      ##
#################################################################

# forum <- "https://simsoccer.jcink.net/index.php?showforum=72"
# 
# new <- newThreads(forum)
# 
# title <- 
#   new %>% 
#   html_elements("a[title]") %>% 
#   html_text2() 
# 
#  # if(length(new)>0){
#  #   title <- title %>% .[seq(2, length(.), by =2)]
#  # }
# 
# index <- !(title %in% postedThreads$title & forum %in% postedThreads$forum)
# 
# new <- new[index]   
# title <- title[index]
# 
# if(length(new) > 0){
#   link <- 
#     new %>% 
#     html_elements("a[title]") %>% 
#     html_attr("href")%>% 
#     str_remove(pattern = "s=[0-9a-z]+&") %>% 
#     .[seq(2, length(.), by = 2)]
#   
#   send_webhook_message(
#     paste(
#       "-----------------------------------------------", "\n",
#       "A new waiver claim has started:", "\n\n", 
#       paste(
#         title, link, sep = " - ", collapse = "\n\n"
#       )
#     )
#   )
#   
#   postedThreads <- 
#     rbind(
#       postedThreads,
#       data.frame(
#         title = title, link = link, forum = forum
#       )
#     )
#   
#   print("Sent new waiver post.")
#   
# } else {
#   #Do Nothing
# }

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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=76"

topics <- 
  read_html(forum) %>% 
  html_elements(".inline_row")

lastPost <-
  topics %>%
  html_elements(".lastpost") %>%
  html_text2()

## Checks last post that is not a date and replaces it
correction <- which(!stringr::str_detect(lastPost, "AM|PM"))

lastPost[correction] <- 
  topics[correction] %>%
  html_elements(".lastpost span") %>%
  html_attr("title")

new <- NULL

if(lastPost %>% length() > 0) {
  lastPost <- 
    lastPost %>% 
    str_split(pattern = "\n", simplify = TRUE) %>%
    .[,1] %>%
    stringr::str_remove_all(pattern = "th|rd|nd") %>%
    stringr::str_replace_all(pattern = "([0-9]+)st", replacement = "\\1") %>%  
    lubridate::ymd_hm(tz = "America/Los_Angeles") %>% 
    lubridate::with_tz(tzone = "Europe/Stockholm")
  
  new <- 
    topics[
      (now() - lastPost) < (hours(24))
    ]
  
  title <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_text2()
   
  # if(length(new)>0){
  #   title <- title %>% .[seq(2, length(.), by =2)]
  # }
  
  index <- !(title %in% postedThreads$title[postedThreads$forum == forum])
  
  new <- new[index]   
  title <- title[index]
}

if(length(new) > 0){
  link <- 
    new %>% 
    html_elements("a[title]") %>% 
    html_attr("href") %>% 
    paste(
      "https://forum.simulationsoccer.com/",
      .,
      sep = ""
    )
  
  send_webhook_message(
    paste(
      "## New Player Approved!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
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

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=92"

topics <-
  read_html(forum) %>%
  html_elements(".inline_row .forumdisplay_regular .subject_new")

# started <-
#   topics %>%
#   html_elements("span.desc") %>%
#   html_text2() %>%
#   stringi::stri_remove_empty_na() %>%
#   str_split(pattern = "\n", simplify = TRUE) %>%
#   .[,1] %>%
#   stringr::str_remove_all(pattern = "th|rd|nd") %>%
#   stringr::str_replace_all(pattern = "([0-9]+)st", replacement = "\\1") %>% 
#   lubridate::as_datetime(format = "%d %B %Y - %I:%M %p", tz = "America/Los_Angeles") %>%
#   lubridate::with_tz(tzone = "Europe/Stockholm")
# 
# currentFileThread <-
#   topics[
#     (now() - started) < (hours(24))
#   ]

currentFileThread <-
  topics[1]

# currentFileThread <- 
#   currentFileThread[
#     !(currentFileThread %>% 
#       html_elements(".row4 a") %>% 
#       html_text2() %>% 
#       str_detect("Academy")
#     )
#   ]

if(length(currentFileThread) > 0){
  posts <-
    read_html(
      currentFileThread %>% 
        html_elements("a[title]") %>%
        html_attr("href") %>%
        paste(
          "https://forum.simulationsoccer.com/",
          .,
          sep = ""
        )
      ) %>%
    html_elements(".post")
  
  if(posts %>% length() > 1){
    posted <-
      posts %>%
      html_elements(".post_date") %>%
      html_text2() %>%
      lubridate::ymd_hm(tz = "America/Los_Angeles") %>% 
      lubridate::with_tz(tzone = "Europe/Stockholm")
    
    new <-
      posts[
        (now() - posted) < (hours(48))
      ]
    
    post <-
      new %>%
      html_elements(".post_body") %>%
      html_text2() %>% 
      stringr::str_remove_all(pattern = "\r") %>% 
      stringr::str_squish()
  
    index <- !(post %in% postedThreads$title[postedThreads$forum == forum])
    
    new <- new[index & (post != "")]
    post <- post[index & (post != "")]
  
    if(length(post) > 0){
      link <-
        new %>%
        html_elements("a") %>%
        html_attr("href") %>% 
        .[stringr::str_detect(., pattern= "#pid")] %>% 
        paste(
          "https://forum.simulationsoccer.com/",
          .,
          sep = ""
        )
      
      
      for(i in 1:length(link)){
        send_webhook_message(
          paste(
            "<@&921299604291612702> <@&1029418339514191983> A new sim-file has been posted", "\n\n",
            paste(
              paste("[",post[i],"](", link[i], ")", sep = ""), collapse = "\n\n"
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
  }
  
} else {
  #Do Nothing
}

##################################################################
##              Storing posted threads in database              ##
##################################################################

RSQLite::dbWriteTable(con, "postedThreads", postedThreads, overwrite = TRUE)

RSQLite::dbDisconnect(con)





