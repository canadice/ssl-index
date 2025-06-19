
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
 
tid <- new |> 
  html_elements("a[title]") |> 
  html_attr("href") |> 
  str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

index <- !sapply(X = tid, FUN = function(x) any(str_detect(postedThreads$link, x), na.rm = TRUE))


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
  print("No new announcements!")
}

##################################################################
##                         Job Openings                         ##
##################################################################

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=40"

new <- newThreads(forum)

title <- new %>% 
  html_elements("a[title]") %>% 
  html_text2()

link <- new %>% 
  html_elements("a[title]") %>% 
  html_attr("href")

tid <- link %>% str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

title <- title[!(tid %in% postedThreads$title)]
tid <- tid[!(tid %in% postedThreads$title)]

if(length(tid) > 0){
  postedThreads <- postedThreads %>% 
    add_row(
      lapply(X = 1:length(tid), function(X){
        send_webhook_message(
          paste(
            "## New Job Opening!", "\n\n", 
            paste(
              paste("[",title[X],"](", paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]), ")", sep = ""), collapse = "\n\n"
            ),
            "\n\n||<@&957275417365057566>||"
          )
        )
        
        tibble(
          title = tid[X],
          link = paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]),
          forum = forum
        ) %>% 
          return()
      }) %>% 
        do.call(what = rbind.fill, args = .)
    )
  
  print("Sent new job openings.")
} else {
  print("No new job openings!")
}


##################################################################
##                         SSL Bounties                         ##
##################################################################

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=187"

new <- newThreads(forum)

title <- new %>% 
  html_elements("a[title]") %>% 
  html_text2()

link <- new %>% 
  html_elements("a[title]") %>% 
  html_attr("href")

tid <- link %>% str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

title <- title[!(tid %in% postedThreads$title)]
tid <- tid[!(tid %in% postedThreads$title)]

if(length(tid) > 0){
  postedThreads <- postedThreads %>% 
    add_row(
      lapply(X = 1:length(tid), function(X){
        send_webhook_message(
          paste(
            "## Seasonal Bounties Open!", "\n\n", 
            paste(
              paste("[",title[X],"](", paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]), ")", sep = ""), collapse = "\n\n"
            ),
            "\n\n||<@&957275417365057566>||"
          )
        )
        
        tibble(
          title = tid[X],
          link = paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]),
          forum = forum
        ) %>% 
          return()
      }) %>% 
        do.call(what = rbind.fill, args = .)
    )
  
  print("Sent new bounty.")
} else {
  print("No new bounty!")
}

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv('DISCIPLINARY'), 
    username = 'The Watcher', 
    set_default = TRUE)

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=110"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 

tid <- new |> 
  html_elements("a[title]") |> 
  html_attr("href") |> 
  str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

index <- !sapply(X = tid, FUN = function(x) any(str_detect(postedThreads$link, x), na.rm = TRUE))


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
      "## :no_entry_sign: | New Punishment/Suspension!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n"
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new punishment.")
  
} else {
  print("No new punishments!")
}

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv('MEDIA_FEED'), 
    username = 'Media Watcher', 
    set_default = TRUE)

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=46"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 

tid <- new |> 
  html_elements("a[title]") |> 
  html_attr("href") |> 
  str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

index <- !sapply(X = tid, FUN = function(x) any(str_detect(postedThreads$link, x), na.rm = TRUE))

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
      "## :writing_hand: | New Media!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n"
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new media.")
  
} else {
  print("No new media!")
}

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=48"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 

tid <- new |> 
  html_elements("a[title]") |> 
  html_attr("href") |> 
  str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

index <- !sapply(X = tid, FUN = function(x) any(str_detect(postedThreads$link, x), na.rm = TRUE))


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
      "## :frame_photo: | New Graphics!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n"
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new media.")
  
} else {
  print("No new media!")
}

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=47"

new <- newThreads(forum)

title <- 
  new %>% 
  html_elements("a[title]") %>% 
  html_text2() 

tid <- new |> 
  html_elements("a[title]") |> 
  html_attr("href") |> 
  str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

index <- !sapply(X = tid, FUN = function(x) any(str_detect(postedThreads$link, x), na.rm = TRUE))


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
      "## :speaking_head: | New Podcast!", "\n\n", 
      paste(
        paste("[",title,"](", link, ")", sep = ""), collapse = "\n\n"
      ),
      "\n\n"
    )
  )
  
  postedThreads <- 
    rbind(
      postedThreads,
      data.frame(
        title = title, link = link, forum = forum
      )
    )
  
  print("Sent new media.")
  
} else {
  print("No new media!")
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
  print("No new predictions!")
}

##################################################################
##                         New Weekly PT                        ##
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
      "## New Weekly PT Thread!", "\n\n", 
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
  print("No new affiliate!")
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

currentFileThread <-
  topics[
    !(topics %>%
      html_text2() %>%
      str_detect("Academy")
    )
  ][1]

if(length(currentFileThread) > 0){
  posts <-
    currentFileThread %>% 
      html_elements("a[title]") %>%
      html_attr("href") %>%
      paste(
        "https://forum.simulationsoccer.com/",
        .,
        "&page=2",
        sep = ""
      ) %>% 
    read_html() %>% 
    html_elements(".post")
    
  
  if(posts %>% length() > 0){
    posted <-
      posts %>%
      html_elements(".post_date") %>%
      html_text2() %>%
      str_split(pattern = " - ", simplify = TRUE) %>% 
      .[,1] %>% 
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
    } else {
      print("Something was missed!")
    }
  } else {
    print("No new sim file!")
  }
  
} else {
  print("No new sim file!")
}







##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv('TRANSACTIONS'), 
    username = 'Budget Watcher', 
    set_default = TRUE)
#################################################################
##                          TRANSACTIONS                       ##
#################################################################

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=52"

new <- newThreads(forum)

title <- new %>% 
  html_elements("a[title]") %>% 
  html_text2()

link <- new %>% 
  html_elements("a[title]") %>% 
  html_attr("href")

tid <- link %>% str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

title <- title[!(tid %in% postedThreads$title)]
tid <- tid[!(tid %in% postedThreads$title)]

if(length(tid) > 0){
  postedThreads <- postedThreads %>% 
    add_row(
      lapply(X = 1:length(tid), function(X){
        send_webhook_message(
          paste(
            "## New Transaction!", "\n\n",
            paste(
              paste("[",title[X],"](", paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]), ")", sep = ""), collapse = "\n\n"
            )
          )
        )
        
        tibble(
          title = tid[X],
          link = paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]),
          forum = forum
        ) %>% 
          return()
      }) %>% 
        do.call(what = rbind.fill, args = .)
    )
    
}

forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=72"

new <- newThreads(forum)

title <- new %>% 
  html_elements("a[title]") %>% 
  html_text2()

link <- new %>% 
  html_elements("a[title]") %>% 
  html_attr("href")

tid <- link %>% str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

title <- title[!(tid %in% postedThreads$title)]
tid <- tid[!(tid %in% postedThreads$title)]

if(length(tid) > 0){
  postedThreads <- postedThreads %>% 
    add_row(
      lapply(X = 1:length(tid), function(X){
        send_webhook_message(
          paste(
            "## New Trade!", "\n\n", 
            paste(
              paste("[",title[X],"](", paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]), ")", sep = ""), collapse = "\n\n"
            )
          )
        )
        
        tibble(
          title = tid[X],
          link = paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]),
          forum = forum
        ) %>% 
          return()
      }) %>% 
        do.call(what = rbind.fill, args = .)
    )
  
}


forum <- "https://forum.simulationsoccer.com/forumdisplay.php?fid=86"

new <- newThreads(forum)

title <- new %>% 
  html_elements("a[title]") %>% 
  html_text2()

link <- new %>% 
  html_elements("a[title]") %>% 
  html_attr("href")

tid <- link %>% str_extract_all(pattern = "[0-9]+$", simplify = TRUE)

title <- title[!(tid %in% postedThreads$title)]
tid <- tid[!(tid %in% postedThreads$title)]

if(length(tid) > 0){
  postedThreads <- postedThreads %>% 
    add_row(
      lapply(X = 1:length(tid), function(X){
        send_webhook_message(
          paste(
            "## New Bonuses!", "\n\n", 
            paste(
              paste("[",title[X],"](", paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]), ")", sep = ""), collapse = "\n\n"
            )
          )
        )
        
        tibble(
          title = tid[X],
          link = paste0("https://forum.simulationsoccer.com/showthread.php?tid=", tid[X]),
          forum = forum
        ) %>% 
          return()
      }) %>% 
        do.call(what = rbind.fill, args = .)
    )
}

##################################################################
##              Storing posted threads in database              ##
##################################################################

RSQLite::dbWriteTable(con, "postedThreads", postedThreads, overwrite = TRUE)

RSQLite::dbDisconnect(con)





