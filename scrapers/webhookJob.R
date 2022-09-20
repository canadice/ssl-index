
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


conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv('ANNOUNCEMENTS'), 
    username = 'Forum Watcher', 
    set_default = TRUE)

#################################################################
##                        Announcements                        ##
#################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=6"

topics <- 
  read_html(forum) %>% 
  html_elements(".topic-row")
  
started <- 
  topics %>% 
  html_elements("[title]") %>% 
  html_attr("title") %>% 
  str_split(pattern = ": ", simplify = TRUE) %>% 
  .[,2] %>% 
  lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>% 
  lubridate::with_tz(tzone = "Europe/Stockholm")

new <- 
  topics[
    (now() - started) < (hours(4) + minutes(30))
  ]

if(length(new) > 0){
  title <- 
    new %>% 
    html_elements("[title]") %>% 
    html_text2()
  
  link <- 
    new %>% 
    html_elements("[title]") %>% 
    html_attr("href")
  
  send_webhook_message(
    paste(
      "New Announcement!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
} else {
  #Do Nothing
}

print("Sent new announcements.")

##################################################################
##                         Job Openings                         ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=25"

topics <- 
  read_html(forum) %>% 
  html_elements(".topic-row")

started <- 
  topics %>% 
  html_elements("[title]") %>% 
  html_attr("title") %>% 
  str_split(pattern = ": ", simplify = TRUE) %>% 
  .[,2] %>% 
  lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>% 
  lubridate::with_tz(tzone = "Europe/Stockholm")

new <- 
  topics[
    (now() - started) < (hours(4) + minutes(30))
  ]

if(length(new) > 0){
  title <- 
    new %>% 
    html_elements("[title]") %>% 
    html_text2()
  
  link <- 
    new %>% 
    html_elements("[title]") %>% 
    html_attr("href")
  
  send_webhook_message(
    paste(
      "New Job Opening!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
} else {
  #Do Nothing
}

print("Sent new job openings.")

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv("TPE_SECRET"), 
    username = 'PT Watcher', 
    set_default = TRUE)

##################################################################
##                         New ACs                              ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=7"

topics <- 
  read_html(forum) %>% 
  html_elements(".topic-row")

started <- 
  topics %>% 
  html_elements("[title]") %>% 
  html_attr("title") %>% 
  str_split(pattern = ": ", simplify = TRUE) %>% 
  .[,2] %>% 
  lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>% 
  lubridate::with_tz(tzone = "Europe/Stockholm")

new <- 
  topics[
    (now() - started) < (hours(4) + minutes(30))
  ]

if(length(new) > 0){
  title <- 
    new %>% 
    html_elements("[title]") %>% 
    html_text2()
  
  link <- 
    new %>% 
    html_elements("[title]") %>% 
    html_attr("href")
  
  send_webhook_message(
    paste(
      "New Activity Check Thread!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
} else {
  #Do Nothing
}

print("Sent new AC thread.")

##################################################################
##                         New Affiliate                        ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=34"

topics <- 
  read_html(forum) %>% 
  html_elements(".topic-row")

started <- 
  topics %>% 
  html_elements("[title]") %>% 
  html_attr("title") %>% 
  str_split(pattern = ": ", simplify = TRUE) %>% 
  .[,2] %>% 
  lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>% 
  lubridate::with_tz(tzone = "Europe/Stockholm")

new <- 
  topics[
    (now() - started) < (hours(4) + minutes(30))
  ]

if(length(new) > 0){
  title <- 
    new %>% 
    html_elements("[title]") %>% 
    html_text2()
  
  link <- 
    new %>% 
    html_elements("[title]") %>% 
    html_attr("href")
  
  send_webhook_message(
    paste(
      "New Affiliate Thread!", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
} else {
  #Do Nothing
}

print("Sent new affiliate thread.")

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
  stringr::str_remove_all(pattern = "th|rd|^[0-9]st") %>% 
  lubridate::as_datetime(format = "%d %B %Y - %I:%M %p", tz = "America/Los_Angeles") %>%
  lubridate::with_tz(tzone = "Europe/Stockholm")

currentClaimThread <-
  topics[
    (now() - started) < (hours(4) + minutes(30))
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
      (now() - posted) < (hours(4) + minutes(30))
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
    )

  
  for(i in 1:length(link)){
    send_webhook_message(
      paste(
        "A new TPE claim has been posted!", "\n\n",
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
  

} else {
  #Do Nothing
}

print("Sent new pt claim.")

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv("CHECK_SECRET"), 
    username = 'Captain Hook', 
    set_default = TRUE)


##################################################################
##                       Retirement posts                       ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=83"

topics <- 
  read_html(forum) %>% 
  html_elements(".topic-row")

started <- 
  topics %>% 
  html_elements("[title]") %>% 
  html_attr("title") %>% 
  str_split(pattern = ": ", simplify = TRUE) %>% 
  .[,2] %>% 
  lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>% 
  lubridate::with_tz(tzone = "Europe/Stockholm")

new <- 
  topics[
    (now() - started) < (hours(4) + minutes(30))
  ]

if(length(new) > 0){
  title <- 
    new %>% 
    html_elements("[title]") %>% 
    html_text2()
  
  link <- 
    new %>% 
    html_elements("[title]") %>% 
    html_attr("href")
  
  send_webhook_message(
    paste(
      "-----------------------------------------------", "\n",
      "Someone has announced their retirement:", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
} else {
  #Do Nothing
}

print("Sent new retirement.")

##################################################################
##                      New player created                      ##
##################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=42"

topics <- 
  read_html(forum) %>% 
  html_elements(".topic-row")

started <- 
  topics %>% 
  html_elements("[title]") %>% 
  html_attr("title") %>% 
  str_split(pattern = ": ", simplify = TRUE) %>% 
  .[,2] %>% 
  lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>% 
  lubridate::with_tz(tzone = "Europe/Stockholm")

new <- 
  topics[
    (now() - started) < (hours(4) + minutes(30))
  ]

if(length(new) > 0){
  title <- 
    new %>% 
    html_elements("[title]") %>% 
    html_text2()
  
  link <- 
    new %>% 
    html_elements("[title]") %>% 
    html_attr("href")
  
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
  
} else {
  #Do Nothing
}

print("Sent new player created.")

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv("WAIVER_SECRET"), 
    username = 'Waiver Watcher', 
    set_default = TRUE)


#################################################################
##                      New Waiver Claims                      ##
#################################################################

forum <- "https://simsoccer.jcink.net/index.php?showforum=72"

topics <- 
  read_html(forum) %>% 
  html_elements(".topic-row")

started <- 
  topics %>% 
  html_elements("[title]") %>% 
  html_attr("title") %>% 
  str_split(pattern = ": ", simplify = TRUE) %>% 
  .[,2] %>% 
  lubridate::as_datetime(format = "%b %d %Y, %I:%M %p", tz = "America/Los_Angeles") %>% 
  lubridate::with_tz(tzone = "Europe/Stockholm")

new <- 
  topics[
    (now() - started) < (hours(4) + minutes(30))
  ]

if(length(new) > 0){
  title <- 
    new %>% 
    html_elements("[title]") %>% 
    html_text2()
  
  link <- 
    new %>% 
    html_elements("[title]") %>% 
    html_attr("href")
  
  send_webhook_message(
    paste(
      "-----------------------------------------------", "\n",
      "A new waiver claim has started:", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
} else {
  #Do Nothing
}

print("Sent new waiver post.")

##---------------------------------------------------------------
##                      New Discord Channel                     -
##---------------------------------------------------------------

conn_obj <- 
  create_discord_connection(
    webhook = Sys.getenv("APPROVED_SECRET"), 
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
  html_text2() %>%
  str_split(pattern = "\n", simplify = TRUE) %>%
  .[,1] %>%
  stringr::str_remove_all(pattern = "th|rd|^[0-9]st") %>%
  lubridate::as_datetime(format = "%d %B %Y - %I:%M %p", tz = "America/Los_Angeles") %>%
  lubridate::with_tz(tzone = "Europe/Stockholm") %>% 
  .[seq(2, length(.), by = 2)]
  
new <- 
  topics[
    (now() - lastPost) < (hours(4) + minutes(30))
  ]

if(length(new) > 0){
  title <- 
    new %>% 
    html_elements("[title]") %>% 
    html_text2()
  
  link <- 
    new %>% 
    html_elements("[title]") %>% 
    html_attr("href")
  
  send_webhook_message(
    paste(
      "-----------------------------------------------", "\n",
      "A new player has been approved:", "\n\n", 
      paste(
        title, link, sep = " - ", collapse = "\n\n"
      )
    )
  )
  
} else {
  #Do Nothing
}

print("Sent new new approved player.")
