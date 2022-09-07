
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


conn_obj <- 
  create_discord_connection(
    webhook = 'https://discord.com/api/webhooks/1017045013944864849/Mfx9KjaQ96y-UvfsYun9QoFwb6gQjS2_gzjyth24pEmxlYdVkNB1PrGTjQ-qtc4iDl9K', 
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
    (now() - started) < hours(2)
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
    (now() - started) < hours(2)
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


