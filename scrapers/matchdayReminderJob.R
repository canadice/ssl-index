
###########################################################################
###########################################################################
###                                                                     ###
###               TACTICS DEADLINE REMINDER FOR GITHUB ACTIONS               ###
###                                                                     ###
###########################################################################
###########################################################################

#remotes::install_github("Canadice/sslrtools")
# remotes::install_github("EriqLaplus/discordr")
require(discordr)

require(dplyr)

require(lubridate)

hooks <- 
  c(
    Sys.getenv('MATCHDAYREM')
  )

sendReminder <- function(x){
  conn_obj <- 
    create_discord_connection(
      webhook = x, 
      username = 'Matchday Reminder', 
      set_default = TRUE)
  
  send_webhook_message(
    paste(
      "========================\n",
      "<@&957275484385861672>, the next matchday will premiere in <t:", 
      (lubridate::today() %>% lubridate::force_tz(tzone = "America/Los_Angeles") + lubridate::hours(12)) %>% as.numeric(),
      ":R>!\n\n",
      "Watch the game on the [SSL Youtube](https://www.youtube.com/@simulationsoccerleague/videos) \n",
      "========================",
      sep = ""
    )
  )
}

lapply(
  X = hooks, 
  FUN = sendReminder
)






