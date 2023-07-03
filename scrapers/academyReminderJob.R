
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
    Sys.getenv('ACADEMYREM')
  )

sendReminder <- function(x){
  conn_obj <- 
    create_discord_connection(
      webhook = x, 
      username = 'Academy Reminder', 
      set_default = TRUE)
  
  send_webhook_message(
    paste(
      "######\n",
      "<&435275669589721101>, this is your gentle reminder to sim the Academy.", 
      "######",
      sep = ""
    )
  )
}

lapply(
  X = hooks, 
  FUN = sendReminder
)






