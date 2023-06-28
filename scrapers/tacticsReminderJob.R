
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
require(sslrtools)

require(rvest)

require(plyr)

require(dplyr)

require(tidyr)

require(stringr)

require(lubridate)

hooks <- 
  c(
    Sys.getenv('TACTICSACR'),
    Sys.getenv('TACTICSATH'),
    Sys.getenv('TACTICSCABA'),
    Sys.getenv('TACTICSCAT'),
    Sys.getenv('TACTICSCAI'),
    Sys.getenv('TACTICSFCK'),
    Sys.getenv('TACTICSHOL'),
    Sys.getenv('TACTICSLAO'),
    Sys.getenv('TACTICSLON'),
    Sys.getenv('TACTICSMTL'),
    Sys.getenv('TACTICSPAR'),
    Sys.getenv('TACTICSREY'),
    Sys.getenv('TACTICSSEO'),
    Sys.getenv('TACTICSSFV'),
    Sys.getenv('TACTICSTOK'),
    Sys.getenv('TACTICSUSP')
  )

sendReminder <- function(x){
  conn_obj <- 
    create_discord_connection(
      webhook = x, 
      username = 'Tactics Reminder', 
      set_default = TRUE)
  
  send_webhook_message(
    paste(
      "######\n",
      "Tactics deadline is in less than three hours!\n", 
      "######",
      sep = ""
    )
  )
}


lapply(
  X = hooks, 
  FUN = sendReminder
)






