
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

require(googlesheets4)

require(dplyr)

require(lubridate)

hooks <- 
  c(
    Sys.getenv('MATCHDAYREM')
  )

googlesheets4::gs4_deauth()

sheet <- Sys.getenv("PRODUCTIONSH")


schedule <- 
  read_sheet(
    ss = sheet,
    skip = 1
  ) %>%
  mutate(
    across(
      everything(),
      unlist
    )
  ) 

current <- 
  schedule %>% 
  filter(
    as.Date(`YOUTUBE DATE`) == today()
  )

if(nrow(current) > 0){

  sendReminder <- function(x){
    conn_obj <- 
      create_discord_connection(
        webhook = x, 
        username = 'Matchday Reminder', 
        set_default = TRUE)
    
    premiere <- if_else(today() |> wday(week_start = 1) %in% c(4), 16, 12)
    
    send_webhook_message(
      paste(
        "========================\n",
        "<@&957275484385861672>, the next matchday will premiere in <t:", 
        (lubridate::today() %>% lubridate::force_tz(tzone = "America/Los_Angeles") + lubridate::hours(premiere)) %>% as.numeric(),
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
}






