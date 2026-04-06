
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
  ) |> 
  rename(AIRING = 13)

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
    
    send_webhook_message(
      paste(
        "========================\n",
        "<@&957275484385861672>, Today's matches will premiere in: ", 
        paste(
          paste(
            "<t:",
            current$`AIRING` |> 
              lubridate::force_tz(tzone = "America/New_York") |>
              lubridate::with_tz(tzone = "Europe/London") |> 
              as.numeric(),
            ":R>",
            sep = ""
          ),
          collapse = " & "
        ),
        "!\n\n",
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






