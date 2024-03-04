
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


production <- 
  Sys.getenv("PRODUCTIONCH")

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
    as.Date(`Eastern (UTC-5)...3`) == today()
  )

if(nrow(current) > 0){
  
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
        "The tactics deadline is <t:", 
        (lubridate::today() + lubridate::hours(20)) %>% as.numeric(),
        ":R>!\n", 
        "######",
        sep = ""
      )
    )
  }
  
  lapply(
    X = hooks, 
    FUN = sendReminder
  )
  
  conn_obj <- 
    create_discord_connection(
      webhook = production, 
      username = 'BOT Producer', 
      set_default = TRUE)
  
  send_webhook_message(
    paste(
      "## Next set of games\n",
      paste(
        current$Matchday, "\n",
        "Simmer: @", current$Simmer, "\n",
        "Commentator: ", 
        if_else(is.na(current$`Commentator 1`), "NONE", 
                paste("@", current$`Commentator 1`, sep = "")), 
        if_else(is.na(current$`Commentator 2`), "", 
                paste(" & @", current$`Commentator 2`, sep = "")), 
        sep = "",
        collapse = "\n\n"
      ),
      "\n\n ||<@&921299760365846548>||",
      sep = ""
    )
  )
}

  
  
  




