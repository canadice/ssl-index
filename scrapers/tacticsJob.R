
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

require(googlesheets4)

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
    as.Date(`Eastern (UTC-5 / UTC-4)...3`) == today()
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
      Sys.getenv('TACTICSUSP'),
      Sys.getenv('TACTICSSHA'),
      Sys.getenv('TACTICSCDT'),
      Sys.getenv('TACTICSKTH'),
      Sys.getenv('TACTICSMAG'),
      Sys.getenv('TACTICSLIF'),
      Sys.getenv('TACTICSXLC'),
      Sys.getenv('TACTICSMSD'),
      Sys.getenv('TACTICSRMP')
    )
  
  sendDeadline <- function(x){
    conn_obj <- 
      create_discord_connection(
        webhook = x, 
        username = 'Tactics Reminder', 
        set_default = TRUE)
    
    send_webhook_message(
      paste(
        "=======================================\n",
        "## Tactics deadline has passed!\n", 
        "=======================================",
        sep = ""
      )
    )
  }
  
  
  lapply(
    X = hooks, 
    FUN = sendDeadline
  )
}







