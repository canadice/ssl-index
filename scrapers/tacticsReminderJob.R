
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

leagueReminder <- 
  c(
    Sys.getenv('MATCHDAYREM')
  )

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
  ) |> 
  rename(AIRING = 13)


if(nrow(current) > 0){
  ## Sending weekly games to the Matchday channel
  conn_obj <- 
    create_discord_connection(
      webhook = leagueReminder, 
      username = 'Matchday Reminder', 
      set_default = TRUE)
  
  send_webhook_message(
    paste(
      "# Matchday Schedule \n",
      "## Next set of matches:\n\n",
      paste(
        current$Matchday, "\n @ ", 
        paste(
          "<t:", 
          current$`AIRING` |> 
            # lubridate::force_tz(tzone = "America/New_York") |> 
            as.numeric() + 5 * 60 * 60,
          ":f>!\n",
          sep = ""
        ),
        sep = "",
        collapse = "\n\n"
      ),
      sep = ""
    )
  )
  
  
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
  
  sendReminder <- function(x){
    conn_obj <- 
      create_discord_connection(
        webhook = x, 
        username = 'Tactics Reminder', 
        set_default = TRUE)
    
    send_webhook_message(
      paste(
        "######\n",
        "## Next set of matches:\n\n", 
        paste(current$Matchday, collapse = "\n\n"),
        "\n\n",
        "The tactics deadline is <t:", 
        (lubridate::today()  %>% lubridate::force_tz(tzone = "America/Los_Angeles") + lubridate::hours(12)) %>% as.numeric(),
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
  
  simmerDeadline <- 
    paste(
      "<t:",
      current$`Eastern (UTC-5 / UTC-4)...7` |> 
        #lubridate::force_tz(tzone = "America/New_York") |> 
        as.numeric() + 6 * 60 * 60,
      sep = ""
    ) |> 
      sapply(
        FUN = function(x) {
          paste0(
            x,
            ":",
            c("f", "R"), 
            ">",
            sep = "",
            collapse = " / "
          )
        },
        simplify = TRUE
      ) |> 
      unname()
  
  
  commentaryDeadline <- 
    paste(
      "<t:",
      current$`Eastern (UTC-5 / UTC-4)...11` |> 
        #lubridate::force_tz(tzone = "America/New_York") |> 
        as.numeric() + 6 * 60 * 60,
      sep = ""
    ) |> 
    sapply(
      FUN = function(x) {
        paste0(
          x,
          ":",
          c("f", "R"), 
          ">",
          sep = "",
          collapse = " / "
        )
      },
      simplify = TRUE
    ) |> 
    unname()
  
  send_webhook_message(
    paste(
      "## Next set of games\n",
      paste(
        current$Matchday, "\n",
        "Simmer: @", current$Simmer, ", Deadline:", 
        simmerDeadline[1:length(current$Simmer)], "\n",
        "Commentator: ", 
        if_else(is.na(current$`Commentator 1`), "NONE", 
                paste("@", current$`Commentator 1`, sep = "")), 
        if_else(is.na(current$`Commentator 2`), "", 
                paste(" & @", current$`Commentator 2`, sep = "")), 
        ", Deadline:", commentaryDeadline[1:length(current$Simmer)], 
        sep = "",
        collapse = "\n\n"
      ),
      "\n\n ||<@&921299760365846548>||",
      sep = ""
    )
  )
}

  
  
  




