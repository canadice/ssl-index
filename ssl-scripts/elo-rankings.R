source("D:/GitHubs/ssl-index/SSL-Index/app-documents/dataLoader.R")
require(googlesheets4)
require(lubridate)
require(tidyr)
require(ggplot2)
require(stringr)
require(plotly)

load("ELO.RData")


seasonELO <- function(season, old_elo = NULL){
  schedule <- 
    read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing", 
      sheet = paste("Season", season)
    ) %>% 
    filter(
      !(str_detect(Matchday, "Pre")|str_detect(Matchday, "Shield"))
    ) %>% 
    mutate(
      `In-game Date` = `In-game Date` %>% as_date() %>% format(format = "%m/%d"),
      `IRL Date` = `IRL Date` %>% as_date() %>% format(format = "%m/%d"),
      HomeScore = 
        stringr::str_split(
          Result, 
          pattern = "-", 
          simplify = TRUE
        )[,1], 
      AwayScore = 
        stringr::str_split(
          Result, 
          pattern = "-", 
          simplify = TRUE
        )[,2]
    ) %>%
    mutate(
      HomePoints = 
        case_when(
          str_detect(HomeScore, "e|p") ~ 3,
          HomeScore > AwayScore ~ 3,
          HomeScore == AwayScore ~ 1,
          TRUE ~ 0
        ),
      AwayPoints = 
        case_when(
          str_detect(AwayScore, "e|p") ~ 3,
          HomeScore < AwayScore ~ 3,
          HomeScore == AwayScore ~ 1,
          TRUE ~ 0
        )
    ) %>% 
    select(
      -`IRL Date`,
      -`In-game Date`,
      -Result
    )
  
  if(season == 1){
    elo <- 
      teamInfo %>%
      select(
        team,
        established
      ) %>%
      filter(
        team != "FA"
      ) %>%
      mutate(
        time = 0,
        ELO = ifelse(established == season, 1500, NA)
      )
  } else {
    elo <- 
      old_elo %>% 
      mutate(
        time = ifelse(established == season, max(time), time)
      ) %>% 
      mutate(
        ELO = ifelse(established == season & time == max(time), 1500, ELO)
      )
  }
  
  elo <- 
    elo %>% 
    mutate(
      ELO = 
        case_when(
          team == "Seoul MFC" & established == season ~ 
            elo %>% 
            filter(
              team == "FC Rio"
            ) %>% 
            filter(
              time == max(time)
            ) %>% 
            select(
              ELO
            ) %>% 
            unlist(),
          team == "Reykjavik United" & established == season ~ 
            elo %>% 
            filter(
              team == "Sydney City"
            ) %>% 
            filter(
              time == max(time)
            ) %>% 
            select(
              ELO
            ) %>% 
            unlist(),
          TRUE ~ ELO
        )
    )
  
  cur_schedule <- 
    schedule %>% 
    mutate(
      Matchday = 
        unlist(Matchday) %>%
        factor(levels = unique(.)) %>% 
        as.numeric() + max(elo$time)
    )
  
  for(i in 1:nrow(cur_schedule)){
    cur_rank <- 
      elo %>%
      filter(
        team %in% c(cur_schedule$Home[i], cur_schedule$Away[i])
      ) %>% 
      group_by(
        team
      ) %>% 
      filter(
        time == max(time)
      ) %>% 
      ungroup() %>% 
      mutate(
        team = 
          factor(team, 
                 levels = 
                   c(cur_schedule$Home[i], 
                     cur_schedule$Away[i]))
      ) %>% 
      arrange(
        team
      ) %>% 
      select(
        ELO
      ) %>% 
      unlist()
    
    ## Calculating the expected score
    Q <- 10^(cur_rank/400)
    
    E <- Q / sum(Q)
    
    ## Calculating the observed score
    S <- cur_schedule[i,c("HomePoints", "AwayPoints")] / 
      sum(cur_schedule[i,c("HomePoints", "AwayPoints")])
    
    ## Calculates new rank
    K <- NA
    
    if(cur_rank[1] < 2100){
      K[1]  <- 32 
    } else if (cur_rank[1] > 2400){
      K[1] <- 16
    } else {
      K[1] <- 24
    }
    
    if(cur_rank[2] < 2100){
      K[2]  <- 32 
    } else if (cur_rank[2] > 2400){
      K[2] <- 16
    } else {
      K[2] <- 24
    }
    
    new_rank <- (cur_rank + K * (S - E)) %>% unname()
    
    ## Summarizes the new data
    new_data <- 
      data.frame(
        team = cur_schedule$Home[i],
        time = cur_schedule$Matchday[i],
        ELO = new_rank[1]
      ) %>% 
      rbind(
        c(
          team = cur_schedule$Away[i],
          time = cur_schedule$Matchday[i],
          ELO = new_rank[2]
        )
      ) %>% 
      mutate(
        established = season
      )
    
    elo <- 
      elo %>% 
      plyr::rbind.fill(
        new_data
      )
  }
  
  return(elo)
}

# current_elo <- seasonELO(1)
current_elo <- seasonELO(9, current_elo)

p <- 
  ggplot(data = current_elo) + aes(x = time, y = ELO, color = team) + 
  geom_line(linewidth = 1) + theme_bw() +
  scale_color_manual(
    "Team",
    values = 
      teamInfo %>% 
      filter(
        team != "FA",
        team %in% current_elo$team
      ) %>% 
      arrange(
        tolower(team)
      ) %>% 
      select(
        color_primary
      ) %>% 
      unlist() %>% 
      unname()
  ) +
  labs(
    x = "Time"
  ) + 
  geom_vline(
    xintercept = c(59, 15, 34, 88, 110, 130, 161, 192)
  ) + 
  scale_x_continuous(
    breaks = NULL
  ) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) 

p %>% 
  ggplotly()

current_elo %>% 
  group_by(team) %>% 
  filter(time == max(time), !(team %in% c("FC Rio", "Sydney City"))) %>% 
  ungroup() %>% 
  arrange(desc(ELO)) %>% 
  select(-time, -established) %>% 
  mutate(
    ELO = round(ELO, 0)
  ) %>% 
  rename(Team = team) %>% 
  reactable::reactable(
    pagination = FALSE,
    fullWidth = FALSE,
    columns = 
      list(
        Team = 
          reactable::colDef(
            width = 150
          )  
      )
    )
  
save(current_elo, file = "ELO.RData")


