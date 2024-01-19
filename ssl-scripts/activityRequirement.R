
require(rvest)
require(stringr)
require(stringi)
require(plyr)
require(dplyr)
require(tidyr)
require(arsenal)
require(DBI)
require(dbplyr)
require(RSQLite)

# source("D:/GitHubs/ssl-index/SSL-Index/app-documents/dataLoader.R")

con <- 
  dbConnect(
    SQLite(), 
    "database/SSL_Database.db"
  )

players <- 
  dbGetQuery(con, "SELECT * from Daily_Scrape") %>% 
  filter(
    !(Team %in% c("Retired", "FA", "Prospect"))
  ) %>% 
  select(
    Name,
    Team,
    Active
  ) %>% 
  mutate(
    Name = 
      case_when(
        Name == "Kuai Liang" ~ "Liang Kuai", 
        Name == "Jerry NA" ~ "Jerry",
        TRUE ~ Name),
  )

statistics <- 
  tbl(con, "gameDataPlayer") %>% 
  filter(
    Season == 7
  ) %>% 
  group_by(
    Name, 
    Club
  ) %>% 
  summarize(
    Apps = sum(Apps, na.rm = TRUE)
  ) %>% 
  collect()

check <- 
  players %>% 
  full_join(
    statistics,
    by = "Name"
  ) %>% 
  arrange(
    Team
  ) %>% 
  group_by(
    Team
  ) %>% 
  mutate(
    Apps = if_else(is.na(Apps), 0, Apps),
    Proportion = (Apps / max(Apps, na.rm = TRUE) )%>% round(3)
  ) %>% 
  mutate(
    Check = if_else(Proportion < 0.33, FALSE, TRUE)
  ) %>% 
  filter(
    Check == FALSE
  )

  
