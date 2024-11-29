require(tidyverse)
require(sslrtools)

schedule <- readAPI("https://api.simulationsoccer.com/index/schedule", query = list(season = "ALL", league = "ALL")) %>% 
  mutate(
    Home = case_when(
      Home == "Sydney City" ~ "Reykjavik United",
      Home == "FC Rio" ~ "Seoul MFC",
      Home == "Adowa Accra FC" ~ "Schwarzwälder FV",
      Home == "Red Star Laos" ~ "North Shore United",
      TRUE ~ Home
    ),
    Away = case_when(
      Away == "Sydney City" ~ "Reykjavik United",
      Away == "FC Rio" ~ "Seoul MFC",
      Away == "Adowa Accra FC" ~ "Schwarzwälder FV",
      Away == "Red Star Laos" ~ "North Shore United",
      TRUE ~ Away
    )
  ) %>% 
  filter(
    !(HomeScore %>% is.na())
  )

teams <- tibble(teams = c(schedule$Home, schedule$Away) %>% unique(),
                time = 0,
                ELO = 1200)

# Define the weight function
getWeight <- function(MatchDay, MatchType) {
  case_when(
    MatchDay == "Pre" ~ 10,
    MatchDay == "Shi" ~ 20,
    MatchType %in% 0:1 ~ 40,
    MatchType == 2 ~ 30,
    TRUE ~ 10
  )
}

# Calculate ELOs sequentially using a loop
updateEloSequential <- function(schedule, teams) {
  # Copy the ELO table for updates
  teamElos <- teams
  
  # Add columns to store ELO results
  schedule <- schedule %>%
    mutate(
      homeElo = NA_real_,
      awayElo = NA_real_,
      newHomeElo = NA_real_,
      newAwayElo = NA_real_,
      eloChange = NA_real_
    )
  
  # Loop through matches sequentially
  for (i in seq_len(nrow(schedule))) {
    # Extract current match details
    match <- schedule[i, ]
    homeTeam <- match$Home
    awayTeam <- match$Away
    
    # Get current ELOs
    homeElo <- teamElos$ELO[teamElos$teams == homeTeam]
    awayElo <- teamElos$ELO[teamElos$teams == awayTeam]
    
    # Calculate expected outcome
    eloDiff <- homeElo - awayElo
    expectedOutcome <- 1 / (10^(-eloDiff / 600) + 1)
    
    # Determine match outcome
    result <- case_when(
      match$HomeScore > match$AwayScore ~ 1,
      match$HomeScore < match$AwayScore ~ 0,
      match$HomeScore == match$AwayScore & match$Penalties > 0 ~ 0.75,
      TRUE ~ 0.5
    )
    
    # Get match weight
    weight <- getWeight(match$MatchDay, match$MatchType)
    
    # Calculate ELO change
    eloChange <- weight * (result - expectedOutcome)
    
    # Update team ELOs
    newHomeElo <- homeElo + eloChange
    newAwayElo <- awayElo - eloChange
    teamElos$ELO[teamElos$teams == homeTeam] <- newHomeElo
    teamElos$ELO[teamElos$teams == awayTeam] <- newAwayElo
    
    # Store the results in the schedule
    schedule$homeElo[i] <- homeElo
    schedule$awayElo[i] <- awayElo
    schedule$newHomeElo[i] <- newHomeElo
    schedule$newAwayElo[i] <- newAwayElo
    schedule$eloChange[i] <- eloChange
  }
  
  return(list(schedule = schedule, elo = teamElos))
}

# Run the calculation
results <- updateEloSequential(schedule, teams)

scheduleWithELO <- results$schedule
currentELO <- results$elo
