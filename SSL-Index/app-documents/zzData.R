
## Loads common and small tables
tpeCost <- 
  portalQuery("SELECT * FROM tpetable")

attributes <- 
  portalQuery("SELECT * FROM attributes")

currentSeason <- 
  indexQuery("SELECT * FROM seasoninfo ORDER BY startDate DESC LIMIT 1")

statisticsLegend <- 
  indexQuery("SELECT * FROM statlegend")

## Functions for tooltips
withTooltip <- function (label, tooltip) 
{
  tags$span(
    class = "hovertext", 
    data_hover = tooltip, 
    label
    ) %>% 
    as.character() %>% 
    tooltipHTML()
}

tooltipHTML <- function (text, ...) 
{
  if (inherits(text, "shiny.tag.list")) {
    text <- as.character(text)
  }
  htmltools::HTML(text, ...)
}

# 
# ##################################################################
# ##           Loading tables with information                    ##
# ##################################################################
# 
# ## Doesn't necessarily have to load here. Need to check when it is being used.
# positionalCoord <- 
#   dbGetQuery(con, "SELECT * FROM positionalCoord")
# 
# 
# ## These are only used when scraping data
# roleMatrix <- 
#   dbGetQuery(con, "SELECT * from Duty_and_Role_Matrix")
# 
# abilityMatrix <- 
#   dbGetQuery(con, "SELECT * from Current_Ability_Calculation_Matrix")
# 
# ## These are used when grouping the attributes in the player builder and update tool
# attributes <- 
#   dbGetQuery(con, "SELECT * from Attributes_And_Availability") %>% 
#   mutate(
#     abbr = 
#       c(
#         "Acc", "Agi", "Bal", "Jum", "Nat", "Pac", "Sta", "Str",
#         "Cor", "Cro", "Dri", "Fin", "Fir", "Fre", "Hea", "Lon", "L Th", "Mar", "Pas" , "Pen", "Tck", "Tec",
#         "Agg", "Ant", "Bra", "Cmp", "Cnt", "Dec", "Det", "Fla", "Ldr", "OtB", "Pos", "Tea", "Vis", "Wor",
#         "Aer", "Cmd", "Com", "Ecc", "Han", "Kic", "1v1", "Pun", "Ref", "TRO", "Thr"
#       )
#   )
# 
# 
# #################################################################
# ##                       Attribute names                       ##
# #################################################################
# 
# attributeNames <- 
#   c(
#     "Aggression",
#     "Anticipation",
#     "Bravery",
#     "Composure",
#     "Concentration",
#     "Decisions",
#     "Determination",
#     "Flair",
#     "Leadership",
#     "OffTheBall",
#     "Positioning",
#     "Teamwork",
#     "Vision",
#     "WorkRate",
#     "Acceleration",
#     "Agility",
#     "Balance",
#     "JumpingReach",
#     "NaturalFitness",
#     "Pace",
#     "Stamina",
#     "Strength",
#     "Corners",
#     "Crossing",
#     "Dribbling",
#     "Finishing",
#     "FirstTouch",
#     "FreeKick",
#     "Heading",
#     "LongShots",
#     "LongThrows",
#     "Marking",
#     "Passing",
#     "PenaltyTaking",
#     "Tackling",
#     "Technique"
#   )
# 
# goalieAttributeNames <- 
#   c(
#     "GoalieAggression",
#     "GoalieAnticipation",
#     "GoalieBravery",
#     "GoalieComposure",
#     "GoalieConcentration",
#     "GoalieDecisions",
#     "GoalieDetermination",
#     "GoalieFlair",
#     "GoalieLeadership",
#     "GoalieOffTheBall",
#     "GoaliePositioning",
#     "GoalieTeamwork",
#     "GoalieVision",
#     "GoalieWorkRate",
#     "GoalieAcceleration",
#     "GoalieAgility",
#     "GoalieBalance",
#     "GoalieJumpingReach",
#     "GoalieNaturalFitness",
#     "GoaliePace",
#     "GoalieStamina",
#     "GoalieStrength",
#     "GoalieFirstTouch",
#     "GoalieFreeKick",
#     "GoaliePassing",
#     "GoaliePenaltyTaking",
#     "GoalieTechnique",
#     "GoalieAerialReach",
#     "GoalieCommandOfArea",
#     "GoalieCommunication",
#     "GoalieEccentricity",
#     "GoalieHandling",
#     "GoalieKicking",
#     "GoalieOneOnOnes",
#     "GoalieTendencyToPunch",
#     "GoalieTendencyToRush",
#     "GoalieReflexes",
#     "GoalieThrowing"
#   )
# 
# 
# ##################################################################
# ##             Loading data sets from Google Sheets             ##
# ##################################################################
# 
# teamInfo <-
#   dbGetQuery(con, "SELECT * from Team_Information")
# 
# pitch <- 
#   try(
#     image_read_svg(
#       path = "https://raw.githubusercontent.com/canadice/ssl-index/main/graphics/pitch.svg",
#       width = 750
#     ),
#     silent = TRUE
#   )
# 
# playerData <- 
#   dbGetQuery(con, "SELECT * from Daily_Scrape") %>%
#   group_by(Name) %>% 
#   filter(!is.na(Class)) %>% 
#   dplyr::mutate(
#     DEFENDING =
#       sum(c(Marking,Tackling,Positioning) %>% replace_na(5))/3,
#     PHYSICAL =
#       sum(c(Agility, Balance, Stamina, Strength) %>% replace_na(5))/4,
#     SPEED =
#       sum(c(Acceleration, Pace)%>% replace_na(5))/2,
#     VISION =
#       sum(c(Passing, Flair , Vision)%>% replace_na(5))/3,
#     ATTACKING =
#       sum(c(Finishing , Composure , `Off the Ball`)%>% replace_na(5))/3,
#     TECHNICAL =
#       sum(c(Dribbling, `First Touch`, Technique)%>% replace_na(5))/3,
#     AERIAL =
#       sum(c(Heading , `Jumping Reach`)%>% replace_na(5))/2,
#     MENTAL =
#       sum(c(Anticipation, Bravery, Concentration, Decisions, Determination, Teamwork)%>% replace_na(5))/6
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     Created = as.Date(Created, origin = "1970-01-01"),
#     lastPost = as.Date(lastPost, origin = "1970-01-01")
#   )
# 
# ## Loads game data
# keeperGameData <- 
#   dbGetQuery(con, "SELECT * from gameDataKeeper")
# 
# playerGameData <- 
#   dbGetQuery(con, "SELECT * from gameDataPlayer")
# 
# rosterAudit <- 
#   dbGetQuery(con, "SELECT * from rosterAudit")


