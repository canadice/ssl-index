## Loading data


##################################################################
##                    Positional Coordinates                    ##
##################################################################
{positionalCoord <- 
  data.frame(
    x = 
      c(
        375, 
        130,375,620,
        130,375,620,
        130,375,620,
        130,375,620,
        375
      ),
    y = 
      c(
        775,
        625,625,625,
        455,455,455,
        310,310,310,
        150,150,150,
        50
      ),
    Position = 
      c(
        "Striker",
        "Attacking Midfielder L",
        "Attacking Midfielder C",
        "Attacking Midfielder R",
        "Midfielder L",
        "Midfielder C",
        "Midfielder R",
        "Wingback L",
        "Defensive Midfielder",
        "Wingback R",
        "Defender L",
        "Defender C",
        "Defender R",
        "Goalkeeper"
      )
  )
}
#################################################################
##                       TPE Cost Matrix                       ##
#################################################################

tpeCost <- 
  data.frame(
    value = 5:20,
    ## -2 comes from an initial error in the costs as 5 is free (starting value)
    cost = c(2,4,6,10,14,18,24,30,36,48,60,72,90,108,133,158)-2
  )
#################################################################
##                       Attribute names                       ##
#################################################################

attributeNames <- 
  c(
    "Aggression",
    "Anticipation",
    "Bravery",
    "Composure",
    "Concentration",
    "Decisions",
    "Determination",
    "Flair",
    "Leadership",
    "OffTheBall",
    "Positioning",
    "Teamwork",
    "Vision",
    "WorkRate",
    "Acceleration",
    "Agility",
    "Balance",
    "JumpingReach",
    "NaturalFitness",
    "Pace",
    "Stamina",
    "Strength",
    "Corners",
    "Crossing",
    "Dribbling",
    "Finishing",
    "FirstTouch",
    "FreeKick",
    "Heading",
    "LongShots",
    "LongThrows",
    "Marking",
    "Passing",
    "PenaltyTaking",
    "Tackling",
    "Technique"
  )

goalieAttributeNames <- 
  c(
    "GoalieAggression",
    "GoalieAnticipation",
    "GoalieBravery",
    "GoalieComposure",
    "GoalieConcentration",
    "GoalieDecisions",
    "GoalieDetermination",
    "GoalieFlair",
    "GoalieLeadership",
    "GoalieOffTheBall",
    "GoaliePositioning",
    "GoalieTeamwork",
    "GoalieVision",
    "GoalieWorkRate",
    "GoalieAcceleration",
    "GoalieAgility",
    "GoalieBalance",
    "GoalieJumpingReach",
    "GoalieNaturalFitness",
    "GoaliePace",
    "GoalieStamina",
    "GoalieStrength",
    "GoalieFirstTouch",
    "GoalieFreeKick",
    "GoaliePassing",
    "GoaliePenaltyTaking",
    "GoalieTechnique",
    "GoalieAerialReach",
    "GoalieCommandOfArea",
    "GoalieCommunication",
    "GoalieEccentricity",
    "GoalieHandling",
    "GoalieKicking",
    "GoalieOneOnOnes",
    "GoalieTendencyToPunch",
    "GoalieTendencyToRush",
    "GoalieReflexes",
    "GoalieThrowing"
  )


##################################################################
##             Loading data sets from Google Sheets             ##
##################################################################

googlesheets4::gs4_deauth()

teamInfo <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
    sheet = "Team Information"
  )

pitch <- 
  try(
    image_read_svg(
      path = "https://raw.githubusercontent.com/canadice/ssl-index/main/graphics/pitch.svg"
    ),
    silent = TRUE
  )
  

playerData <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
    sheet = "Daily Scrape"
  )

roleMatrix <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
    sheet = "Duty and Role Matrix"
  )

abilityMatrix <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
    sheet = "Current Ability Calculation Matrix"
  )

attributes <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
    sheet = "Attributes and Availability"
  )





