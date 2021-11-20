## Loading data

tpeCost <- 
  data.frame(
    value = 5:20,
    cost = c(2,4,6,10,14,18,24,30,36,48,60,72,90,108,132,157)-2
  )


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

teams <- 
  data.frame(
    team = 
      c(
        "Athênai F.C.",
        "Cairo City F.C.",
        "Football Club de Rio",
        "Hollywood FC",
        "Inter London FC",
        "Tokyo S.C."
      )
  )

pitch <- 
  image_read_svg(
    path = "graphics/pitch.svg"
  )

googlesheets4::gs4_deauth()

playerData <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/167RCPHiZYryXxvkl-Y5dSnRul04WANqSfN6CgGwVB8Y/edit?usp=sharing",
    sheet = "Daily Scrape"
  )
