
options(scipen = 999, dplyr.summarise.inform = FALSE)

#### Common variables used throughout the application ####

sslBlueL <- "#324f7e"
sslBlueD <- "#4b8dad"
sslGold <- "#BD9523"

red <- "#D96F68"
yellow <- "#F5D17E"
green <- "#66B38C"

traitSep <- " \\\\ "


## Loads common and small tables
tpeCost <- 
  portalQuery("SELECT * FROM tpetable")

attributes <- 
  portalQuery("SELECT * FROM attributes")

currentSeason <- 
  indexQuery("SELECT * FROM seasoninfo ORDER BY startDate DESC LIMIT 1")

statisticsLegend <- 
  indexQuery("SELECT * FROM statlegend")

editableAttributes <- 
  attributes$attribute %>% 
  .[!(. %in% c("Natural Fitness", "Stamina"))] %>% 
  str_to_title() %>% 
  str_remove_all(pattern = " ") 

sslNations <- 
  c(
    "",
    "Other" = "OTH",
    "Afghanistan" = "AFG",
    "Albania" = "ALB",
    "Algeria" = "DZA",
    "Andorra" = "AND",
    "Angola" = "AGO",
    "Antigua and Barbuda" = "ATG",
    "Argentina" = "ARG",
    "Armenia" = "ARM",
    "Australia" = "AUS",
    "Austria" = "AUT",
    "Azerbaijan" = "AZE",
    "Bahamas" = "BHS",
    "Bahrain" = "BHR",
    "Bangladesh" = "BGD",
    "Barbados" = "BRB",
    "Belarus" = "BLR",
    "Belgium" = "BEL",
    "Belize" = "BLZ",
    "Benin" = "BEN",
    "Bhutan" = "BTN",
    "Bolivia" = "BOL",
    "Bosnia and Herzegovina" = "BIH",
    "Botswana" = "BWA",
    "Brazil" = "BRA",
    "Brunei" = "BRN",
    "Bulgaria" = "BGR",
    "Burkina Faso" = "BFA",
    "Burundi" = "BDI",
    "Cabo Verde" = "CPV",
    "Cambodia" = "KHM",
    "Cameroon" = "CMR",
    "Canada" = "CAN",
    "Central African Republic" = "CAF",
    "Chad" = "TCD",
    "Chile" = "CHL",
    "China" = "CHN",
    "Colombia" = "COL",
    "Comoros" = "COM",
    "Côte d'Ivoire" = "CIV",
    "Democratic Republic of the Congo" = "COD",
    "Republic of the Congo" = "COG",
    "Costa Rica" = "CRI",
    "Croatia" = "HRV",
    "Cuba" = "CUB",
    "Cyprus" = "CYP",
    "Czechia" = "CZE",
    "Denmark" = "DNK",
    "Djibouti" = "DJI",
    "Dominica" = "DMA",
    "Dominican Republic" = "DOM",
    "East Timor" = "TLS",
    "England" = "ENG",
    "Ecuador" = "ECU",
    "Egypt" = "EGY",
    "El Salvador" = "SLV",
    "Equatorial Guinea" = "GNQ",
    "Eritrea" = "ERI",
    "Estonia" = "EST",
    "Eswatini" = "SWZ",
    "Ethiopia" = "ETH",
    "Faroe Islands" = "FRO",
    "Fiji" = "FJI",
    "Finland" = "FIN",
    "France" = "FRA",
    "Gabon" = "GAB",
    "Gambia" = "GMB",
    "Georgia" = "GEO",
    "Germany" = "DEU",
    "Ghana" = "GHA",
    "Gibraltar" = "GIB",
    "Greece" = "GRC",
    "Grenada" = "GRD",
    "Guatemala" = "GTM",
    "Guinea" = "GIN",
    "Guinea-Bissau" = "GNB",
    "Guyana" = "GUY",
    "Haiti" = "HTI",
    "Honduras" = "HND",
    "Hungary" = "HUN",
    "Iceland" = "ISL",
    "India" = "IND",
    "Indonesia" = "IDN",
    "Iran" = "IRN",
    "Iraq" = "IRQ",
    "Ireland" = "IRL",
    "Israel" = "ISR",
    "Italy" = "ITA",
    "Jamaica" = "JAM",
    "Japan" = "JPN",
    "Jordan" = "JOR",
    "Kazakhstan" = "KAZ",
    "Kenya" = "KEN",
    "Kiribati" = "KIR",
    "Korea, North" = "PRK",
    "Korea, South" = "KOR",
    "Kosovo" = "KOS",
    "Kuwait" = "KWT",
    "Kyrgyzstan" = "KGZ",
    "Laos" = "LAO",
    "Latvia" = "LVA",
    "Lebanon" = "LBN",
    "Lesotho" = "LSO",
    "Liberia" = "LBR",
    "Libya" = "LBY",
    "Liechtenstein" = "LIE",
    "Lithuania" = "LTU",
    "Luxembourg" = "LUX",
    "Madagascar" = "MDG",
    "Malawi" = "MWI",
    "Malaysia" = "MYS",
    "Maldives" = "MDV",
    "Mali" = "MLI",
    "Malta" = "MLT",
    "Marshall Islands" = "MHL",
    "Mauritania" = "MRT",
    "Mauritius" = "MUS",
    "Mexico" = "MEX",
    "Micronesia" = "FSM",
    "Moldova" = "MDA",
    "Monaco" = "MCO",
    "Mongolia" = "MNG",
    "Montenegro" = "MNE",
    "Morocco" = "MAR",
    "Mozambique" = "MOZ",
    "Myanmar" = "MMR",
    "Namibia" = "NAM",
    "Nauru" = "NRU",
    "Nepal" = "NPL",
    "Netherlands" = "NLD",
    "New Zealand" = "NZL",
    "Nicaragua" = "NIC",
    "Niger" = "NER",
    "Nigeria" = "NGA",
    "North Macedonia" = "MKD",
    "Northern Ireland" = "NIR",
    "Norway" = "NOR",
    "Oman" = "OMN",
    "Pakistan" = "PAK",
    "Palau" = "PLW",
    "Panama" = "PAN",
    "Papua New Guinea" = "PNG",
    "Paraguay" = "PRY",
    "Peru" = "PER",
    "Philippines" = "PHL",
    "Poland" = "POL",
    "Portugal" = "PRT",
    "Qatar" = "QAT",
    "Romania" = "ROU",
    "Russia" = "RUS",
    "Rwanda" = "RWA",
    "Saint Kitts and Nevis" = "KNA",
    "Saint Lucia" = "LCA",
    "Saint Vincent and the Grenadines" = "VCT",
    "Samoa" = "WSM",
    "San Marino" = "SMR",
    "Sao Tome and Principe" = "STP",
    "Saudi Arabia" = "SAU",
    "Scotland" = "SCO",
    "Senegal" = "SEN",
    "Serbia" = "SRB",
    "Seychelles" = "SYC",
    "Sierra Leone" = "SLE",
    "Singapore" = "SGP",
    "Slovakia" = "SVK",
    "Slovenia" = "SVN",
    "Solomon Islands" = "SLB",
    "Somalia" = "SOM",
    "South Africa" = "ZAF",
    "South Sudan" = "SSD",
    "Spain" = "ESP",
    "Sri Lanka" = "LKA",
    "Sudan" = "SDN",
    "Suriname" = "SUR",
    "Sweden" = "SWE",
    "Switzerland" = "CHE",
    "Syria" = "SYR",
    "Taiwan" = "TWN",
    "Tajikistan" = "TJK",
    "Tanzania" = "TZA",
    "Thailand" = "THA",
    "Togo" = "TGO",
    "Tonga" = "TON",
    "Trinidad and Tobago" = "TTO",
    "Tunisia" = "TUN",
    "Turkey" = "TUR",
    "Turkmenistan" = "TKM",
    "Tuvalu" = "TUV",
    "Uganda" = "UGA",
    "Ukraine" = "UKR",
    "United Arab Emirates" = "ARE",
    "United States" = "USA",
    "Uruguay" = "URY",
    "Uzbekistan" = "UZB",
    "Vanuatu" = "VUT",
    "Vatican City" = "VAT",
    "Venezuela" = "VEN",
    "Vietnam" = "VNM",
    "Wales" = "WAL",
    "Yemen" = "YEM",
    "Zambia" = "ZMB",
    "Zimbabwe" = "ZWE"
  )

# Define position options
positions <- c(
  "LD" = "Left Defender", "CD" = "Central Defender", "RD" = "Right Defender",
  "LWB" = "Left Wing Back", "CDM" = "Defensive Midfielder", "RWB" = "Right Wing Back",
  "LM" = "Left Midfielder", "CM" = "Central Midfielder", "RM" = "Right Midfielder",
  "LAM" = "Left Attacking Midfielder", "CAM" = "Central Attacking Midfielder", 
  "RAM" = "Right Attacking Midfielder", "ST" = "Striker"
)

positionsGK <- c(positions, "GK" = "Goalkeeper")

# Define trait options with optgroup
traits <- list(
  "Movement - On the Ball" = list(
    "Cuts Inside From Both Wings", "Knocks Ball Past Opponent", "Runs With Ball Rarely",
    "Runs With Ball Often", "Runs With Ball Down Left", "Runs With Ball Down Right",
    "Runs With Ball Through Centre", "Stops Play", "Brings Ball Out Of Defense"
  ),
  "Movement - Off the Ball" =list(
    "Arrives Late In Opponent's Area", "Comes Deep To Get Ball", "Gets Forward Whenever Possible",
    "Gets Into Opposition Area", "Hugs Line", "Likes To Try To Beat Offside Trap",
    "Moves Into Channels", "Plays One-Twos", "Plays With Back To Goal",
    "Does Not Move Into Channels", "Stays Back At All Times"
  ),
  "Passing" = list(
    "Dictates Tempo", "Likes To Switch Ball To Other Flank", "Looks For Pass Rather Than Attempting To Score",
    "Plays No Through Balls", "Plays Short Simple Passes", "Tries Killer Balls Often",
    "Tries Long Range Passes", "Crossing Early"
  ),
  "Finishing" = list(
    "Attempts Overhead Kicks", "Hits Free Kicks With Power",
    "Likes To Lob Keeper", "Likes To Round Keeper", "Places Shots", "Refrains From Taking Long Shots",
    "Shoots From Distance", "Shoots With Power", "Tries First Time Shots", "Tries Long Range Free Kicks"
  ),
  "Discipline" = list(
    "Argues With Officials", "Winds Up Opponents", "Gets Crowd Going"
  ),
  "Defending" = list(
    "Dives Into Tackles", "Does Not Dive Into Tackles", "Marks Opponent Tightly"
  ),
  "Technique" = list(
    "Avoids Using Weaker Foot", "Uses Outside Of Foot", "Tries Tricks", "Likes To Beat Opponent Repeatedly",
    "Curls Ball", "Possess Long Flat Throw", "Tries To Play Way Out Of Trouble"
  )
)

jsonTraits <- 
  c(
    "Runs With Ball Down Left",
    "Runs With Ball Down Right",
    "Runs With Ball Through Centre",
    "Gets Into Opposition Area",
    "Moves Into Channels",
    "Gets Forward Whenever Possible",
    "Plays Short Simple Passes",
    "Tries Killer Balls Often",
    "Shoots From Distance",
    "Shoots With Power",
    "Places Shots",
    "Curls Ball",
    "Likes To Round Keeper", 
    "Likes To Try To Beat Offside Trap",
    "Uses Outside Of Foot",
    "Marks Opponent Tightly",    
    "Winds Up Opponents",
    "Argues With Officials",
    "Plays With Back To Goal",
    "Comes Deep To Get Ball",
    "Plays One-Twos",
    "Likes To Lob Keeper",
    "Dictates Tempo",
    "Attempts Overhead Kicks",
    "Looks For Pass Rather Than Attempting To Score",
    "Plays No Through Balls",
    "Stops Play",
    "Knocks Ball Past Opponent",
    "Moves ball to right foot before dribble attempt",
    "Moves ball to left foot before dribble attempt",
    "Dwells on ball",
    "Arrives Late In Opponent's Area",
    "Tries To Play Way Out Of Trouble",
    "Stays Back At All Times",
    "Avoids Using Weaker Foot",
    "Tries Tricks",
    "Tries Long Range Free Kicks",
    "Dives Into Tackles",
    "Does Not Dive Into Tackles",
    "Cuts Inside From Both Wings",
    "Hugs Line",
    "Gets Crowd Going",
    "Tries First Time Shots",
    "Tries Long Range Passes",
    "Likes Ball Played To Feet",
    "Hits Free Kicks With Power",
    "Likes To Beat Opponent Repeatedly",
    "Likes To Switch Ball To Other Flank",
    "Will retire at top",
    "Will play football as long as possible",
    "Possess Long Flat Throw",
    "Runs With Ball Often",
    "Runs With Ball Rarely",
    "Attempts to develop weaker foot",
    "Does Not Move Into Channels",
    "Uses long throw to start counter attack",
    "Refrains From Taking Long Shots",
    "Cuts inside from left wing",
    "Cuts inside from right wing",
    "Crosses Early",
    "Brings Ball Out Of Defense",
    "Plays ball with feet"
  )




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


