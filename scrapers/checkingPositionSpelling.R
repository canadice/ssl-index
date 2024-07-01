posTraits <- 
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

temp <- 
  playerData %>% 
  mutate(
    vecTraits = `All Traits` %>% str_to_title() %>% str_split(" \\\\ ")
  ) %>% 
  filter(
    lapply(
      vecTraits, 
      FUN = function(x) any(!(x %in% posTraits))
    ) %>% 
      unlist()
  ) %>% 
  filter(
    Team != "Retired",
    Position != "Goalkeeper"
  ) %>% 
  mutate(
    test = 
      lapply(
        vecTraits, 
        FUN = function(x) which(!(x %in% posTraits)) %>% paste0()
      )
  ) %>% 
  select(
    Name,,
    Team,
    `All Traits`,
    test
  ) 
  



