#* @apiTitle Player API
#* @apiDescription Endpoints to get player information.

#* Allows acces from cross domain places
# Enable CORS Filtering
#' @filter cors
cors <- function(req, res) {
  safe_domains <- c("https://api.simulationsoccer.com", 
                    "https://forum.simulationsoccer.com",
                    "https://index.simulationsoccer.com")
  
  if (any(grepl(pattern = paste0(safe_domains,collapse="|"), req$HTTP_REFERER,ignore.case=T))) {
    res$setHeader("Access-Control-Allow-Origin", sub("/$","",req$HTTP_REFERER)) #Have to remove last slash, for some reason
    
    if (req$REQUEST_METHOD == "OPTIONS") {
      res$setHeader("Access-Control-Allow-Methods","GET,HEAD,PUT,PATCH,POST,DELETE") #This is how node.js does it
      res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
      res$status <- 200
      return(list())
    } else {
      plumber::forward()
    }
  } else {
    plumber::forward()
  }
}


#* Get all players from the portal database
#* @get /getAllPlayers
#* @serializer json
#* 
#* 
function() {
  portalQuery(
    paste(
      "SELECT pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
      pd.created, pd.tpe, pd.tpeused, pd.tpebank, t.name AS team, pd.affiliate, pd.birthplace, 
      pd.nationality, pd.height, pd.weight, pd.hair_color, pd.hair_length, pd.skintone, 
      pd.render, pd.`left foot`, pd.`right foot`, pd.position, pd.pos_st, pd.pos_lam, 
      pd.pos_cam, pd.pos_ram, pd.pos_lm, pd.pos_cm, pd.pos_rm, pd.pos_lwb, pd.pos_cdm,
      pd.pos_rwb, pd.pos_ld, pd.pos_cd, pd.pos_rd, pd.pos_gk, pd.acceleration, pd.agility,
      pd.balance, pd.`jumping reach`, pd.`natural fitness`, pd.pace, pd.stamina, pd.strength, 
      pd.corners, pd.crossing, pd.dribbling, pd.finishing, pd.`first touch`, pd.`free kick`, 
      pd.heading, pd.`long shots`, pd.`long throws`, pd.marking, pd.passing, pd.`penalty taking`, 
      pd.tackling, pd.technique, pd.aggression, pd.anticipation, pd.bravery, pd.composure, 
      pd.concentration, pd.decisions, pd.determination, pd.flair, pd.leadership, pd.`off the ball`, 
      pd.positioning, pd.teamwork, pd.vision, pd.`work rate`, pd.`aerial reach`, pd.`command of area`, 
      pd.communication, pd.eccentricity, pd.handling, pd.kicking, pd.`one on ones`, pd.reflexes, 
      pd.`tendency to rush`, pd.`tendency to punch`, pd.throwing, pd.traits, pd.rerollused, pd.redistused,
      mb.username, us.desc AS `userStatus`, ps.desc AS `playerStatus`, 
            CASE 
              WHEN pd.tpe <= 350 THEN 1000000
              WHEN pd.tpe BETWEEN 351 AND 500 THEN 1500000
              WHEN pd.tpe BETWEEN 501 AND 650 THEN 2000000
              WHEN pd.tpe BETWEEN 651 AND 800 THEN 2500000
              WHEN pd.tpe BETWEEN 801 AND 950 THEN 3000000
              WHEN pd.tpe BETWEEN 951 AND 1100 THEN 3500000
              WHEN pd.tpe BETWEEN 1101 AND 1250 THEN 4000000
              WHEN pd.tpe BETWEEN 1251 AND 1400 THEN 4500000
              WHEN pd.tpe BETWEEN 1401 AND 1550 THEN 5000000
              WHEN pd.tpe BETWEEN 1551 AND 1700 THEN 5500000
              WHEN pd.tpe > 1700 THEN 6000000
              ELSE NULL
          END AS `minimum salary`
        FROM playerdata pd
        LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
        LEFT JOIN useractivity ua ON pd.uid = ua.uid
        LEFT JOIN userstatuses us ON ua.status_u = us.status
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
        LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
        WHERE pd.status_p >= 0
        ORDER BY pd.name;"
    )
  ) %>% 
    mutate(
      across(where(is.numeric), ~replace_na(.x, 5))
    )
}

#* Get single players from the portal database, only one of `name` or `pid` should be used
#* @get /getPlayer
#* @serializer json
#* @param name:str The player name
#* @param pid:int The player ID
#* @param username:str The username
#* 
function(name = NULL, pid = NULL, username = NULL) {
  if(all(name %>% is.null(), pid %>% is.null(), username %>% is.null())){
    return("You need to specify at least one of the arguments!")
  }
  
  if(!(username %>% is.null())){
    whereClause <- paste("WHERE mb.username = ", paste0("'", username, "'"), "ORDER BY pid DESC LIMIT 1;")
  } else {
    whereClause <- if_else(name %>% is.null(), paste("WHERE pd.pid = ", pid, ";"), paste("WHERE pd.name = ", paste0("'", name, "'"), ";"))
  }
  
  data <- 
    portalQuery(
      paste(
        "SELECT pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
        pd.created, pd.tpe, pd.tpeused, pd.tpebank, pd.team AS organization, t.name AS team, pd.affiliate, pd.birthplace, 
        pd.nationality, pd.height, pd.weight, pd.hair_color, pd.hair_length, pd.skintone, 
        pd.render, pd.`left foot`, pd.`right foot`, pd.position, pd.pos_st, pd.pos_lam, 
        pd.pos_cam, pd.pos_ram, pd.pos_lm, pd.pos_cm, pd.pos_rm, pd.pos_lwb, pd.pos_cdm,
        pd.pos_rwb, pd.pos_ld, pd.pos_cd, pd.pos_rd, pd.pos_gk, pd.acceleration, pd.agility,
        pd.balance, pd.`jumping reach`, pd.`natural fitness`, pd.pace, pd.stamina, pd.strength, 
        pd.corners, pd.crossing, pd.dribbling, pd.finishing, pd.`first touch`, pd.`free kick`, 
        pd.heading, pd.`long shots`, pd.`long throws`, pd.marking, pd.passing, pd.`penalty taking`, 
        pd.tackling, pd.technique, pd.aggression, pd.anticipation, pd.bravery, pd.composure, 
        pd.concentration, pd.decisions, pd.determination, pd.flair, pd.leadership, pd.`off the ball`, 
        pd.positioning, pd.teamwork, pd.vision, pd.`work rate`, pd.`aerial reach`, pd.`command of area`, 
        pd.communication, pd.eccentricity, pd.handling, pd.kicking, pd.`one on ones`, pd.reflexes, 
        pd.`tendency to rush`, pd.`tendency to punch`, pd.throwing, pd.traits, pd.rerollused, pd.redistused,
        mb.username, us.desc AS `userStatus`, ps.desc AS `playerStatus`, 
              CASE 
                WHEN pd.tpe <= 350 THEN 1000000
                WHEN pd.tpe BETWEEN 351 AND 500 THEN 1500000
                WHEN pd.tpe BETWEEN 501 AND 650 THEN 2000000
                WHEN pd.tpe BETWEEN 651 AND 800 THEN 2500000
                WHEN pd.tpe BETWEEN 801 AND 950 THEN 3000000
                WHEN pd.tpe BETWEEN 951 AND 1100 THEN 3500000
                WHEN pd.tpe BETWEEN 1101 AND 1250 THEN 4000000
                WHEN pd.tpe BETWEEN 1251 AND 1400 THEN 4500000
                WHEN pd.tpe BETWEEN 1401 AND 1550 THEN 5000000
                WHEN pd.tpe BETWEEN 1551 AND 1700 THEN 5500000
                WHEN pd.tpe > 1700 THEN 6000000
                ELSE NULL
            END AS `minimum salary`
          FROM playerdata pd
          LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
          LEFT JOIN useractivity ua ON pd.uid = ua.uid
          LEFT JOIN userstatuses us ON ua.status_u = us.status
          LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
          LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate",
          whereClause
      )
    ) %>% 
    mutate(
      across(where(is.numeric), ~replace_na(.x, 5))
    )
  
  if(data %>% nrow() < 1){
    return("No player found.")
  }
  
  return(data)
}

#* Get draft class leaderboards
#* @get /getDraftClass
#* @serializer json
#* @param class The class leaderboard
#* 
function(class = NULL) {
  # If no class is given it defaults to the youngest
  if(class %>% is.null()){
    class <- indexQuery("SELECT season FROM seasoninfo ORDER BY season DESC LIMIT 1;") %>% unlist() + 1
  }
  
  portalQuery(
    paste(
      "SELECT pd.name, pd.tpe, t.abbreviation AS team, mb.username, us.desc AS `userStatus`, ps.desc AS `playerStatus`
        FROM playerdata pd
        LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
        LEFT JOIN useractivity ua ON pd.uid = ua.uid
        LEFT JOIN userstatuses us ON ua.status_u = us.status
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
        LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
      WHERE pd.class = ", paste0("'S", class, "'"), " AND pd.status_p > 0
      ORDER BY pd.tpe DESC;"
    )
  )
}

#* Get Activity Check History
#* @get /acHistory
#* @serializer json
#* 
function(){
  portalQuery(
    "SELECT 
        YEAR(FROM_UNIXTIME(time)) AS year,
        WEEK(FROM_UNIXTIME(time), 3) AS week,
        COUNT(*) AS count
    FROM tpehistory
    WHERE source = 'Activity Check'
    GROUP BY year, week
    ORDER BY year, week;"
  )
}