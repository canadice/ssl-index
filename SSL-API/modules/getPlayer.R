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
#* @param active:boolean Filters out retired players
#* 
function(active = FALSE) {
  portalQuery(
    query = 
      "SELECT 
       pd.uid, 
       pd.pid, 
       pd.status_p, 
       pd.first, 
       pd.last, 
       pd.name, 
       pd.class, 
       pd.created, 
       pd.tpe, 
       pd.tpeused, 
       pd.tpebank, 
       t.name AS team, 
       pd.affiliate, 
       pd.birthplace, 
       -- Check if nationality is 3 letters and map it to the full name from portaldb.nationality, else show pd.nationality
       CASE 
         WHEN LENGTH(pd.nationality) = 3 THEN n.name
         ELSE pd.nationality 
       END AS nationality,
       pd.height, 
       pd.weight, 
       pd.hair_color, 
       pd.hair_length, 
       pd.skintone, 
       pd.render, 
       pd.`left foot`, 
       pd.`right foot`, 
       pd.position, 
       pd.pos_st, 
       pd.pos_lam, 
       pd.pos_cam, 
       pd.pos_ram, 
       pd.pos_lm, 
       pd.pos_cm, 
       pd.pos_rm, 
       pd.pos_lwb, 
       pd.pos_cdm, 
       pd.pos_rwb, 
       pd.pos_ld, 
       pd.pos_cd, 
       pd.pos_rd, 
       pd.pos_gk, 
       pd.acceleration, 
       pd.agility, 
       pd.balance, 
       pd.`jumping reach`, 
       pd.`natural fitness`, 
       pd.pace, 
       pd.stamina, 
       pd.strength, 
       pd.corners, 
       pd.crossing, 
       pd.dribbling, 
       pd.finishing, 
       pd.`first touch`, 
       pd.`free kick`, 
       pd.heading, 
       pd.`long shots`, 
       pd.`long throws`, 
       pd.marking, 
       pd.passing, 
       pd.`penalty taking`, 
       pd.tackling, 
       pd.technique, 
       pd.aggression, 
       pd.anticipation, 
       pd.bravery, 
       pd.composure, 
       pd.concentration, 
       pd.decisions, 
       pd.determination, 
       pd.flair, 
       pd.leadership, 
       pd.`off the ball`, 
       pd.positioning, 
       pd.teamwork, 
       pd.vision, 
       pd.`work rate`, 
       pd.`aerial reach`, 
       pd.`command of area`, 
       pd.communication, 
       pd.eccentricity, 
       pd.handling, 
       pd.kicking, 
       pd.`one on ones`, 
       pd.reflexes, 
       pd.`tendency to rush`, 
       pd.`tendency to punch`, 
       pd.throwing, 
       pd.traits, 
       pd.rerollused, 
       pd.redistused, 
       mb.username, 
       mbuf.fid4 AS discord, 
       us.desc AS `userStatus`, 
       ps.desc AS `playerStatus`, 
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
       END AS `minimum salary`,
       SUM(CASE WHEN bt.status = 1 THEN bt.transaction ELSE 0 END) AS bankBalance,
       n.region,
       o.name AS organization
     FROM playerdata pd
     LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
     LEFT JOIN useractivity ua ON pd.uid = ua.uid
     LEFT JOIN userstatuses us ON ua.status_u = us.status
     LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
     LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
     LEFT JOIN mybbdb.mybb_userfields mbuf ON pd.uid = mbuf.ufid
     LEFT JOIN portaldb.nationality n ON pd.nationality = n.abbreviation OR pd.nationality = n.name
     LEFT JOIN banktransactions bt ON pd.pid = bt.pid
     LEFT JOIN organizations o ON pd.team = o.id
     WHERE pd.status_p >= ?active
     GROUP BY pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
              pd.created, pd.tpe, pd.tpeused, pd.tpebank, t.name, pd.affiliate, pd.birthplace, 
              n.name, pd.height, pd.weight, pd.hair_color, pd.hair_length, pd.skintone, 
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
              mb.username, mbuf.fid4, us.desc, ps.desc, n.region
     ORDER BY pd.created;",
    active = if_else(active == "true", 1, 0)
  ) %>% 
    mutate(
      across(where(is.numeric), ~replace_na(.x, 5))
    )
}

#* Get all players from the portal database
#* @get /getUpdatedBuilds
#* @serializer json
#* 
function() {
  portalQuery(
    paste(
      "SELECT pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
      pd.created, pd.tpe, pd.tpeused, pd.tpebank, t.name AS team, pd.affiliate, pd.birthplace, 
      -- Check if nationality is 3 letters and map it to the full name from portaldb.nationality, else show pd.nationality
        CASE 
            WHEN LENGTH(pd.nationality) = 3 THEN n.name
            ELSE pd.nationality 
        END AS nationality,
      pd.height, pd.weight, pd.hair_color, pd.hair_length, pd.skintone, 
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
      mb.username, mbuf.fid4 AS discord, us.desc AS `userStatus`, ps.desc AS `playerStatus`, 
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
          END AS `minimum salary`,
          SUM(CASE WHEN bt.status = 1 THEN bt.transaction ELSE 0 END) AS bankBalance,
          n.region,
          o.name AS organization
        FROM weeklybuilds pd
        LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
        LEFT JOIN useractivity ua ON pd.uid = ua.uid
        LEFT JOIN userstatuses us ON ua.status_u = us.status
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
        LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
        LEFT JOIN mybbdb.mybb_userfields mbuf ON pd.uid = mbuf.ufid
        LEFT JOIN portaldb.nationality n ON pd.nationality = n.abbreviation OR pd.nationality = n.name
        LEFT JOIN banktransactions bt ON pd.pid = bt.pid
        LEFT JOIN organizations o ON pd.team = o.id
        GROUP BY pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
         pd.created, pd.tpe, pd.tpeused, pd.tpebank, t.name, pd.affiliate, pd.birthplace, 
         n.name, pd.height, pd.weight, pd.hair_color, pd.hair_length, pd.skintone, 
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
         mb.username, mbuf.fid4, us.desc, ps.desc, n.region
        ORDER BY pd.created;"
    )
  ) %>% 
    mutate(
      across(where(is.numeric), ~replace_na(.x, 5))
    )
}

#* Get single players from the portal database, only one of `name` and `pid` or `username` and `uid` should be used at the same time
#* @get /getPlayer
#* @serializer json
#* @param name:str The player name
#* @param pid:int The player ID
#* @param username:str The username
#* @param uid:int The user ID
#* 
function(name = NULL, pid = NULL, username = NULL, uid = NULL) {
  
  # If no identifier was provided, return an error message.
  if (all(is.null(name), is.null(pid), is.null(username), is.null(uid))) {
    return("You need to specify at least one of the arguments!")
  }
  
  # Build the static portion of the query.
  baseQuery <- "
    SELECT 
      pd.uid,
      pd.pid,
      pd.status_p,
      pd.first,
      pd.last,
      pd.name,
      pd.class,
      pd.created,
      pd.tpe,
      pd.tpeused,
      pd.tpebank,
      pd.team AS organization,
      t.name AS team,
      pd.affiliate,
      pd.birthplace,
      -- Map a 3-letter nationality to its full name
      CASE 
          WHEN LENGTH(pd.nationality) = 3 THEN n.name
          ELSE pd.nationality 
      END AS nationality,
      pd.height,
      pd.weight,
      pd.hair_color,
      pd.hair_length,
      pd.skintone,
      pd.render,
      pd.`left foot`,
      pd.`right foot`,
      pd.position,
      pd.pos_st,
      pd.pos_lam,
      pd.pos_cam,
      pd.pos_ram,
      pd.pos_lm,
      pd.pos_cm,
      pd.pos_rm,
      pd.pos_lwb,
      pd.pos_cdm,
      pd.pos_rwb,
      pd.pos_ld,
      pd.pos_cd,
      pd.pos_rd,
      pd.pos_gk,
      pd.acceleration,
      pd.agility,
      pd.balance,
      pd.`jumping reach`,
      pd.`natural fitness`,
      pd.pace,
      pd.stamina,
      pd.strength,
      pd.corners,
      pd.crossing,
      pd.dribbling,
      pd.finishing,
      pd.`first touch`,
      pd.`free kick`,
      pd.heading,
      pd.`long shots`,
      pd.`long throws`,
      pd.marking,
      pd.passing,
      pd.`penalty taking`,
      pd.tackling,
      pd.technique,
      pd.aggression,
      pd.anticipation,
      pd.bravery,
      pd.composure,
      pd.concentration,
      pd.decisions,
      pd.determination,
      pd.flair,
      pd.leadership,
      pd.`off the ball`,
      pd.positioning,
      pd.teamwork,
      pd.vision,
      pd.`work rate`,
      pd.`aerial reach`,
      pd.`command of area`,
      pd.communication,
      pd.eccentricity,
      pd.handling,
      pd.kicking,
      pd.`one on ones`,
      pd.reflexes,
      pd.`tendency to rush`,
      pd.`tendency to punch`,
      pd.throwing,
      pd.traits,
      pd.rerollused,
      pd.redistused,
      mb.username,
      us.desc AS `userStatus`,
      ps.desc AS `playerStatus`,
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
      END AS `minimum salary`,
      pd.timesregressed
    FROM playerdata pd
    LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
    LEFT JOIN useractivity ua ON pd.uid = ua.uid
    LEFT JOIN userstatuses us ON ua.status_u = us.status
    LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
    LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
    LEFT JOIN portaldb.nationality n ON pd.nationality = n.abbreviation"
  
  # Determine the WHERE clause and parameters based on which identifiers are provided.
  if (!is.null(username)) {
    # If username is provided, search based on mb.username.
    whereClause <- " WHERE mb.username = ?username ORDER BY pid DESC LIMIT 1;"
    params <- list(username = username)
    
  } else if (!is.null(uid)) {
    # If uid is provided, search based on pd.uid.
    whereClause <- " WHERE pd.uid = ?uid ORDER BY pid DESC LIMIT 1;"
    params <- list(uid = uid)
    
  } else {
    # Otherwise use either name or pid.
    if (is.null(name)) {
      # Use pid if name is not provided.
      whereClause <- " WHERE pd.pid = ?pid;"
      params <- list(pid = pid)
    } else {
      # Use name.
      whereClause <- " WHERE pd.name = ?name;"
      params <- list(name = name)
    }
  }
  
  # Combine the base query and the WHERE clause.
  fullQuery <- paste0(baseQuery, whereClause)
  
  # Execute the parameterized query.
  data <- portalQuery(query = fullQuery, !!!params) %>% 
    mutate(across(where(is.numeric), ~ replace_na(.x, 5)))
  
  if(nrow(data) < 1) {
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
  # If no class is provided, default to the youngest (default season + 1)
  if (is.null(class)) {
    class <- indexQuery("SELECT season FROM seasoninfo ORDER BY season DESC LIMIT 1;") %>% unlist() + 1
  }
  
  # Prepend "S" to class value to form the search string.
  myclass <- paste0("S", class)
  
  portalQuery(
    query = "
      SELECT 
        pd.name, 
        pd.tpe, 
        t.name AS team, 
        mb.username, 
        us.desc AS `userStatus`, 
        ps.desc AS `playerStatus`, 
        pd.position, 
        SUM(bt.transaction) AS bankBalance 
      FROM playerdata pd
      LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
      LEFT JOIN useractivity ua ON pd.uid = ua.uid
      LEFT JOIN userstatuses us ON ua.status_u = us.status
      LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
      LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
      LEFT JOIN banktransactions bt ON pd.pid = bt.pid
      WHERE pd.class = ?class
        AND pd.status_p > 0
        AND bt.status = 1
      GROUP BY 
        pd.name, pd.tpe, t.name, mb.username, us.desc, ps.desc, pd.position
      ORDER BY pd.tpe DESC;",
    class = myclass
  ) %>% 
    suppressWarnings()
}


#* Get Activity Check History
#* @get /acHistory
#* @serializer json
#* 
function(){
  portalQuery(
    "SELECT 
        FLOOR(DATEDIFF(
        CONVERT_TZ(FROM_UNIXTIME(time), 'UTC', 'America/Los_Angeles'),
        '2024-07-22' -- Start of week 30 in 2024 (adjust the date as needed)
    ) / 7) + 140 AS nweeks,
        COUNT(*) AS count
    FROM tpehistory
    WHERE source = 'Activity Check'
    GROUP BY nweeks
    ORDER BY nweeks;"
  )
}


#* Get weekly TPE Checklist for one player
#* @get /tpeChecklist
#* @serializer json
#* @param username Forum username
#* 
function(username) {
  ## Get the timestamp for the start of the current week in US/Pacific time
  weekStart <- lubridate::now() %>% 
    with_tz("US/Pacific") %>% 
    floor_date("week", week_start = "Monday") %>% 
    as.numeric()
  
  # Main query: Get thread/post information based on the player's username.
  tasks <- mybbQuery(
    query = "
        WITH current_season AS (
          SELECT MAX(season) AS current_season
          FROM indexdb.seasoninfo
        ), 
        player_class AS (
          SELECT CONCAT('S', MAX(CAST(SUBSTRING(pd.class, 2) AS UNSIGNED))) AS class
          FROM portaldb.playerdata pd
          JOIN mybbdb.mybb_users mbb ON pd.uid = mbb.uid
          WHERE mbb.username = ?username
        )
        SELECT 
          p.username AS user, 
          COUNT(p.pid) - (CASE WHEN p.username = t.username THEN 1 ELSE 0 END) AS count, 
          t.tid, 
          CONCAT('https://forum.simulationsoccer.com/showthread.php?tid=', t.tid) AS link, 
          t.subject, 
          t.username AS op
        FROM mybbdb.mybb_threads t
        JOIN mybbdb.mybb_posts p ON p.tid = t.tid
        JOIN player_class pc ON 1=1
        JOIN current_season cs ON 1=1
        WHERE (
              (pc.class <> CONCAT('S', cs.current_season + 1) AND t.fid IN (22, 49, 25, 24, 122))
           OR (pc.class = CONCAT('S', cs.current_season + 1) AND 
               t.fid IN (22, 49, 25, 24, 122, 179, 180, 181, 182, 183)
               AND NOT (t.subject LIKE CONCAT('%S', cs.current_season, ' Minor%') 
                        OR t.subject LIKE CONCAT('%S', cs.current_season, ' Major%'))
              )
        )
          AND t.sticky = 0 
          AND t.closed = 0
        GROUP BY p.username, t.tid, t.subject, t.username;",
        username = username
  ) %>% 
  group_by(subject, link) %>% 
  summarize(
    posted = dplyr::if_else(any(str_to_lower(user) == str_to_lower(username) & count > 0),
                            TRUE, FALSE) %>% tidyr::replace_na(replace = FALSE)
  ) %>% 
  ungroup() %>% 
  add_row(
    tibble(
      subject = "Activity Check",
      link = "https://index.simulationsoccer.com",
      posted = (portalQuery(
        query = "
            SELECT * 
            FROM tpehistory 
            WHERE pid = (
                SELECT pd.pid
                FROM playerdata AS pd
                JOIN mybbdb.mybb_users AS mbb ON pd.uid = mbb.uid
                WHERE mbb.username = ?username
                  AND pd.status_p = 1
            ) 
            AND source LIKE '%Activity Check' 
            AND time > ?weekStart",
        username = username,
        weekStart = weekStart
      ) %>% nrow()) > 0
    )
  ) %>% 
  suppressWarnings()

return(tasks)
}


#* Get weekly TPE Checklist for one team
#* @get /teamTPEChecklist
#* @serializer json
#* @param username Forum username
#* 
function(username) {
  ## Gets date of the start of the week in Pacific
  weekStart <- lubridate::now() %>% 
    with_tz("US/Pacific") %>% 
    floor_date("week", week_start = "Monday") %>% 
    as.numeric()
  
  # Step 1: Query for players in the same organization using parameterized query
  playersInSameTeam <- mybbQuery(
    query = "
      WITH current_season AS (
          SELECT MAX(season) AS current_season
          FROM indexdb.seasoninfo
      ), 
      player_info AS (
          SELECT 
              pd.team, 
              CONCAT('S', MAX(CAST(SUBSTRING(pd.class, 2) AS UNSIGNED))) AS class
          FROM portaldb.playerdata pd
          JOIN mybbdb.mybb_users mbb ON pd.uid = mbb.uid
          WHERE mbb.username = ?username AND pd.status_p > 0
          GROUP BY pd.team
      ), 
      same_team_players AS (
          SELECT 
              pd.pid, 
              pd.name, 
              pd.team, 
              mbb.username 
          FROM portaldb.playerdata pd
          JOIN mybbdb.mybb_users mbb ON pd.uid = mbb.uid
          WHERE pd.team = (SELECT team FROM player_info)
      )
      SELECT 
          sop.username AS user,
          sop.name AS player_name,
          sop.team AS team_id
      FROM same_team_players sop
      ORDER BY sop.username;",
  username = username
  )

  # Step 2: Iterate through each user and query their tasks using parameterized queries
  allTasks <- purrr::map_df(playersInSameTeam$user, function(currentUsername) {
    tasks <- mybbQuery(
      query = "
        WITH current_season AS (
            SELECT MAX(season) AS current_season
            FROM indexdb.seasoninfo
        ), 
        player_class AS (
            SELECT CONCAT('S', MAX(CAST(SUBSTRING(pd.class, 2) AS UNSIGNED))) AS class
            FROM portaldb.playerdata pd
            JOIN mybbdb.mybb_users mbb ON pd.uid = mbb.uid
            WHERE mbb.username = ?currentUsername
        )
        SELECT 
            p.username AS user, 
            COUNT(p.pid) - (CASE WHEN p.username = t.username THEN 1 ELSE 0 END) AS count, 
            t.tid, 
            CONCAT('https://forum.simulationsoccer.com/showthread.php?tid=', t.tid) AS link, 
            t.subject, 
            t.username AS op
        FROM mybbdb.mybb_threads t
        JOIN mybbdb.mybb_posts p ON p.tid = t.tid
        JOIN player_class pc ON 1=1
        JOIN current_season cs ON 1=1
        WHERE (
              (pc.class <> CONCAT('S', cs.current_season + 1) AND t.fid IN (22, 49, 25, 24, 122))
              OR 
              (pc.class = CONCAT('S', cs.current_season + 1) AND 
               t.fid IN (22, 49, 25, 24, 122, 179, 180, 181, 182, 183) AND
               NOT (t.subject LIKE CONCAT('%S', cs.current_season, ' Minor%') OR 
                    t.subject LIKE CONCAT('%S', cs.current_season, ' Major%'))
              )
        )
        AND t.sticky = 0 
        AND t.closed = 0
        GROUP BY p.username, t.tid, t.subject, t.username;",
        currentUsername = currentUsername
    )
  
  # Summarize the tasks for the current user.
  tasks <- tasks %>% 
    group_by(subject, link) %>% 
    summarize(
      posted = dplyr::if_else(
        any(str_to_lower(user) == str_to_lower(currentUsername) & count > 0), 
        TRUE, 
        FALSE
      ) %>% tidyr::replace_na(replace = FALSE)
    ) %>% 
    ungroup()
  
  # Activity Check using a parameterized query (portalQuery):
  activityCheck <- portalQuery(
    query = "
      SELECT * 
      FROM tpehistory 
      WHERE pid = (
          SELECT pd.pid
          FROM playerdata AS pd
          JOIN mybbdb.mybb_users AS mbb ON pd.uid = mbb.uid
          WHERE mbb.username = ?currentUsername
            AND pd.status_p = 1
      ) 
      AND source LIKE '%Activity Check' 
      AND time > ?weekStart;",
    currentUsername = currentUsername,
    weekStart = weekStart
  )
  
  # Add the Activity Check row.
  tasks <- tasks %>% 
    add_row(
      tibble(
        subject = "Activity Check",
        link = "https://index.simulationsoccer.com",
        posted = (nrow(activityCheck) > 0),
        user = currentUsername
      )
    ) %>% 
    mutate(user = currentUsername) %>% 
    suppressWarnings()
  
  return(tasks)
  })

return(allTasks)
}

#* Gets weekly top earners
#* @get /topEarners
#* @serializer json
#* 
function() {
  portalQuery(
    paste(
      "SELECT 
            pd.name AS Name,
            mbb.username AS Username,
            SUM(ph.tpe) AS `TPE Earned`
        FROM 
            tpehistory ph
        JOIN 
            playerdata pd ON ph.pid = pd.pid
        LEFT JOIN
            mybbdb.mybb_users mbb ON pd.uid = mbb.uid
        WHERE 
            YEARWEEK(FROM_UNIXTIME(ph.time), 1) = YEARWEEK(CONVERT_TZ(CURDATE(), 'UTC', 'America/Los_Angeles'), 1) AND ph.source <> 'Initial TPE' AND ph.tpe > 0
        GROUP BY 
            ph.pid
        ORDER BY 
            `TPE Earned` DESC
        LIMIT 10;"
    )
  )
}