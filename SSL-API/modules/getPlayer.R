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
      "SELECT *
      FROM
        allplayersview
      WHERE
        status_p >= ?active;",
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
      "SELECT *
      FROM allplayersweeklyview;"
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
      uid,
      pid,
      status_p,
      first,
      last,
      name,
      class,
      created,
      tpe,
      tpeused,
      tpebank,
      organization,      
      team,              
      affiliate,
      birthplace,
      nationality,       
      height,
      weight,
      hair_color,
      hair_length,
      skintone,
      render,
      `left foot`,
      `right foot`,
      position,
      pos_st,
      pos_lam,
      pos_cam,
      pos_ram,
      pos_lm,
      pos_cm,
      pos_rm,
      pos_lwb,
      pos_cdm,
      pos_rwb,
      pos_ld,
      pos_cd,
      pos_rd,
      pos_gk,
      acceleration,
      agility,
      balance,
      `jumping reach`,
      `natural fitness`,
      pace,
      stamina,
      strength,
      corners,
      crossing,
      dribbling,
      finishing,
      `first touch`,
      `free kick`,
      heading,
      `long shots`,
      `long throws`,
      marking,
      passing,
      `penalty taking`,
      tackling,
      technique,
      aggression,
      anticipation,
      bravery,
      composure,
      concentration,
      decisions,
      determination,
      flair,
      leadership,
      `off the ball`,
      positioning,
      teamwork,
      vision,
      `work rate`,
      `aerial reach`,
      `command of area`,
      communication,
      eccentricity,
      handling,
      kicking,
      `one on ones`,
      reflexes,
      `tendency to rush`,
      `tendency to punch`,
      throwing,
      traits,
      rerollused,
      redistused,
      username,         
      userStatus,       
      playerStatus,     
      `minimum salary`, 
      timesregressed
    FROM allplayersview
"
  
  # Determine the WHERE clause and parameters based on which identifiers are provided.
  if (!is.null(username)) {
    # If username is provided, search based on mb.username.
    whereClause <- " WHERE username = ?username ORDER BY pid DESC LIMIT 1;"
    params <- list(username = username)
    
  } else if (!is.null(uid)) {
    # If uid is provided, search based on pd.uid.
    whereClause <- " WHERE uid = ?uid ORDER BY pid DESC LIMIT 1;"
    params <- list(uid = uid)
    
  } else {
    # Otherwise use either name or pid.
    if (is.null(name)) {
      # Use pid if name is not provided.
      whereClause <- " WHERE pid = ?pid;"
      params <- list(pid = pid)
    } else {
      # Use name.
      whereClause <- " WHERE name = ?name;"
      params <- list(name = name)
    }
  }
  
  # Combine the base query and the WHERE clause.
  fullQuery <- paste0(baseQuery, whereClause)
  
  # Execute the parameterized query.
  data <- do.call(portalQuery, c(list(query = fullQuery), params)) %>% 
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
        name, tpe, team, username, userStatus, playerStatus,
        position, bankBalance
      FROM
        allplayersview
      WHERE
        class = ?class
        AND status_p > 0
      ORDER BY tpe DESC;",
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
  tasks <- portalQuery(
    query = 
    "WITH current_season AS (
        SELECT MAX(season) AS current_season 
        FROM indexdb.seasoninfo
      ),
      player_class AS (
        SELECT CONCAT('S', MAX(CAST(SUBSTRING(ap.class, 2) AS UNSIGNED))) AS class
        FROM allplayersview ap
        WHERE ap.username = ?username
      )
      SELECT 
        p.username AS user,
        COUNT(p.pid) - (CASE WHEN p.username = t.username THEN 1 ELSE 0 END) AS count,
        t.tid,
        CONCAT('https://forum.simulationsoccer.com/showthread.php?tid=', t.tid) AS link,
        t.subject,
        t.username AS op
      FROM threadsview t
      JOIN postsview p ON p.tid = t.tid
      JOIN player_class pc ON 1 = 1
      JOIN current_season cs ON 1 = 1
      WHERE (
              (pc.class <> CONCAT('S', cs.current_season + 1) 
               AND t.fid IN (22, 49, 25, 24, 122))
           OR (pc.class = CONCAT('S', cs.current_season + 1)
               AND t.fid IN (22, 49, 25, 24, 122, 179, 180, 181, 182, 183)
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
        query = 
        "SELECT * 
        FROM acview
        WHERE username = ?username
          AND time > ?weekStart;",
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
  playersInSameTeam <- portalQuery(
    query = 
    "SELECT 
      username AS user, name AS player_name
    FROM allplayersview
    WHERE
      organization = (
        SELECT organization 
        FROM allplayersview 
        WHERE username = ?username
        ORDER BY created DESC
        LIMIT 1
      );",
    username = username
  )

  # Step 2: Iterate through each user and query their tasks using parameterized queries
  allTasks <- purrr::map_df(playersInSameTeam$user, function(currentUsername) {
    tasks <- portalQuery(
      query = 
        "SELECT *
        FROM checklistview
        WHERE username = ?username;",
      username = currentUsername
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
    query = 
      "SELECT * 
        FROM acview
        WHERE username = ?username
          AND time > ?weekStart;",
    username = currentUsername,
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
      "SELECT *
      FROM weeklytpeview
      ORDER BY `TPE Earned` DESC
      LIMIT 10;"
    )
  )
}