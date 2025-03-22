box::use(
  dplyr[across, mutate, if_else, where],
  lubridate,
  promises[future_promise],
  tidyr[replace_na],
)

box::use(
  app/logic/db/api[readAPI],
  app/logic/db/database[portalQuery, indexQuery],
)

#' @export
getUpdateHistory <- function(pid) {
  portalQuery(
    paste("SELECT 
            uh.time AS Time,
            mbb.username AS Username,
            uh.attribute AS `Changed attribute`,
            uh.old AS `From`,
            uh.new AS `To`
        FROM 
            updatehistory uh
        LEFT JOIN
            mybbdb.mybb_users mbb ON uh.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY Time DESC")
  ) |>
    mutate(
      Time = Time |>
        as.numeric() |>
        lubridate$as_datetime(tz = "US/Pacific")
    ) |>
    future_promise()
}

#' @export
getTpeHistory <- function(pid) {
  portalQuery(
    paste("SELECT 
            tpeh.time AS Time,
            mbb.username AS Username,
            tpeh.source AS Source,
            tpeh.tpe AS `TPE Change`
        FROM 
            tpehistory tpeh
        LEFT JOIN
            mybbdb.mybb_users mbb ON tpeh.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY time DESC")
  ) |>
    mutate(
      Time = Time |>
        as.numeric() |>
        lubridate$as_datetime(tz = "US/Pacific")
    ) |>
    future_promise()
}

#' @export
getBankHistory <- function(pid) {
  readAPI(
    "https://api.simulationsoccer.com/bank/getBankTransactions",
    query = list(pid = pid)
  ) |>
    future_promise()
}

#' @export
getRecentCreates <- function() {
  portalQuery(
    "SELECT pd.name, mb.username, pd.position
    FROM playerdata pd
    LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
    ORDER BY pd.created DESC
    LIMIT 10;"
  )
}

#' @export
getTopEarners <- function() {
  portalQuery(
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
        YEARWEEK(FROM_UNIXTIME(ph.time), 1) = YEARWEEK(CONVERT_TZ(CURTIME(), 'UTC', 'America/Los_Angeles'), 1) AND ph.source <> 'Initial TPE' AND ph.tpe > 0
    GROUP BY 
        ph.pid
    ORDER BY 
        `TPE Earned` DESC
    LIMIT 10;"
  )
}

#' @export
getPlayerNames <- function(retired = TRUE, freeAgent = TRUE){
  portalQuery(
    paste0(
      "SELECT name, pid
      FROM playerdata ",
      if_else(!freeAgent, 
              "WHERE team > -2", 
              if_else(!retired,
                      "WHERE status_p > 0",
                      "WHERE status_p >= 0"
                      )
              ),
      " ORDER BY name;"
    )
  )
}

#' @export
getStandings <- function(league, season){
  indexQuery(
    paste(
      "SELECT
    Team,
    COUNT(*) AS MatchesPlayed,
    SUM(CASE
        WHEN (Home = Team AND HomeScore > AwayScore) OR (Away = Team AND AwayScore > HomeScore) THEN 1
        ELSE 0
    END) AS Wins,
    SUM(CASE
        WHEN (HomeScore = AwayScore) THEN 1
        ELSE 0
    END) AS Draws,
    SUM(CASE
        WHEN (Home = Team AND HomeScore < AwayScore) OR (Away = Team AND AwayScore < HomeScore) THEN 1
        ELSE 0
    END) AS Losses,
    SUM(CASE
        WHEN (Home = Team) THEN HomeScore
        ELSE AwayScore
    END) AS GoalsFor,
    SUM(CASE
        WHEN (Home = Team) THEN AwayScore
        ELSE HomeScore
    END) AS GoalsAgainst,
    SUM(CASE
        WHEN (Home = Team AND HomeScore > AwayScore) OR (Away = Team AND AwayScore > HomeScore) THEN 3
        WHEN (HomeScore = AwayScore) THEN 1
        ELSE 0
    END) AS Points
FROM (
    SELECT Home AS Team, Home, Away, HomeScore, AwayScore FROM schedule WHERE HomeScore IS NOT NULL AND matchtype ", 
      if_else(league == "ALL", '>= 0', paste0("=", league)), 
      if_else(season == "ALL", "", paste0(" AND Season = ", season)),  
      "UNION ALL
    SELECT Away AS Team, Home, Away, HomeScore, AwayScore FROM schedule WHERE HomeScore IS NOT NULL AND matchtype ", 
      if_else(league == "ALL", '>= 0', paste0("=", league)), 
      if_else(season == "ALL", "", paste0(" AND Season = ", season)),
      ") AS combined
GROUP BY Team
ORDER BY Points DESC, GoalsFor DESC, GoalsAgainst ASC;"
    )
  ) |> 
    suppressWarnings()
}

#' @export
getSchedule <- function(league, season){
  indexQuery(
    paste(
      "SELECT IRLDate, MatchType, MatchDay, Home, Away, HomeScore, AwayScore, ExtraTime, Penalties
      FROM schedule
      WHERE season ",
      if_else(season == "ALL", "> 0", paste0("=", season)), 
      "AND MatchType",
      if_else(league == "ALL", '< 10', paste0("=", league)),
      "ORDER BY IRLDate;"
    )
  )
}

#' @export
getPlayer <- function(pid){
  portalQuery(
    paste(
      "SELECT pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
        pd.created, pd.tpe, pd.tpeused, pd.tpebank, pd.team AS organization, t.name AS team, pd.affiliate, pd.birthplace, 
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
          LEFT JOIN portaldb.nationality n ON pd.nationality = n.abbreviation",
      paste("WHERE pd.pid = ", pid, ";")
    )
  ) |>
    mutate(
      across(where(is.numeric), ~replace_na(.x, 5))
    )
}