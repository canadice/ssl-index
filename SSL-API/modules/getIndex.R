#* @apiTitle Index API
#* @apiDescription Endpoints to get index information.


#* Gets next gameID that is not present in the indexdata
#* @get /nextGame
#* @param season The season to search
#* @serializer json

function(season) {
  indexQuery(
    query = 
      "SELECT team, MIN(gid) AS gid
       FROM (
           SELECT home AS team, MIN(s.gid) AS gid
           FROM schedule s
           LEFT JOIN gamedataoutfield o ON s.gid = o.gid
           WHERE o.gid IS NULL AND s.season = ?season AND s.Matchtype >= 0
           GROUP BY home

           UNION ALL

           SELECT away AS team, MIN(s.gid) AS gid
           FROM schedule s
           LEFT JOIN gamedataoutfield o ON s.gid = o.gid
           WHERE o.gid IS NULL AND s.season = ?season AND s.Matchtype >= 0
           GROUP BY away
       ) AS combined
       GROUP BY team
       ORDER BY gid;",
    season = season
  )
}


#* Gets outfield index data for season and league
#* @get /outfield
#* @param season The selected season
#* @param league The selected league

function(league, season) {
  indexQuery(
    query = 
      "SELECT
         `name`,
         `club`,
         `position`,
         `apps`,
         `minutes played`,
         `player of the match`,
         `distance run (km)`,
         `goals`,
         `assists`,
         `xg`,
         `shots on target`,
         `shots`,
         `penalties taken`,
         `penalties scored`,
         `successful passes`,
         `attempted passes`,
         SUM(`successful passes`) / SUM(`attempted passes`) * 100 AS `pass%`,
         `key passes`,
         `successful crosses`,
         `attempted crosses`,
         SUM(`successful crosses`) / SUM(`attempted crosses`) * 100 AS `cross%`,
         `chances created`,
         `successful headers`,
         `attempted headers`,
         SUM(`successful headers`) / SUM(`attempted headers`) * 100 AS `header%`,
         `key headers`,
         `dribbles`,
         `tackles won`,
         `attempted tackles`,
         SUM(`tackles won`) / SUM(`attempted tackles`) * 100 AS `tackle%`,
         `key tackles`,
         `interceptions`,
         `clearances`,
         `mistakes leading to goals`,
         `yellow cards`,
         `red cards`,
         `fouls`,
         `fouls against`,
         `offsides`,
         `xa`,
         `xg overperformance`,
         `fk shots`,
         `blocks`,
         `open play key passes`,
         `successful open play crosses`,
         `attempted open play crosses`,
         SUM(`successful open play crosses`) / SUM(`attempted open play crosses`) * 100 AS `open play crosses%`,
         `shots blocked`,
         `progressive passes`,
         `successful presses`,
         `attempted presses`,
         CASE WHEN IFNULL(SUM(`attempted presses`),0) = 0 THEN 0
              ELSE SUM(`successful presses`) / SUM(`attempted presses`) * 100
         END AS `press%`,
         `goals outside box`,
         AVG(`average rating`) AS `average rating`,
         `shots on target` / `shots` * 100 AS `shot accuracy%`,
         `xG` - 0.83 * `penalties taken` AS `pen adj xG`
       FROM (
         SELECT
           `name`,
           GROUP_CONCAT(DISTINCT club SEPARATOR ', ') AS `club`,
           MAX(`position`) AS `position`,
           SUM(`apps`) AS `apps`,
           SUM(`minutes played`) AS `minutes played`,
           SUM(`distance run (km)`) AS `distance run (km)`,
           SUM(`goals`) AS `goals`,
           SUM(`assists`) AS `assists`,
           SUM(`xg`) AS `xg`,
           SUM(`shots on target`) AS `shots on target`,
           SUM(`shots`) AS `shots`,
           SUM(`penalties taken`) AS `penalties taken`,
           SUM(`penalties scored`) AS `penalties scored`,
           SUM(`successful passes`) AS `successful passes`,
           SUM(`attempted passes`) AS `attempted passes`,
           SUM(`key passes`) AS `key passes`,
           SUM(`successful crosses`) AS `successful crosses`,
           SUM(`attempted crosses`) AS `attempted crosses`,
           SUM(`chances created`) AS `chances created`,
           SUM(`successful headers`) AS `successful headers`,
           SUM(`attempted headers`) AS `attempted headers`,
           SUM(`key headers`) AS `key headers`,
           SUM(`dribbles`) AS `dribbles`,
           SUM(`tackles won`) AS `tackles won`,
           SUM(`attempted tackles`) AS `attempted tackles`,
           SUM(`key tackles`) AS `key tackles`,
           SUM(`interceptions`) AS `interceptions`,
           SUM(`clearances`) AS `clearances`,
           SUM(`mistakes leading to goals`) AS `mistakes leading to goals`,
           SUM(`yellow cards`) AS `yellow cards`,
           SUM(`red cards`) AS `red cards`,
           SUM(`fouls`) AS `fouls`,
           SUM(`fouls against`) AS `fouls against`,
           SUM(`offsides`) AS `offsides`,
           SUM(`xa`) AS `xa`,
           SUM(`xg overperformance`) AS `xg overperformance`,
           SUM(`fk shots`) AS `fk shots`,
           SUM(`blocks`) AS `blocks`,
           SUM(`open play key passes`) AS `open play key passes`,
           SUM(`successful open play crosses`) AS `successful open play crosses`,
           SUM(`attempted open play crosses`) AS `attempted open play crosses`,
           SUM(`shots blocked`) AS `shots blocked`,
           SUM(`progressive passes`) AS `progressive passes`,
           SUM(`successful presses`) AS `successful presses`,
           SUM(`attempted presses`) AS `attempted presses`,
           SUM(`goals outside box`) AS `goals outside box`,
           SUM(`player of the match`) AS `player of the match`,
           AVG(`average rating`) AS `average rating`
         FROM (
           SELECT
             `name`,
             `club`,
             `position`,
             `apps`,
             `minutes played`,
             `distance run (km)`,
             `average rating`,
             `player of the match`,
             `goals`,
             `assists`,
             `xg`,
             `shots on target`,
             `shots`,
             `penalties taken`,
             `penalties scored`,
             `successful passes`,
             `attempted passes`,
             `pass%`,
             `key passes`,
             `successful crosses`,
             `attempted crosses`,
             `cross%`,
             `chances created`,
             `successful headers`,
             `attempted headers`,
             `header%`,
             `key headers`,
             `dribbles`,
             `tackles won`,
             `attempted tackles`,
             `tackle%`,
             `key tackles`,
             `interceptions`,
             `clearances`,
             `mistakes leading to goals`,
             `yellow cards`,
             `red cards`,
             `fouls`,
             `fouls against`,
             `offsides`,
             `xa`,
             `xg overperformance`,
             `fk shots`,
             `blocks`,
             `open play key passes`,
             `successful open play crosses`,
             `attempted open play crosses`,
             `shots blocked`,
             `progressive passes`,
             `successful presses`,
             `attempted presses`,
             `goals outside box`
           FROM `gamedataoutfield` AS gd
           JOIN schedule AS s ON gd.gid = s.gid
           WHERE ( ?league = 'ALL' OR s.Matchtype = ?league )
             AND ( ?season = 'ALL' OR s.season = ?season )
         ) AS q01
         GROUP BY `name`
       ) AS q02
       GROUP BY `name`;",
    league = league,
    season = season
  )
}


#* Gets outfield game by game data
#* @get /outfieldGameByGame
#* @param name:str The selected player
#* @param season:int The selected season
#*
function(name, season = NA) {
  indexQuery(
    query = 
      "SELECT 
         CONCAT('S', s.season, ' MD', s.matchday) AS matchday,
         ti.abbreviation AS opponent,
         CONCAT(
           CASE WHEN g.club = s.home THEN s.HomeScore ELSE s.AwayScore END,
           '-',
           CASE WHEN g.club = s.home THEN s.AwayScore ELSE s.HomeScore END
         ) AS result,
         g.`minutes played`,
         g.`average rating`,
         g.goals,
         g.assists,
         g.`pass%`,
         g.`header%`,
         g.`tackle%`,
         g.*
      FROM `gamedataoutfield` AS g 
      JOIN schedule AS s ON g.gid = s.gid
      JOIN portaldb.teams AS ti 
         ON ti.name = (CASE WHEN g.club = s.home THEN s.away ELSE s.home END)
      WHERE ( ?season = 'ALL' OR s.season = ?season )
        AND ( ?name   = 'ALL' OR g.name = ?name )
      ORDER BY g.gid DESC;",
    season = if_else(is.na(season), "ALL", season),
    name   = name
  )
}


#* Gets outfield game by game data
#* @get /latestGames
#* @param name:str The selected player
#*
function(name, outfield = TRUE) {
  
  if (outfield) {
    indexQuery(
      query = 
        "SELECT 
           CONCAT('S', s.season, ' MD', s.matchday) AS matchday,
           ti.abbreviation AS opponent,
           CONCAT(
             CASE WHEN g.club = s.home THEN s.HomeScore ELSE s.AwayScore END,
             '-',
             CASE WHEN g.club = s.home THEN s.AwayScore ELSE s.HomeScore END
           ) AS result,
           g.`minutes played`,
           g.`average rating`,
           g.goals,
           g.assists,
           g.`pass%`,
           g.`header%`,
           g.`tackle%`
         FROM `gamedataoutfield` AS g 
         JOIN schedule AS s ON g.gid = s.gid
         JOIN portaldb.teams AS ti ON ti.name = (CASE WHEN g.club = s.home THEN s.away ELSE s.home END)
         WHERE g.name = ?name
         ORDER BY g.gid DESC
         LIMIT 10;",
      name = name
    )
  } else {
    indexQuery(
      query =
        "SELECT
           CONCAT('S', s.season, ' MD', s.matchday) AS matchday,
           ti.abbreviation AS opponent,
           CONCAT(
             CASE WHEN g.club = s.home THEN s.HomeScore ELSE s.AwayScore END,
             '-',
             CASE WHEN g.club = s.home THEN s.AwayScore ELSE s.HomeScore END
           ) AS result,
           g.`minutes played`,
           g.`average rating`,
           (g.`saves parried` + g.`saves parried` + g.`saves tipped`) AS `total saves`,
           g.`save%`,
           g.`xg prevented`
         FROM `gamedatakeeper` AS g
         JOIN schedule AS s ON g.gid = s.gid
         JOIN portaldb.teams AS ti ON ti.name = (CASE WHEN g.club = s.home THEN s.away ELSE s.home END)
         WHERE g.name = ?name
         ORDER BY g.gid DESC
         LIMIT 10;",
      name = name
    )
  }
}

#* Gets keeper index data for season and league
#* @get /keeper
#* @param season The selected season
#* @param league The selected league
#* 
function(league, season) {
  indexQuery(
    query = 
      "SELECT
         `name`, `club`, `apps`, `minutes played`, `average rating`, `player of the match`, 
         SUM(won) AS won, SUM(lost) AS lost, SUM(drawn) AS drawn, `clean sheets`, conceded, 
         `saves parried`, `saves held`, `saves tipped`, 
         (1 - (conceded / (conceded + `saves parried` + `saves held` + `saves tipped`))) * 100 AS `save%`,
         `penalties faced`, `penalties saved`, `xsave%`, `xg prevented`
       FROM (
         SELECT
           `name`,
           GROUP_CONCAT(DISTINCT club SEPARATOR ', ') AS `club`,
           SUM(`apps`) AS `apps`,
           SUM(`minutes played`) AS `minutes played`,
           AVG(`average rating`) AS `average rating`,
           SUM(`player of the match`) AS `player of the match`,
           SUM(CASE WHEN (Home = club AND HomeScore > AwayScore) 
                    OR (Away = club AND AwayScore > HomeScore)
                    THEN 1 ELSE 0 END) AS `won`,
           SUM(CASE WHEN (Home = club AND HomeScore < AwayScore) 
                    OR (Away = club AND AwayScore < HomeScore)
                    THEN 1 ELSE 0 END) AS `lost`,
           SUM(CASE WHEN HomeScore = AwayScore THEN 1 ELSE 0 END) AS `drawn`,
           SUM(`clean sheets`) AS `clean sheets`,
           SUM(`conceded`) AS `conceded`,
           SUM(`saves parried`) AS `saves parried`,
           SUM(`saves held`) AS `saves held`,
           SUM(`saves tipped`) AS `saves tipped`,
           SUM(`save%`) AS `save%`,
           SUM(`penalties faced`) AS `penalties faced`,
           SUM(`penalties saved`) AS `penalties saved`,
           AVG(`xsave%`) AS `xsave%`,
           SUM(`xg prevented`) AS `xg prevented`
         FROM (
           SELECT
             `name`,
             `club`,
             `apps`,
             `minutes played`,
             `average rating`,
             `player of the match`,
             `clean sheets`,
             `conceded`,
             `saves parried`,
             `saves held`,
             `saves tipped`,
             `save%`,
             `penalties faced`,
             `penalties saved`,
             `xsave%`,
             `xg prevented`,
             s.*
           FROM `gamedatakeeper` AS gd
           JOIN schedule AS s ON gd.gid = s.gid
           WHERE ( ?league = 'ALL' OR s.Matchtype = ?league )
             AND ( ?season = 'ALL' OR s.season = ?season )
         ) AS q01
         GROUP BY `name`
       ) AS q02
       GROUP BY `name`;",
    league = league,
    season = season
  )
}


#* Gets keeper game by game data
#* @get /keeperGameByGame
#* @param name:str The selected player
#* @param season:int The selected season
function(name, season = NA){
  indexQuery(
    query = 
      "SELECT
         CONCAT('S', s.season, ' MD', s.matchday) AS matchday,
         ti.abbreviation AS opponent,
         CONCAT(
           CASE WHEN g.club = s.home THEN s.HomeScore ELSE s.AwayScore END,
           '-',
           CASE WHEN g.club = s.home THEN s.AwayScore ELSE s.HomeScore END
         ) AS result,
         g.`minutes played`,
         g.`average rating`,
         (g.`saves parried` + g.`saves parried` + g.`saves tipped`) AS `total saves`,
         g.`save%`,
         g.`xg prevented`,
         g.*
      FROM `gamedatakeeper` AS g
      JOIN schedule AS s ON g.gid = s.gid
      JOIN portaldb.teams AS ti 
         ON ti.name = (CASE WHEN g.club = s.home THEN s.away ELSE s.home END)
      WHERE ( ?season = 'ALL' OR s.season = ?season )
         AND ( ?name   = 'ALL' OR g.name     = ?name )
      ORDER BY g.gid DESC;",
    season = if_else(is.na(season), "ALL", season),
    name = name
  )
}


#* Gets aggregated standings
#* @get /standings
#* @param season The selected season
#* @param league The selected league
#* 
function(league = "ALL", season = "ALL"){
  indexQuery(
    query =
      "SELECT
         Team,
         COUNT(*) AS MatchesPlayed,
         SUM(CASE
             WHEN (Home = Team AND HomeScore > AwayScore) 
               OR (Away = Team AND AwayScore > HomeScore) THEN 1
             ELSE 0
         END) AS Wins,
         SUM(CASE
             WHEN (HomeScore = AwayScore) THEN 1
             ELSE 0
         END) AS Draws,
         SUM(CASE
             WHEN (Home = Team AND HomeScore < AwayScore) 
               OR (Away = Team AND AwayScore < HomeScore) THEN 1
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
             WHEN (Home = Team AND HomeScore > AwayScore) 
               OR (Away = Team AND AwayScore > HomeScore) THEN 3
             WHEN (HomeScore = AwayScore) THEN 1
             ELSE 0
         END) AS Points,
         SUM(CASE
             WHEN (Home = Team) THEN HomeScore
             ELSE AwayScore
         END)
         - SUM(CASE
             WHEN (Home = Team) THEN AwayScore
             ELSE HomeScore
         END) AS GoalDifference
       FROM (
         SELECT 
           Home AS Team, Home, Away, HomeScore, AwayScore
         FROM schedule
         WHERE HomeScore IS NOT NULL
           AND ( (?league = 'ALL' AND matchtype >= 0)
                 OR (matchtype = ?league) )
           AND ( (?season = 'ALL') OR (Season = ?season) )
         UNION ALL
         SELECT 
           Away AS Team, Home, Away, HomeScore, AwayScore
         FROM schedule
         WHERE HomeScore IS NOT NULL
           AND ( (?league = 'ALL' AND matchtype >= 0)
                 OR (matchtype = ?league) )
           AND ( (?season = 'ALL') OR (Season = ?season) )
       ) AS combined
       GROUP BY Team
       ORDER BY Points DESC, GoalDifference DESC, GoalsFor DESC;",
    league = league,
    season = season
  ) %>% 
    suppressWarnings()
}

#* Gets outfield index data for season and league
#* @get /academyOutfield
#* @param season The selected season

function(season) {
  indexQuery(
    query = 
      "SELECT
         `name`,
         `club`,
         `position`,
         `apps`,
         `minutes played`,
         `player of the match`,
         `distance run (km)`,
         `goals`,
         `assists`,
         `xg`,
         `shots on target`,
         `shots`,
         `penalties taken`,
         `penalties scored`,
         `successfull passes` AS `successful passes`,
         `attempted passes`,
         `successfull passes` / `attempted passes` * 100 AS `pass%`,
         `key passes`,
         `successful crosses`,
         `attempted crosses`,
         `successful crosses` / `attempted crosses` * 100 AS `cross%`,
         `chances created`,
         `successful headers`,
         `attempted headers`,
         `successful headers` / `attempted headers` * 100 AS `header%`,
         `key headers`,
         `dribbles`,
         `tackles won`,
         `attempted tackles`,
         `tackles won` / `attempted tackles` * 100 AS `tackle%`,
         `key tackles`,
         `interceptions`,
         `clearances`,
         `mistakes leading to goals`,
         `yellow cards`,
         `red cards`,
         `fouls`,
         `fouls against`,
         `offsides`,
         `xa`,
         `xg overperformance`,
         `fk shots`,
         `blocks`,
         `open play key passes`,
         `successful open play crosses`,
         `attempted open play crosses`,
         `shots blocked`,
         `progressive passes`,
         `successful presses`,
         `attempted presses`,
         `goals outside box`,
         `average rating`,
         CASE 
           WHEN IFNULL(`attempted presses`, 0) = 0 THEN 0
           ELSE (`successful presses` / `attempted presses`) * 100
         END AS `press%`,
         CASE 
           WHEN IFNULL(`attempted open play crosses`, 0) = 0 THEN 0
           ELSE (`successful open play crosses` / `attempted open play crosses`) * 100
         END AS `open play crosses%`,
         `shots on target` / `shots` * 100 AS `shot accuracy%`,
         `xG` - 0.83 * `penalties taken` AS `pen adj xG`
       FROM academyoutfield
       WHERE season = ?season;",
    season = season
  ) %>% 
    suppressWarnings()
} 


#* Gets keeper index data for season and league
#* @get /academyKeeper
#* @param season The selected season
function(season) {
  indexQuery(
    query = 
      "SELECT
         `name`, 
         `club`, 
         `apps`, 
         `minutes played`, 
         `average rating`, 
         `player of the match`, 
         won, 
         lost, 
         draw, 
         `clean sheets`, 
         conceded, 
         `saves parried`, 
         `saves held`, 
         `saves tipped`, 
         (1 - (conceded / (conceded + `saves parried` + `saves held` + `saves tipped`))) * 100 AS `save%`,
         `penalties faced`, 
         `penalties saved`, 
         `xsave%`, 
         `xg prevented`
       FROM academykeeper 
       WHERE season = ?season;",
    season = season
  ) %>% 
    suppressWarnings()
}


#* Gets the schedule for the league
#* @get /schedule
#* @param season The selected season
#* @param league The selected league
#* 
function(season, league = "ALL") {
  indexQuery(
    query = 
      "SELECT IRLDate, 
              MatchType, 
              MatchDay, 
              Home, 
              Away, 
              HomeScore, 
              AwayScore, 
              ExtraTime, 
              Penalties
       FROM schedule
       WHERE ( ( ?season = 'ALL' AND season > 0 )
              OR ( ?season <> 'ALL' AND season = ?season ) )
         AND ( ( ?league = 'ALL' AND MatchType < 10 )
              OR ( ?league <> 'ALL' AND MatchType = ?league ) )
       ORDER BY IRLDate;",
    season = season,
    league = league
  )
}

#* Gets the boxscore for a specific game
#* @get /boxscore
#* @param season The selected season
#* @param league The selected league (0 = Cup, 1 = Majors, 2 = Minors)
#* @param matchday The selected matchday (League matchdays as numbers, cup uses shorthand A, B, C, D for group stage, FR, R16, QF, SF, F for knockout followed by game/leg number)
#* @param team The selected team using the full name found <a href="https://docs.google.com/spreadsheets/d/1dCOGjnLrtgYjO43Zz1dYkuQ5ZQRpION3LQiRrooMZaQ/edit?gid=731383221#gid=731383221">here</a> in column C.
function(season, league, matchday, team) {
  gid <- 
    indexQuery(
      "SELECT gid
      FROM schedule
      WHERE (season = ?season) 
        AND (MatchType = ?league) 
        AND (Matchday = ?matchday)
        AND ( (Home = ?team) OR (Away = ?team))",
      season = season,
      league = league,
      matchday = matchday,
      team = team
    )
  
  if (nrow(gid) != 0){
    indexQuery(
      query = 
        "SELECT 
          s.IRLDate, 
          s.MatchType, 
          s.MatchDay, 
          s.Home, 
          s.Away, 
          s.HomeScore, 
          s.AwayScore, 
          s.ExtraTime, 
          s.Penalties,
          /* Home scorers only where goals > 0 */
          (
            SELECT 
              GROUP_CONCAT(
                CONCAT(o.name, ' (', o.goals, ')')
                ORDER BY o.goals DESC, o.name
                SEPARATOR ', '
              )
            FROM gamedataoutfield o
            WHERE o.gid    = s.gid
              AND o.club   = s.Home
              AND o.goals > 0
          ) AS homeGoals,
          /* Home assisters where assists > 0 */
          (
            SELECT 
              GROUP_CONCAT(
                CONCAT(o.name, ' (', o.assists, ')')
                ORDER BY o.assists DESC, o.name
                SEPARATOR ', '
              )
            FROM gamedataoutfield AS o
            WHERE o.gid      = s.gid
              AND o.club     = s.Home
              AND o.assists > 0
          ) AS homeAssists,
          /* Away scorers only where goals > 0 */
          (
            SELECT 
              GROUP_CONCAT(
                CONCAT(o.name, ' (', o.goals, ')')
                ORDER BY o.goals DESC, o.name
                SEPARATOR ', '
              )
            FROM gamedataoutfield o
            WHERE o.gid    = s.gid
              AND o.club   = s.Away
              AND o.goals > 0
          ) AS awayGoals,
          /* Away assisters where assists > 0 */
          (
            SELECT 
              GROUP_CONCAT(
                CONCAT(o.name, ' (', o.assists, ')')
                ORDER BY o.assists DESC, o.name
                SEPARATOR ', '
              )
            FROM gamedataoutfield AS o
            WHERE o.gid      = s.gid
              AND o.club     = s.Away
              AND o.assists > 0
          ) AS awayAssists,
          (
            SELECT
              CONCAT(o.name, ' (', o.club, ')')
            FROM gamedataoutfield AS o
            WHERE o.gid     = s.gid
              AND o.`player of the match` > 0
          ) AS `Player of the Match`
        FROM schedule s
               
       WHERE gid = ?gid;",
      gid = gid$gid
    )
  } else {
    "No game found with these inputs."
  }
  
}



