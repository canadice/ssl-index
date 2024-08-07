getOutfieldIndex <- function(league, season){
  indexQuery(
    paste(
      "SELECT
      `name`, `club`, `position`, `apps`, `minutes played`,`player of the match`,
      `distance run (km)`, `goals`,`assists`,`xg`,`shots on target`,
      `shots`,`penalties taken`,`penalties scored`,`successful passes`,
      `attempted passes`,
      `successful passes` / `attempted passes` * 100 AS `pass%`,
      `key passes`,  `successful crosses`,`attempted crosses`,
      `successful crosses` / `attempted crosses` * 100 AS `cross%`,
      `chances created`,`successful headers`,`attempted headers`,
      `successful headers` / `attempted headers` * 100 AS `header%`,
      `key headers`,`dribbles`,`tackles won`,`attempted tackles`,
      `tackles won` / `attempted tackles` * 100 AS `tackle%`,
      `key tackles`,`interceptions`,`clearances`,`mistakes leading to goals`,
      `yellow cards`,`red cards`,`fouls`,`fouls against`,`offsides`,
      `xa`,`xg overperformance`,`fk shots`,`blocks`,
      `open play key passes`,`successful open play crosses`,`attempted open play crosses`,
      `shots blocked`,`progressive passes`,`successful presses`,`attempted presses`,
      `goals outside box`,`average rating`,
      `successful presses` / `attempted presses` * 100 AS `press%`,
      `successful open play crosses` / `attempted open play crosses` * 100 AS `open play crosses%`,
      `shots on target` / `shots` * 100 AS `shot accuracy%`,
      `xG` - 0.83*`penalties taken` AS `pen adj xG`
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
      SUM(`pass%`) AS `pass%`,
      SUM(`key passes`) AS `key passes`,
      SUM(`successful crosses`) AS `successful crosses`,
      SUM(`attempted crosses`) AS `attempted crosses`,
      SUM(`cross%`) AS `cross%`,
      SUM(`chances created`) AS `chances created`,
      SUM(`successful headers`) AS `successful headers`,
      SUM(`attempted headers`) AS `attempted headers`,
      SUM(`header%`) AS `header%`,
      SUM(`key headers`) AS `key headers`,
      SUM(`dribbles`) AS `dribbles`,
      SUM(`tackles won`) AS `tackles won`,
      SUM(`attempted tackles`) AS `attempted tackles`,
      SUM(`tackle%`) AS `tackle%`,
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
      FROM `gamedataoutfield`",
      if_else(league == "ALL",
              paste("WHERE season = ", season, sep = ""),
              paste("WHERE division = '", league, "' AND season = ", season, sep = "")
      ),
      ") `q01`
      GROUP BY `name`
    ) `q01`",
      sep = "")
  ) %>% future_promise()
}


getOutfieldCareer <- function(league){
  indexQuery(
    paste(
      "SELECT
      `name`, `club`, `position`, `apps`, `minutes played`,`player of the match`,
      `distance run (km)`, `goals`,`assists`,`xg`,`shots on target`,
      `shots`,`penalties taken`,`penalties scored`,`successful passes`,
      `attempted passes`,
      `successful passes` / `attempted passes` * 100 AS `pass%`,
      `key passes`,  `successful crosses`,`attempted crosses`,
      `successful crosses` / `attempted crosses` * 100 AS `cross%`,
      `chances created`,`successful headers`,`attempted headers`,
      `successful headers` / `attempted headers` * 100 AS `header%`,
      `key headers`,`dribbles`,`tackles won`,`attempted tackles`,
      `tackles won` / `attempted tackles` * 100 AS `tackle%`,
      `key tackles`,`interceptions`,`clearances`,`mistakes leading to goals`,
      `yellow cards`,`red cards`,`fouls`,`fouls against`,`offsides`,
      `xa`,`xg overperformance`,`fk shots`,`blocks`,
      `open play key passes`,`successful open play crosses`,`attempted open play crosses`,
      `shots blocked`,`progressive passes`,`successful presses`,`attempted presses`,
      `goals outside box`,`average rating`,
      `successful presses` / `attempted presses` * 100 AS `press%`,
      `successful open play crosses` / `attempted open play crosses` * 100 AS `open play crosses%`,
      `shots on target` / `shots` * 100 AS `shot accuracy%`,
      `xG` - 0.83*`penalties taken` AS `pen adj xG`
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
      SUM(`pass%`) AS `pass%`,
      SUM(`key passes`) AS `key passes`,
      SUM(`successful crosses`) AS `successful crosses`,
      SUM(`attempted crosses`) AS `attempted crosses`,
      SUM(`cross%`) AS `cross%`,
      SUM(`chances created`) AS `chances created`,
      SUM(`successful headers`) AS `successful headers`,
      SUM(`attempted headers`) AS `attempted headers`,
      SUM(`header%`) AS `header%`,
      SUM(`key headers`) AS `key headers`,
      SUM(`dribbles`) AS `dribbles`,
      SUM(`tackles won`) AS `tackles won`,
      SUM(`attempted tackles`) AS `attempted tackles`,
      SUM(`tackle%`) AS `tackle%`,
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
        `name`,`club`,`position`,`apps`,`minutes played`,`distance run (km)`,`average rating`,`player of the match`,
        `goals`,`assists`,`xg`,`shots on target`,`shots`,`penalties taken`,`penalties scored`,`successful passes`,
        `attempted passes`,`pass%`,`key passes`,`successful crosses`,`attempted crosses`,`cross%`,`chances created`,
        `successful headers`,`attempted headers`,`header%`,`key headers`,`dribbles`,`tackles won`,`attempted tackles`,
        `tackle%`,`key tackles`,`interceptions`,`clearances`,`mistakes leading to goals`,`yellow cards`,`red cards`,
        `fouls`,`fouls against`,`offsides`,`xa`,`xg overperformance`,`fk shots`,`blocks`,`open play key passes`,
        `successful open play crosses`,`attempted open play crosses`,`shots blocked`,`progressive passes`,
        `successful presses`,`attempted presses`,`goals outside box`
      FROM `gamedataoutfield` AS gd
      JOIN schedule AS s ON gd.gid = s.gid
      ",if_else(league == "ALL",
                "",
                paste("WHERE s.Matchtype = '", league, "'", sep = "")
      ),
      ") `q01`
      GROUP BY `name`
    ) `q01`",
      sep = "")
  ) %>% future_promise()
}


getOutfieldMatchStats <- function(name){
  indexQuery(
    paste(
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
      JOIN teaminformation AS ti ON ti.team = (CASE WHEN g.club = s.home THEN s.away ELSE s.home END)
      WHERE name = ", paste0("'", name %>% str_replace_all(pattern = "'", replacement = "\\\\'"), "'"), " 
      ORDER BY g.gid DESC LIMIT 5;"
    )
  ) %>% 
    future_promise()
}