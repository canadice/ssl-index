#* @apiTitle Index API
#* @apiDescription Endpoints to get index information.


#* Gets next gameID that is not present in the indexdata
#* @get /nextGame
#* @param season The season to search
#* @serializer json

function(season) {
  indexQuery(
    paste(
      "SELECT team, MIN(gid) AS gid
        FROM (
            SELECT home AS team, MIN(s.gid) AS gid
            FROM schedule s
            LEFT JOIN gamedataoutfield o ON s.gid = o.gid
            WHERE o.gid IS NULL AND s.season =", season, " AND s.Matchtype >= 0
            GROUP BY home
        
            UNION ALL
        
            SELECT away AS team, MIN(s.gid) AS gid
            FROM schedule s
            LEFT JOIN gamedataoutfield o ON s.gid = o.gid
            WHERE o.gid IS NULL AND s.season =", season, " AND s.Matchtype >= 0
            GROUP BY away
        ) AS combined
        GROUP BY team
        ORDER BY gid;"
    )
  )
}

#* Gets outfield index data for season and league
#* @get /outfield
#* @param season The selected season
#* @param league The selected league

function(league, season){
  indexQuery(
    paste(
      "SELECT
      `name`, `club`, `position`, `apps`, `minutes played`,`player of the match`,`distance run (km)`, 
      `goals`,`assists`,`xg`,`shots on target`,`shots`,`penalties taken`,`penalties scored`,`successful passes`,
      `attempted passes`,`successful passes` / `attempted passes` * 100 AS `pass%`,`key passes`,  
      `successful crosses`,`attempted crosses`,`successful crosses` / `attempted crosses` * 100 AS `cross%`,
      `chances created`,`successful headers`,`attempted headers`,`successful headers` / `attempted headers` * 100 AS `header%`,
      `key headers`,`dribbles`,`tackles won`,`attempted tackles`,`tackles won` / `attempted tackles` * 100 AS `tackle%`,
      `key tackles`,`interceptions`,`clearances`,`mistakes leading to goals`,`yellow cards`,`red cards`,
      `fouls`,`fouls against`,`offsides`,`xa`,`xg overperformance`,`fk shots`,`blocks`,`open play key passes`,
      `successful open play crosses`,`attempted open play crosses`,`shots blocked`,`progressive passes`,
      `successful presses`,`attempted presses`,`goals outside box`,`average rating`,
      CASE 
          WHEN IFNULL(`attempted presses`, 0) = 0 THEN 0
          ELSE (`successful presses` / `attempted presses`) * 100
  	  END AS `press%`,
        CASE 
          WHEN IFNULL(`attempted open play crosses`, 0) = 0 THEN 0
          ELSE (`successful open play crosses` / `attempted open play crosses`) * 100
      END AS `open play crosses%`,
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
      ",if_else(league == "ALL" & season == "ALL",
                "",
                if_else(league == "ALL",
                        paste("WHERE s.season = ", season, sep = ""),
                        if_else(season == "ALL",
                                paste("WHERE s.Matchtype = ", league, sep = ""),
                                paste("WHERE s.Matchtype = '", league, "' AND s.season = ", season, sep = ""))
                )
      ),
      ") `q01`
      GROUP BY `name`
    ) `q01`",
      sep = "")
  )
}

#* Gets keeper index data for season and league
#* @get /keeper
#* @param season The selected season
#* @param league The selected league
#* 
function(league, season){
  indexQuery(
    paste(
      "SELECT
      `name`, `club`, `apps`, `minutes played`, `average rating`, `player of the match`, SUM(won) AS won, 
      SUM(lost) AS lost, SUM(drawn) AS drawn,  `clean sheets`, conceded, `saves parried`, `saves held`, 
      `saves tipped`, (1 - (conceded / (conceded + `saves parried` + `saves held` + `saves tipped`))) * 100 AS `save%`,
      `penalties faced`, `penalties saved`, `xsave%`, `xg prevented`
  FROM (
    SELECT
      `name`,
      GROUP_CONCAT(DISTINCT club SEPARATOR ', ') AS `club`,
      SUM(`apps`) AS `apps`,
      SUM(`minutes played`) AS `minutes played`,
      AVG(`average rating`) AS `average rating`,
      SUM(`player of the match`) AS `player of the match`,
      SUM(CASE 
            WHEN (Home = club AND HomeScore > AwayScore) OR 
                 (Away = club AND AwayScore > HomeScore)
            THEN 1 ELSE 0 END) AS `won`,
        SUM(CASE 
            WHEN (Home = club AND HomeScore < AwayScore) OR 
                 (Away = club AND AwayScore < HomeScore)
            THEN 1 ELSE 0 END) AS `lost`,
        SUM(CASE 
            WHEN HomeScore = AwayScore THEN 1 ELSE 0 END) AS `drawn`,
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
      FROM `gamedatakeeper`AS gd
      JOIN schedule AS s ON gd.gid = s.gid
      ",if_else(league == "ALL" & season == "ALL",
                "",
                if_else(league == "ALL",
                        paste("WHERE s.season = ", season, sep = ""),
                        if_else(season == "ALL",
                                paste("WHERE s.Matchtype = ", league, sep = ""),
                                paste("WHERE s.Matchtype = '", league, "' AND s.season = ", season, sep = ""))
                )
      ),
      ") `q01`
      GROUP BY `name`
    ) `q02`
    GROUP BY `name`",
      sep = "")
  )
}