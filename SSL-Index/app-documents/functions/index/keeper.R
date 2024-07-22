getKeeperIndex <- function(league, season){
  indexQuery(
    paste(
      "SELECT
      `name`, `club`, `apps`, `minutes played`,
      `average rating`, `player of the match`, 
      won, lost, drawn, `clean sheets`,
      conceded, `saves parried`, `saves held`, `saves tipped`,
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
      SUM(`won`) AS `won`,
      SUM(`lost`) AS `lost`,
      SUM(`drawn`) AS `drawn`,
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
        `position`,
        `apps`,
        `minutes played`,
        `average rating`,
        `player of the match`,
        `won`,
        lost,
        drawn,
        `clean sheets`,
        `conceded`,
        `saves parried`,
        `saves held`,  
        `saves tipped`,  
        `save%`,  
        `penalties faced`,  
        `penalties saved`,
        `xsave%`, 
        `xg prevented`
      FROM `gamedatakeeper`",
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

getKeeperCareer <- function(league){
  indexQuery(
    paste(
      "SELECT
      `name`, `club`, `apps`, `minutes played`,
      `average rating`, `player of the match`, 
      won, lost, drawn, `clean sheets`,
      conceded, `saves parried`, `saves held`, `saves tipped`,
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
      SUM(`won`) AS `won`,
      SUM(`lost`) AS `lost`,
      SUM(`drawn`) AS `drawn`,
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
        `position`,
        `apps`,
        `minutes played`,
        `average rating`,
        `player of the match`,
        `won`,
        lost,
        drawn,
        `clean sheets`,
        `conceded`,
        `saves parried`,
        `saves held`,  
        `saves tipped`,  
        `save%`,  
        `penalties faced`,  
        `penalties saved`,
        `xsave%`, 
        `xg prevented`
      FROM `gamedatakeeper`",
      if_else(league == "ALL", 
              "",
              paste("WHERE division = '", league, "'", sep = "")
      ),
      ") `q01`
      GROUP BY `name`
    ) `q01`",
      sep = "")
  ) %>% future_promise()
}


getKeeperMatchStats <- function(name){
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
        (g.`saves parried`+g.`saves parried`+g.`saves tipped`) AS `total saves`,
        g.`save%`,
        g.`xg prevented`
      FROM `gamedatakeeper` AS g 
      JOIN schedule AS s ON g.gid = s.gid
      JOIN teaminformation AS ti ON ti.team = (CASE WHEN g.club = s.home THEN s.away ELSE s.home END)
      WHERE name = ", paste0("'", name, "'"), " 
      ORDER BY g.gid DESC LIMIT 5;"
    )
  ) %>% 
    future_promise()
}