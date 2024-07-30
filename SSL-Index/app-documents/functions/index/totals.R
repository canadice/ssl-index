getKeeperSeasonTotal <- function(season){
  indexQuery(
    paste(
      "SELECT
        name,
        club,
        SUM(apps) AS apps,
        SUM(`minutes played`) AS `minutes played`,
        AVG(`average rating`) AS `average rating`,
        SUM(`player of the match`) AS `player of the match`,
        SUM(`clean sheets`) AS `clean sheets`,
        SUM(conceded) AS conceded,
        SUM(`saves parried`) AS `saves parried`,
        SUM(`saves held`) AS `saves held`,
        SUM(`saves tipped`) AS `saves tipped`,
        SUM(`save%`) AS `save%`,
        SUM(`penalties faced`) AS `penalties faced`,
        SUM(`penalties saved`) AS `penalties saved`,
        AVG(`xsave%`) AS `xsave%`,
        SUM(`xg prevented`) AS `xg prevented`
      FROM
          gamedatakeeper AS gd
      JOIN
          schedule AS s ON gd.gid = s.gid
      WHERE
          s.Season = ", season, "
      GROUP BY
          Name, Club
      ORDER BY
          Name, Club;"
    )
  )  
}


getOutfieldSeasonTotal <- function(season){
  indexQuery(
    paste(
      "SELECT
        name,
        club,
        SUM(apps) AS apps,
        SUM(`minutes played`) AS `minutes played`,
        SUM(`distance run (km)`) AS `distance run (km)`,
        AVG(`average rating`) AS `average rating`,
        SUM(`player of the match`) AS `player of the match`,
        SUM(goals) AS goals,
        SUM(assists) AS assists,
        SUM(xg) AS xg,
        SUM(`shots on target`) AS `shots on target`,
        SUM(shots) AS shots,
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
        SUM(dribbles) AS dribbles,
        SUM(`tackles won`) AS `tackles won`,
        SUM(`attempted tackles`) AS `attempted tackles`,
        SUM(`tackle%`) AS `tackle%`,
        SUM(`key tackles`) AS `key tackles`,
        SUM(interceptions) AS interceptions,
        SUM(clearances) AS clearances,
        SUM(`mistakes leading to goals`) AS `mistakes leading to goals`,
        SUM(`yellow cards`) AS `yellow cards`,
        SUM(`red cards`) AS `red cards`,
        SUM(fouls) AS fouls,
        SUM(`fouls against`) AS `fouls against`,
        SUM(offsides) AS offsides,
        SUM(xa) AS xa,
        SUM(`xg overperformance`) AS `xg overperformance`,
        SUM(`goals outside box`) AS `goals outside box`,
        SUM(`fk shots`) AS `fk shots`,
        SUM(blocks) AS blocks,
        SUM(`open play key passes`) AS `open play key passes`,
        SUM(`successful open play crosses`) AS `successful open play crosses`,
        SUM(`attempted open play crosses`) AS `attempted open play crosses`,
        SUM(`shots blocked`) AS `shots blocked`,
        SUM(`progressive passes`) AS `progressive passes`,
        SUM(`successful presses`) AS `successful presses`,
        SUM(`attempted presses`) AS `attempted presses`
      FROM
          gamedataoutfield AS gd
      JOIN
          schedule AS s ON gd.gid = s.gid
      WHERE
          s.Season = ", season, "
      GROUP BY
          Name, Club
      ORDER BY
          Name, Club;"
    )
  )  
}





