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