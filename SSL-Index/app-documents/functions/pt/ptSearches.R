topEarners <- function(){
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
            YEARWEEK(FROM_UNIXTIME(ph.time), 1) = YEARWEEK(CURDATE(), 1) AND ph.source <> 'Initial TPE' AND ph.tpe > 0
        GROUP BY 
            ph.pid
        ORDER BY 
            `TPE Earned` DESC
        LIMIT 10;"
    )
  ) %>% 
    future_promise()
}