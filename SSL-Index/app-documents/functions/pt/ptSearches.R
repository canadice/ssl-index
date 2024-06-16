topEarners <- function(){
  portalQuery(
    paste(
      "SELECT 
            pd.name AS Name,
            SUM(ph.tpe) AS `TPE Earned`
        FROM 
            tpehistory ph
        JOIN 
            playerData pd ON ph.pid = pd.pid
        WHERE 
            YEARWEEK(FROM_UNIXTIME(ph.time), 1) = YEARWEEK(CURDATE(), 1)
        GROUP BY 
            ph.pid
        ORDER BY 
            `TPE Earned` DESC
        LIMIT 10;"
    )
  ) %>% 
    future_promise()
}