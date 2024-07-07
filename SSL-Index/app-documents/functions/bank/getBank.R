getBankTotal <- function(pid){
  future_promise({
    portalQuery(
      paste(
        "SELECT sum(transaction) AS balance FROM banktransactions WHERE pid = ", pid, " AND status = 1;"
      )
    ) %>% 
      suppressWarnings()
  })
}


getBankTransactions <- function(pid){
  future_promise({
    portalQuery(
      paste("SELECT 
            bt.time AS Time,
            mbb.username AS Username,
            bt.source AS Source,
            bt.transaction AS Transaction
        FROM 
            banktransactions bt
        LEFT JOIN
            mybbdb.mybb_users mbb ON bt.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY Time DESC")
    ) %>% 
      mutate(
        Time = Time %>% as.numeric() %>% as_datetime(tz = "US/Pacific")
      )
  })
}