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
            pd.name AS Player,
            mbb.username AS Username,
            bt.source AS Source,
            bt.transaction AS Transaction
        FROM 
            banktransactions bt
        LEFT JOIN
            mybbdb.mybb_users mbb ON bt.uid = mbb.uid
        LEFT JOIN playerdata pd ON bt.pid = pd.pid",
            if_else(pid < 0, "WHERE bt.status = 1", paste("WHERE bt.pid = ", pid, " AND bt.status = 1")),
        "ORDER BY Time DESC;")
    ) %>% 
      mutate(
        Time = Time %>% as.numeric() %>% as_datetime(tz = "US/Pacific")
      )
  })
}

getBankTransactionsForApproval <- function(){
  future_promise({
    portalQuery(
      paste("SELECT 
            bt.time AS Time,
            mbb.username AS `Deposited by`,
            bt.pid AS pid,
            pd.name AS Player,
            bt.source AS Source,
            bt.transaction AS Amount
        FROM 
            banktransactions bt
        LEFT JOIN
            mybbdb.mybb_users mbb ON bt.uid = mbb.uid
        LEFT JOIN
            playerdata pd ON bt.pid = pd.pid
        WHERE 
            status = 0
        ORDER BY Time DESC;")
    ) %>% 
      mutate(
        Time = Time %>% as.numeric()
      )
  })
}