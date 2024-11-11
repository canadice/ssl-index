#* Get bank balance for chosen player
#* @get /getBankBalance
#* @serializer json
#* @param name The player name
#* @param pid The player ID

function(name = "", pid = ""){
  if(pid != ""){
    portalQuery(
      paste(
        "SELECT sum(transaction) AS balance 
      FROM banktransactions bt
      WHERE bt.pid = ", pid, " AND bt.status = 1;"
      )
    ) %>% 
      mutate(balanceStr = paste0("$", comma(balance))) %>% 
      suppressWarnings()
  } else {
    portalQuery(
      paste(
        "SELECT sum(transaction) AS balance 
      FROM banktransactions bt
      JOIN playerdata pd ON bt.pid = pd.pid
      WHERE pd.name = ", paste0("'", name %>% str_replace(pattern = "'", replacement = "\\\\'"), "'"), " AND bt.status = 1;"
      )
    ) %>% 
      mutate(balanceStr = paste0("$", comma(balance))) %>% 
      suppressWarnings()
  }
  
}

#* Get bank history for chosen player
#* @get /getBankHistory
#* @serializer json
#* @param name The player name
#* 
function(name){
  portalQuery(
    paste("SELECT 
          bt.time AS Time,
          mbb.username AS `User`,
          bt.source AS Source,
          bt.transaction AS Transaction
      FROM 
          banktransactions bt
      LEFT JOIN
          mybbdb.mybb_users mbb ON bt.uid = mbb.uid
      LEFT JOIN playerdata pd ON bt.pid = pd.pid
      WHERE 
          pd.name = ", paste0("'", name %>% str_replace(pattern = "'", replacement = "\\\\'"), "'"), "AND bt.status = 1
      ORDER BY Time DESC;")
  ) %>% 
    mutate(
      Time = Time %>% as.numeric() %>% as_datetime(tz = "US/Pacific") %>% as_date(),
      Transaction = paste0("$", comma(Transaction))
    )
}

#* Get bank transactions from player using pid or all of a specific status
#* @get /getBankTransactions
#* @serializer json
#* @param pid The player ID, leave blank to get all players
#* @param status The status of the transaction where 0 is pending approval and 1 is approved
#* 
function(pid = -1, status = 1){
  portalQuery(
    paste("SELECT 
          bt.time AS Time,
          pd.name AS Player,
          pd.pid AS pid,
          mbb.username AS Username,
          bt.source AS Source,
          bt.transaction AS Transaction
      FROM 
          banktransactions bt
      LEFT JOIN
          mybbdb.mybb_users mbb ON bt.uid = mbb.uid
      LEFT JOIN playerdata pd ON bt.pid = pd.pid",
          if_else(pid < 0, paste("WHERE bt.status = ", status), paste("WHERE bt.pid = ", pid, " AND bt.status =  ", status)),
          "ORDER BY Time DESC;")
  ) %>% 
    mutate(
      Time = Time %>% as.numeric() %>% as_datetime(tz = "US/Pacific")
    )
}