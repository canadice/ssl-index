#* Get bank balance for chosen player
#* @get /getBankBalance
#* @serializer json
#* @param name The player name
#* @param pid The player ID

function(name = "", pid = ""){
  if(pid != ""){
    portalQuery(
      query  = "SELECT sum(transaction) AS balance 
        FROM banktransactions bt
        WHERE bt.pid = ?pid AND bt.status = 1",
      pid = pid
    ) %>% 
      mutate(balanceStr = paste0("$", comma(balance))) %>% 
      suppressWarnings()
  } else {
    portalQuery(
      query = 
          "SELECT sum(transaction) AS balance 
          FROM banktransactions bt
          JOIN playerdata pd ON bt.pid = pd.pid
          WHERE pd.name = ?name AND bt.status = 1",
      name = name %>% str_replace(pattern = "'", replacement = "\\\\'")
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
    query = 
      "SELECT 
          Time, Username AS User, Source, Transaction
      FROM 
          bankhistoryview
      WHERE 
          Player = ?name AND Status = 1
      ORDER BY Time DESC;",
    name = name
  ) %>% 
    mutate(
      Time = Time %>% as.numeric() %>% as_datetime(tz = "US/Pacific") %>% as_date(),
      Transaction = paste0("$", comma(Transaction))
    )
}

#* Get bank transactions from player using pid or all of a specific status
#* @get /getBankTransactions
#* @serializer json list(digits = 5)
#* @param pid The player ID, leave blank to get all players
#* @param status The status of the transaction where 0 is pending approval and 1 is approved
#* 
function(pid = -1, status = 1){
  
  if(pid < 0){
    portalQuery(
      query = 
        "SELECT Time, Player, Username, Source, Transaction, pid 
        FROM 
          bankhistoryview
        WHERE
          Status = ?status
      ORDER BY Time DESC;",
      status = status
    )
  } else {
    portalQuery(
      query = 
        "SELECT Time, Player, Username, Source, Transaction, pid 
        FROM 
          bankhistoryview
        WHERE
          pid = ?pid AND Status = ?status
      ORDER BY Time DESC;",
      pid = pid,
      status = status
    )
  }
}