#* Get bank balance for chosen player
#* @get /getBankBalance
#* @serializer json
#* @param name The player name

function(name){
  portalQuery(
    paste(
      "SELECT sum(transaction) AS balance 
      FROM banktransactions bt
      JOIN playerdata pd ON bt.pid = pd.pid
      WHERE pd.name = ", paste0("'", name, "'"), " AND bt.status = 1;"
    )
  ) %>% 
    suppressWarnings()
}

#* Get bank history for chosen player
#* @get /getBankHistory
#* @serializer json
#* @param name The player name
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
          pd.name = ", paste0("'", name, "'"), "AND bt.status = 1
      ORDER BY Time DESC;")
  ) %>% 
    mutate(
      Time = Time %>% as.numeric() %>% as_datetime(tz = "US/Pacific") %>% as_date()
    )
}