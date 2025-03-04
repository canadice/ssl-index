box::use(
  dplyr,
  lubridate,
  promises[future_promise],
)

box::use(
  app/logic/db/database[portalQuery],
)

#' @export
getUpdateHistory <- function(pid){
  portalQuery(
    paste("SELECT 
            uh.time AS Time,
            mbb.username AS Username,
            uh.attribute AS `Changed attribute`,
            uh.old AS `From`,
            uh.new AS `To`
        FROM 
            updatehistory uh
        LEFT JOIN
            mybbdb.mybb_users mbb ON uh.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY Time DESC")
  ) |> 
    dplyr$mutate(
      Time = Time |> 
        as.numeric() |> 
        lubridate$as_datetime(tz = "US/Pacific")
    ) |> 
    future_promise()
}

#' @export
getTpeHistory <- function(pid){
  portalQuery(
    paste("SELECT 
            tpeh.time AS Time,
            mbb.username AS Username,
            tpeh.source AS Source,
            tpeh.tpe AS `TPE Change`
        FROM 
            tpehistory tpeh
        LEFT JOIN
            mybbdb.mybb_users mbb ON tpeh.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY time DESC")
  ) |> 
    dplyr$mutate(
      Time = Time |> 
        as.numeric() |> 
        lubridate$as_datetime(tz = "US/Pacific")
    ) |> 
    future_promise()
}

