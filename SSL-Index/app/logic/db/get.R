box::use(
  dplyr,
  lubridate,
  promises[future_promise],
)

box::use(
  app/logic/db/api[readAPI],
  app/logic/db/database[portalQuery],
)

#' @export
getUpdateHistory <- function(pid) {
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
getTpeHistory <- function(pid) {
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

#' @export
getBankHistory <- function(pid) {
  readAPI(
    "https://api.simulationsoccer.com/bank/getBankTransactions",
    query = list(pid = pid)
  ) |>
    future_promise()
}

#' @export
getRecentCreates <- function() {
  portalQuery(
    "SELECT pd.name, mb.username, pd.position
    FROM playerdata pd
    LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
    ORDER BY pd.created DESC
    LIMIT 10;"
  )
}

#' @export
getTopEarners <- function() {
  portalQuery(
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
        YEARWEEK(FROM_UNIXTIME(ph.time), 1) = YEARWEEK(CONVERT_TZ(CURDATE(), 'UTC', 'America/Los_Angeles'), 1) AND ph.source <> 'Initial TPE' AND ph.tpe > 0
    GROUP BY 
        ph.pid
    ORDER BY 
        `TPE Earned` DESC
    LIMIT 10;"
  )
}