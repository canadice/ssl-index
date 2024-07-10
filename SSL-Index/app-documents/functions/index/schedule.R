getUnfinishedSchedule <- function(){
  indexQuery(
    paste(
      "SELECT * FROM schedule WHERE HomeScore IS NULL OR AwayScore IS NULL;"
    )
  ) %>% 
    future_promise()
}

editUnfinishedSchedule <- function(gid, edits){
  indexQuery(
    paste(
      "UPDATE schedule SET Home = ", if_else(edits$home == "", "NULL", paste0("'", edits$home, "'")),
      ", Away = ", if_else(edits$away == "", "NULL", paste0("'", edits$away, "'")),
      ", HomeScore = ", edits$hscore,
      ", AwayScore = ", edits$ascore,
      ", ExtraTime = ", if_else(edits$et, 1, 0),
      ", Penalties = ", if_else(edits$p, 1, 0),
      "WHERE gid = ", gid, ";"
    )
  )
}
