## THESE FUNCTIONS ARE ONLY FOR JOB WORKERS SO SHOULD NOT BE API

getUnfinishedSchedule <- function(){
  indexQuery(
    query = 
      "SELECT * FROM schedule WHERE HomeScore IS NULL OR AwayScore IS NULL;"
  ) %>% 
    future_promise()
}

editUnfinishedSchedule <- function(gid, edits) {
  # Preprocess values: empty strings become NA (SQL NULL)
  homeVal <- if_else(edits$home == "", NA_character_, edits$home)
  awayVal <- if_else(edits$away == "", NA_character_, edits$away)
  
  # Convert values for boolean flags into 1/0
  etVal <- if_else(edits$et, 1L, 0L)
  pVal  <- if_else(edits$p, 1L, 0L)
  
  indexQuery(
    query = "UPDATE schedule 
             SET Home = ?home,
                 Away = ?away,
                 HomeScore = ?hscore,
                 AwayScore = ?ascore,
                 ExtraTime = ?et,
                 Penalties = ?p
             WHERE gid = ?gid;",
    home   = homeVal,
    away   = awayVal,
    hscore = edits$hscore,
    ascore = edits$ascore,
    et     = etVal,
    p      = pVal,
    gid    = gid,
    type   = "set"
  )
}

