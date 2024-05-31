getUserID <- function(name){
  future_promise({
    mybbQuery(
      paste(
        "SELECT uid
          FROM mybb_users
          WHERE username = '", name, "'",
        sep = ""
      )
    )
  })
}

getUserName <- function(uid){
  future_promise({
    mybbQuery(
      paste(
        "SELECT username
          FROM mybb_users
          WHERE uid =", uid, ";"
      )
    )
  })
}

