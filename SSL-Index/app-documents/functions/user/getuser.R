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
  }) %>% 
    suppressWarnings()
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

getUsers <- function(){
  future_promise({
    mybbQuery(
      paste(
        "SELECT uid, username
          FROM mybb_users;
          "
      )
    )
  })
}


getUserStatus <- function(pid){
  future_promise({
    portalQuery(
      paste(
        "SELECT status.desc
        FROM playerdata pd
        JOIN useractivity activity ON pd.uid = activity.uid
        JOIN userstatuses status ON activity.status_u = status.status
        WHERE pd.pid =", pid, ";"
      )
    )
  })
}



