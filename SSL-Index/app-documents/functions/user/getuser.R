## THIS ONE SHOULD NOT BE EXPOSED TO API
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


# getUserStatus <- function(playerID){
#   future_promise({
#     portalQuery(
#       paste(
#         "SELECT status.desc
#         FROM playerdata pd
#         JOIN useractivity activity ON pd.uid = activity.uid
#         JOIN userstatuses status ON activity.status_u = status.status
#         WHERE pd.pid =", playerID, ";"
#       )
#     )
#   })
# }



