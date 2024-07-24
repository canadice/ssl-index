#* @apiTitle Player API
#* @apiDescription Endpoints to get player information.

#* Get all players from the portal database
#* @get /getAllPlayers
#* @serializer json
#* 
#* 
function() {
  portalQuery(
    paste(
      "SELECT pd.*, mb.username, us.desc AS `userStatus`, ps.desc AS `playerStatus`, 
            CASE 
              WHEN pd.tpe <= 350 THEN 1000000
              WHEN pd.tpe BETWEEN 351 AND 500 THEN 1500000
              WHEN pd.tpe BETWEEN 501 AND 650 THEN 2000000
              WHEN pd.tpe BETWEEN 651 AND 800 THEN 2500000
              WHEN pd.tpe BETWEEN 801 AND 950 THEN 3000000
              WHEN pd.tpe BETWEEN 951 AND 1100 THEN 3500000
              WHEN pd.tpe BETWEEN 1101 AND 1250 THEN 4000000
              WHEN pd.tpe BETWEEN 1251 AND 1400 THEN 4500000
              WHEN pd.tpe BETWEEN 1401 AND 1550 THEN 5000000
              WHEN pd.tpe BETWEEN 1551 AND 1700 THEN 5500000
              WHEN pd.tpe > 1700 THEN 6000000
              ELSE NULL
          END AS `minimum salary`
        FROM playerdata pd
        LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
        LEFT JOIN useractivity ua ON pd.uid = ua.uid
        LEFT JOIN userstatuses us ON ua.status_u = us.status
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status;"
    )
  )
}

#* Get single players from the portal database
#* @get /getPlayer
#* @serializer json
#* @param name 
#* 
function(name) {
  portalQuery(
    paste(
      "SELECT pd.*, mb.username, us.desc AS `userStatus`, ps.desc AS `playerStatus`, 
            CASE 
              WHEN pd.tpe <= 350 THEN 1000000
              WHEN pd.tpe BETWEEN 351 AND 500 THEN 1500000
              WHEN pd.tpe BETWEEN 501 AND 650 THEN 2000000
              WHEN pd.tpe BETWEEN 651 AND 800 THEN 2500000
              WHEN pd.tpe BETWEEN 801 AND 950 THEN 3000000
              WHEN pd.tpe BETWEEN 951 AND 1100 THEN 3500000
              WHEN pd.tpe BETWEEN 1101 AND 1250 THEN 4000000
              WHEN pd.tpe BETWEEN 1251 AND 1400 THEN 4500000
              WHEN pd.tpe BETWEEN 1401 AND 1550 THEN 5000000
              WHEN pd.tpe BETWEEN 1551 AND 1700 THEN 5500000
              WHEN pd.tpe > 1700 THEN 6000000
              ELSE NULL
          END AS `minimum salary`
        FROM playerdata pd
        LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
        LEFT JOIN useractivity ua ON pd.uid = ua.uid
        LEFT JOIN userstatuses us ON ua.status_u = us.status
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
      WHERE pd.name = ", paste0("'", name, "'"), ";"
    )
  )
}