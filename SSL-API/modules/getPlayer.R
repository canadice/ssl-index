#* @apiTitle Player API
#* @apiDescription Endpoints to get player information.

#* Allows acces from cross domain places
# Enable CORS Filtering
#' @filter cors
cors <- function(req, res) {
  safe_domains <- c("https://api.simulationsoccer.com", 
                    "https://forum.simulationsoccer.com",
                    "https://index.simulationsoccer.com")
  
  if (any(grepl(pattern = paste0(safe_domains,collapse="|"), req$HTTP_REFERER,ignore.case=T))) {
    res$setHeader("Access-Control-Allow-Origin", sub("/$","",req$HTTP_REFERER)) #Have to remove last slash, for some reason
    
    if (req$REQUEST_METHOD == "OPTIONS") {
      res$setHeader("Access-Control-Allow-Methods","GET,HEAD,PUT,PATCH,POST,DELETE") #This is how node.js does it
      res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
      res$status <- 200
      return(list())
    } else {
      plumber::forward()
    }
  } else {
    plumber::forward()
  }
}


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
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
        ORDER BY pd.name DESC;"
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

#* Get draft class leaderboards
#* @get /getDraftClass
#* @serializer json
#* @param class The class leaderboard
#* 
function(class = NULL) {
  # If no class is given it defaults to the youngest
  if(class %>% is.null()){
    class <- indexQuery(paste("SELECT season FROM seasoninfo ORDER BY season DESC LIMIT 1;")) %>% unlist() + 1
  }
  
  portalQuery(
    paste(
      "SELECT pd.name, pd.tpe, mb.username, us.desc AS `userStatus`, ps.desc AS `playerStatus`
        FROM playerdata pd
        LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
        LEFT JOIN useractivity ua ON pd.uid = ua.uid
        LEFT JOIN userstatuses us ON ua.status_u = us.status
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
      WHERE pd.class = ", paste0("'S", class, "'"), " AND pd.status_p > 0
      ORDER BY pd.tpe DESC;"
    )
  )
}