#* @apiTitle Organization API
#* @apiDescription Endpoints to get organization information.

#* Get all organization data
#* @get /getOrganizations
#* @serializer json
#* 
function(){
  portalQuery("SELECT o.ID, o.name AS organization, o.abbr AS abbreviation, t.name, t.primaryColor, t.secondaryColor, t.city 
              FROM teams AS t 
              LEFT JOIN organizations AS o ON t.orgID = o.ID 
              ORDER BY o.ID")
}

#* Get all manager data
#* @get /getManagers
#* @serializer json
#* 
function(){
  portalQuery(
    paste(
      "SELECT organizations.id, teams.name, teams.primaryColor, managers.orgManager, managers.assManager1, managers.assManager2 
      FROM organizations
      LEFT JOIN managers ON organizations.id = managers.orgID
      LEFT JOIN teams ON organizations.id = teams.orgID
      WHERE teams.affiliate = 1;"
    )
    # ORDER BY pid DESC
    
    ## NEED TO ADD SOMETHING THAT ONLY TAKES BACK ONE PLAYER IF SOMEONE HAS RECREATED
  )
}


#* Get all players from the same organization as the user
#* @get /getPlayersFromOrg
#* @serializer json
#* @param uid
#* 
#* 
function(uid){
  portalQuery(
    query = 
      "SELECT 
       us.desc AS `user status`, 
       ps.desc AS `player status`, 
       pd.name, 
       pd.class, 
       pd.tpe, 
       pd.tpebank, 
       `left foot`, 
       `right foot`, 
       pd.position, 
       (CASE WHEN teams.affiliate = 2 THEN 'Minor' ELSE 'Major' END) AS affiliate, 
       pid
     FROM playerdata pd
     JOIN useractivity ua ON pd.uid = ua.uid
     JOIN userstatuses us ON ua.status_u = us.status
     JOIN playerstatuses ps ON pd.status_p = ps.status
     JOIN teams ON pd.team = teams.orgID 
                AND pd.affiliate = teams.affiliate
     WHERE pd.team IN (
       SELECT orgID
       FROM managers
       WHERE orgManager = ?uid 
          OR assManager1 = ?uid 
          OR assManager2 = ?uid
     );",
    uid = uid
  ) %>% 
    arrange(affiliate, tpe %>% desc())
}
