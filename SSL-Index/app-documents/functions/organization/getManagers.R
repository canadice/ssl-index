# getManagers <- function(){
#   future_promise({
#     portalQuery(
#       paste(
#         "SELECT organizations.id, teams.name, teams.primaryColor, managers.orgManager, managers.assManager1, managers.assManager2 
#         FROM organizations
#         LEFT JOIN managers ON organizations.id = managers.orgID
#         LEFT JOIN teams ON organizations.id = teams.orgID
#         WHERE teams.affiliate = 1;"
#       )
#       # ORDER BY pid DESC
#       
#       ## NEED TO ADD SOMETHING THAT ONLY TAKES BACK ONE PLAYER IF SOMEONE HAS RECREATED
#     )
#   })
# }

editManagers <- function(managers, id){
  portalQuery(
    paste(
      "UPDATE managers SET orgManager = ", if_else(managers[1] == "", "NULL", managers[1] %>% as.character()), 
      ", assManager1 = ", if_else(managers[2] == "","NULL", managers[2] %>% as.character()), 
      ", assManager2 = ", if_else(managers[3] == "", "NULL", managers[3] %>% as.character()),
      "WHERE orgID = ", id, ";"
    )
  )
}