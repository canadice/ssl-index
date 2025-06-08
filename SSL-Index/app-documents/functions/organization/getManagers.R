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

editManagers <- function(managers, id) {
  # Pre-process the values: convert empty strings to NA
  orgManager  <- if_else(managers[1] == "", NA_character_, as.character(managers[1]))
  assManager1 <- if_else(managers[2] == "", NA_character_, as.character(managers[2]))
  assManager2 <- if_else(managers[3] == "", NA_character_, as.character(managers[3]))
  
  portalQuery(
    query = "UPDATE managers
             SET orgManager = ?orgManager,
                 assManager1 = ?assManager1,
                 assManager2 = ?assManager2
             WHERE orgID = ?id;",
    orgManager  = orgManager,
    assManager1 = assManager1,
    assManager2 = assManager2,
    id          = id,
    type        = "set"
  )
}
