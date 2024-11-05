# getActiveTeams <- function(){
#   portalQuery(
#     paste(
#       "SELECT * FROM teams WHERE orgID IS NOT NULL;"
#     )
#   ) %>% 
#     future_promise()
# }
# 
# getOrganizations <- function(){
  # portalQuery(
  #   paste(
  #     "SELECT * FROM organizations;"
  #   )
  # ) %>%
#     future_promise()
# }
