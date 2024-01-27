#### Bank Functions ####

#* Get the bank balance of a given user/player
#* @param user The username of the user to search for
#* @serializer json
#* @get /getBankBalance
function(user = "") {
  
  if(user == ""){
    return(0)
  } 
  
  ## Downloads the bank data only if 
  if(file.exists(paste("bankBuffer.RData"))){
    load(file = paste("bankBuffer.RData"))
    
    # Only updates after six hours
    if(difftime(Sys.time(), now, units = "hours") < 6){
      # DO NOTHING
    } else {
      sheet <- 
        googlesheets4::read_sheet(
          ss = "https://docs.google.com/spreadsheets/d/1hY1ArnfTICZx0lZF3PTGA922u49avs6X5hKNyldbAn0/edit?usp=sharing", 
          sheet = "Shorrax Import Player Pool"
        )
      
      now <- Sys.time()
      
      save("sheet", "now", file = paste("bankBuffer.RData"))
    }
  } else {
    sheet <- 
      googlesheets4::read_sheet(
        ss = "https://docs.google.com/spreadsheets/d/1hY1ArnfTICZx0lZF3PTGA922u49avs6X5hKNyldbAn0/edit?usp=sharing", 
        sheet = "Shorrax Import Player Pool"
      )
    
    now <- Sys.time()
    
    save("sheet", "now", file = paste("bankBuffer.RData"))
  }
  
  balance <- 
    sheet %>% 
    filter(
      Username == user
    ) %>% 
    select(Balance)
  
  if(balance %>% nrow() != 1){
    return(0)
  } else {
    return(balance)
  }
  
  
}
