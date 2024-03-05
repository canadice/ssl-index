## Verifies the password from a response from the mybb db and the given password
customCheckCredentials <- function(){
  function(user, password) {
    res <- 
      mybbQuery(
        query = 
          paste(
          "SELECT uid, username, password, salt, usergroup
          FROM mybb_users
          WHERE username = '", user, "'",
          sep = ""
          )
      )
    
    if(nrow(res) == 1){
      saltedPASS <- 
        paste(
          digest::digest(
            res$salt, 
            algo = "md5", 
            serialize = F
          ), 
          digest::digest(
            password, 
            algo = "md5", 
            serialize = F
          ), 
          sep = "") %>% 
        digest::digest(algo = "md5", serialize = F)
      
      if(saltedPASS != res$password) {
        list(result = FALSE) %>% 
          return()
      } 
      
      list(
        result = TRUE, 
        ## user_info is hardcoded in the shinymanager auth function so cannot be renamed
        user_info = 
          list(
            uid = res$uid, 
            username = res$username, 
            usergroup = res$usergroup
          )
      ) %>% 
        return()
      
    } else {
      list(result = FALSE) %>% 
        return()
    }
  }
}



