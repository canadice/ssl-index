## Verifies the password from a response from the mybb db and the given password
customCheckCredentials <- function(session = shiny::getDefaultReactiveDomain()){
  function(user, password, session = shiny::getDefaultReactiveDomain()) {
    res <- 
      mybbQuery(
        query = 
          paste(
          "SELECT uid, username, password, salt, usergroup, additionalgroups, suspendposting
          FROM mybb_users
          WHERE username = '", user, "'",
          sep = ""
          )
      ) %>% 
      suppressWarnings()
    
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
      } else{
        token <- paste0(replicate(n = 4, expr = stri_rand_strings(1, length = 4)), collapse = "-")
        
        setRefreshToken(uid = res$uid, token = token)
        
        msg <- list(
          name = "token", value = token
        )
        
        session$sendCustomMessage("cookie-set", msg)
        
        list(
          result = TRUE, 
          ## user_info is hardcoded in the shinymanager auth function so cannot be renamed
          user_info = 
            list(
              uid = res$uid, 
              username = res$username, 
              usergroup = 
                paste(res$usergroup, res$additionalgroups, sep = ",") %>% 
                str_split(pattern = ",", simplify = TRUE) %>%
                as.numeric() %>% 
                as.list(),
              suspended = res$suspendposting == 1
            )
        ) %>% 
          return()
      }
      
    } else {
      list(result = FALSE) %>% 
        return()
    }
  }
}

getRefreshToken <- function(token){
  portalQuery(
    query = 
    "SELECT * 
    FROM currentrefreshtokensview
    WHERE token = ?token;",
    token = token
  ) %>% 
    suppressWarnings()
}

setRefreshToken <- function(uid, token, session = shiny::getDefaultReactiveDomain()){
  expires <- (now() + hours(72)) %>% as.numeric()
  
  portalQuery(
    query = 
      "INSERT INTO refreshtokens (uid, expires_at, token)
      VALUES ( ?uid, ?expires, ?token )
      ON DUPLICATE KEY UPDATE token = ?token, expires_at = ?expires;",
    uid = uid,
    token = token,
    expires = expires,
    type = "set"
  )
}



