#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

require(plumber)
require(RMySQL)
require(dplyr)
require(lubridate)
require(scales)
require(tidyr)
require(stringr)

#* @apiTitle Player API
#* @apiDescription Endpoints to get player information.

if(Sys.info()["sysname"] == "Linux"){
  Sys.setenv(HOME = "/root")  
}

source("functions/database.R")

root <- pr() %>% 
  pr_set_api_spec(function(spec) {
    spec$info$title <- "SSL Portal API" 
    spec$info$description <- "This API is connected to the SSL Portal and grants access to various different databases used to store information about players teams and organizations."
    spec$info$contact <- list(name = "Canadice", url = "https://forum.simulationsoccer.com")
    
    spec
  })

players <- pr("./modules/getPlayer.R")
bank <- pr("./modules/getBank.R")
info <- pr("./info.R")
index <- pr("./modules/getIndex.R")


root %>% 
  pr_mount("/player", players) %>% 
  pr_mount("/bank", bank) %>% 
  pr_mount("/info", info) %>% 
  pr_mount("/index", index) %>% 
  pr_run(port = 8001)
