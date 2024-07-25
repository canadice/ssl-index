#### FUNCTION THAT READS FROM API URL ####
readAPI <- function (url, ...) {
  require(dplyr)
  temp <- url %>% httr::GET(...)
  temp$content %>% rawToChar() %>% jsonlite::fromJSON() %>% 
    return()
}