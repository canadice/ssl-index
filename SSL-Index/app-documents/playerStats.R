
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SHL                 ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
playerStatsUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 2,
          offset = 1,
          DTOutput(outputId = ns("playerStats"))
        )
      )
    )
 )
}

## Backend module for player similarities
playerStatsSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
     
      url <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/sttas.html"
      
      playerStats <- 
        url %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table()
      
      output$playerStats <- renderDT({
        playerStats[[1]]
      })
       
    }
  )
}

